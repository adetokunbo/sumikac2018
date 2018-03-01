{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-|
Module      : Sumikac.Conduit
Description : Conduit-based pipelines that generate the SumikaCrafts website.
Copyright   : (c) Tim Emiola, 2018
License     : None
Maintainer  : sam@sumikacrafts.com
Stability   : experimental
-}
module Sumikac.Conduit
  (
  -- * Yaml Parsing
    ConvertPipeline(..)
  , mkProductPipe
  , mkLitDescPipe
  , convertFilesIn
  , runAll

  -- * Useful functions
  , pipeDecoded
  , pipeEitherDecoded
  , groupBySep
  , handleEither
  )
where

import qualified Control.Exception                   as Exc
import           Control.Monad                       (join)
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource

import           Data.ByteString                     (ByteString)
import qualified Data.ByteString.Char8               as BS
import           Data.List                           (isSuffixOf)
import           Data.List.NonEmpty                  (NonEmpty)
import qualified Data.List.NonEmpty                  as NonEmpty
import qualified Data.Map.Strict                as Map
import           Data.Monoid                         ((<>))
import           Data.Text                           (Text)
import qualified Data.Text                           as Text
import           System.Directory
import           System.FilePath

import           Data.Aeson                          (FromJSON (..))
import           Data.Yaml                           (ParseException (..),
                                                      decode, decodeEither',
                                                      decodeFileEither, encode)
import           Lens.Micro.Platform

import           Data.Conduit
import qualified Data.Conduit.Combinators            as CC
import qualified Data.Conduit.Filesystem             as CF
import qualified Data.Conduit.List                   as CL
import qualified Data.Conduit.Text                   as CT

import           Sumikac.Types
import           Sumikac.Types.Rendered.CategoryPage
import           Sumikac.Types.Rendered.ProductPage

-- Env is an environment available to all processing pipelines.
--
-- Initially it just contains the currencies.
data Env = Env
  { productEnv      :: FullProductEnv
  , knownCategories :: NonEmpty Text
  }

-- | Load files that provide static configuration data.
loadEnv
  :: (MonadIO m)
  => FilePath
  -> m (Either ParseException Env)
loadEnv src = liftIO $ do
    let ratesPath = src </> "latest_rates.yaml"
        categoriesPath = src </> "site_categories.yaml"
        currenciesPath = src </> "site_currencies.yaml"
        deliveryCostPath = src </> "ems_delivery_costs.yaml"
    fromUSD <- decodeFileEither ratesPath
    categories' <- decodeFileEither categoriesPath
    currencies <- decodeFileEither currenciesPath
    emsRates <- decodeFileEither deliveryCostPath
    return $ do
      knownCategories <- categories'
      currencies' <- currencies
      fromUSD' <- fromUSD
      emsRates' <- emsRates
      fromYen <- mkYenRates currencies' fromUSD'
      return $ Env
        { productEnv = FullProductEnv fromYen emsRates'
        , knownCategories
        }

-- | Run the pipes that regenerate the site.
runAll
  :: (MonadIO m, MonadThrow m, MonadBaseControl IO m)
  => FilePath -- ^ the source directory containing the downloaded files
  -> FilePath -- ^ the destination directory to where the files are saved
  -> m ()
runAll src dst = do
  let prodDir = src </> "Products"
      descDir = prodDir </> "Description/English"

  env <- loadEnv src
  case env of
    Left e -> throwM e -- could not load files in the environment
    Right env' -> do
      let withEnv = flip runReaderT env' . runConduitRes
      runConduitRes $ convertFilesIn descDir $ mkLitDescPipe dst
      withEnv $ convertFilesIn prodDir $ mkProductPipe dst
      withEnv $ collectCategories dst $ knownCategories env'

collectCategories
  :: (MonadIO m, MonadResource m, MonadReader Env m)
  => FilePath -> NonEmpty Text -> ConduitM i o m ()
collectCategories src cats = do
  let dst c = src </> (Text.unpack c) <> "-category.yaml"
  pages <- CC.yieldMany cats
           .| awaitForever (yieldCategoryPage src)
           .| CC.sinkList
  let availableCats = map (^. categoryId) pages
      mkLayout = mkCategoryLayoutPage $ NonEmpty.fromList availableCats
      layouts = zip (map dst availableCats) $ map (encode . mkLayout) pages
  CC.yieldMany layouts
    .| save
    .| CC.sinkNull
  generateProductPages src $ NonEmpty.fromList availableCats

-- | Read the FullProducts in the target dir, and generate product pages from
-- them
generateProductPages
  :: (MonadIO m, MonadResource m, MonadReader Env m)
  => FilePath         -- ^ target directory
  -> NonEmpty Text    -- ^ available categories
  -> ConduitM i o m ()
generateProductPages src cats =
  CF.sourceDirectory src
  .| filter'
  .| sourceFile'
  .| handleEither dumpParseException decode' generate
  where
    filter' = CC.filter (isSuffixOf "-complete.yaml")
    decode' = CC.map decodeEither'
    sourceFile' = awaitForever $ \f -> do
      let msg = "Generating product page from: " <> Text.pack f
      liftIO $ print msg
      CC.sourceFile f

    generate = awaitForever $ \fp -> do
      Env { productEnv } <- ask
      let FullProductEnv { _fpeRates} = productEnv
          currs = NonEmpty.fromList $ Map.keys _fpeRates
      case (mkProductLayoutPage cats currs fp) of
        Left err -> do
          let msg = "Failed to get product page yaml from " ++ show err
          liftIO $ putStrLn msg
        Right page -> do
          let dst = src </> productLayoutBasename page
          yield (encode page) .| CC.sinkFile dst

-- | A pipeline that yields the model of a CategoryPage.
--
-- It scans *-complete.yaml in the given directory as FullProduct yaml files
--   If the product is the given category, includes the product CategoryPage
--   it yields
yieldCategoryPage
  :: (MonadIO m, MonadResource m)
  => FilePath  -- ^ target directory
  -> Text      -- ^ category name
  -> ConduitM i CategoryPage m ()
yieldCategoryPage src c =
  CF.sourceDirectory src
  .| filter'
  .| sourceFile'
  .| handleEither dumpParseException decode' collect
  where
    filter' = CC.filter (isSuffixOf "-complete.yaml")
    decode' = CC.map decodeEither'
    sourceFile' = awaitForever $ \f -> do
      let msg = "Scanning for products in category: "
            <> c <> " in " <> Text.pack f
      liftIO $ print msg
      CC.sourceFile f

    collect = do
      accum <- (CL.mapMaybe $ categoryProduct c) .| CC.sinkList
      case (length accum) of
        0 -> liftIO  $ putStrLn $ "No products found for: " ++ Text.unpack c
        _ -> yield $ mkCategoryPage c numColumns accum


-- | Process the target YAML files in a srcDir into outputs in dstDir.
--
-- E.g, to process all the product info files in srcDir
-- @
--   runConduitRes $ convertFilesIn srcDir dstDir $ mkProductPipe dstDir
-- @
convertFilesIn
  :: (MonadIO m, MonadResource m)
  => FilePath -- ^ the source directory
  -> ConvertPipeline j o m
  -> ConduitM i o m ()
convertFilesIn srcDir pipe =
  CF.sourceDirectory srcDir
  .| CC.filterM (liftIO . doesFileExist) -- filter out directories
  .| awaitForever go
  where
    go f = do
      let ConvertPipeline{cpParse, cpGo, cpError} = pipe
      liftIO $ putStrLn "" >> (putStrLn $ "Processing " ++ f)
      CC.sourceFile f .| handleEither cpError cpParse cpGo

-- FileContents are the entire contents for reading a file
type FileContents = ByteString

-- YamlDoc is a Yaml Document; there may be several of these in a file
type YamlDoc = ByteString

-- | Creates a 'ConvertPipeline' for 'LitDesc'.
mkLitDescPipe
  :: (MonadThrow m, MonadResource m)
  => FilePath -- ^ the destination directory
  -> ConvertPipeline LitDesc o m
mkLitDescPipe d = ConvertPipeline
  { cpParse = pipeEitherDecoded
  , cpGo = fullDescs .| CC.map pathOutput .| save .| CC.map fst .| CC.print
  , cpError = dumpParseException
  }
  where
    fullDescs = do
      accum <- CC.foldl addLitDesc descAccum
      CC.yieldMany $ asFullDescs accum

    pathOutput fd = (d </> fullDescBasename fd, encode fd)

-- | Creates a 'ConvertPipeline' for 'Product'.
mkProductPipe
  :: (MonadThrow m, MonadReader Env m, MonadResource m, MonadBaseControl IO m)
  => FilePath -- ^ the destination directory
  -> ConvertPipeline (YamlDoc, Product) o m
mkProductPipe d = ConvertPipeline
  { cpParse = pipeEitherDecodedKeep
  , cpGo = CC.map pPath .| handleFullProduct .| save .| CC.map fst .| CC.print
  , cpError = dumpParseException
  }
  where
    pPath (_, p) = (d </> productBasename p, p)
    fpPath fp = (d </> fullProductBasename fp, encode fp)
    handleFullProduct = withDumpParseException pipeToFullProduct $ CC.map fpPath

-- | Derive a path from another one by replacing the suffix of its basename.
replaceBaseSuffix :: String -> FilePath -> FilePath
replaceBaseSuffix suffix f =
  addExtension (dropExtension f ++ suffix) $ takeExtension f

pipeToFullProduct
  :: (MonadThrow m,
      MonadReader Env m,
      MonadResource m,
      MonadBaseControl IO m)
  => ConduitM (FilePath, Product) (Either ParseException FullProduct) m ()
pipeToFullProduct =
  awaitForever $ \(path, p) -> do
    Env { productEnv } <- ask
    let descPath = replaceBaseSuffix "-descs" path
        imgsPath = replaceBaseSuffix "-web-images" path
        handleOthers e = yield . Left . OtherParseException $ e
        wrapExc = either (Left . OtherParseException . Exc.toException) Right
    handleC handleOthers $ do
      imgs <- liftIO $ decodeFileEither imgsPath
      desc <- liftIO $ decodeFileEither descPath
      yield $ join $ wrapExc <$> (fullProduct p productEnv <$> imgs <*> desc)

  -- | Save the content to the indicated path.
--
-- This is not sink - the inputs are yielded so that further downstream
-- processing is allowed.
save
  :: (MonadResource m)
  => ConduitM (FilePath, FileContents) (FilePath, FileContents) m ()
save = awaitForever $ \(path, bytez) -> do
  liftIO $ createDirectoryIfMissing True $ takeDirectory path
  yield bytez .| CC.sinkFile path
  yield (path, bytez)

-- | Generalize handling errors for Conduits that produce Either.
handleEither
  :: Monad m
  => ConduitM b1 c m r1
  -> ConduitM a (Either b1 b2) m ()
  -> ConduitM b2 c m r2
  -> ConduitM a c m r2
handleEither onExc parser onOK = go'
  where
    go' = getZipConduit $ ZipConduit goP' <* ZipConduit goE'
    goE' = parser .| CC.concatMap left .| onExc
    goP' = parser .| CC.concatMap right .| onOK
    left = either Just (const Nothing)
    right = either (const Nothing) Just

-- | ConvertPipeline configures the conduits used by convert1File' to process a
-- specific type of YAML file.
data ConvertPipeline a o m = ConvertPipeline
  { cpParse :: ConduitM FileContents (Either ParseException a) m ()
  , cpGo    :: ConduitM a o m ()
  , cpError :: ConduitM ParseException o m ()
  }

-- | Writes the 'ParseException' to stderr.
dumpParseException
  :: (MonadIO m)
  => ConduitM ParseException o m ()
dumpParseException = CC.map show .| CC.unlines .| CC.map BS.pack .| CC.stderr

withDumpParseException
  :: (MonadThrow m, MonadResource m)
  => ConduitM i (Either ParseException a) m ()
  -> ConduitM a o m r
  -> ConduitM i o m r
withDumpParseException = handleEither dumpParseException

-- | Splits the upstream 'FileContents' into a stream of decoded objects.
pipeDecoded
  :: (MonadThrow m, FromJSON a)
  => ConduitM FileContents (Maybe a) m ()
pipeDecoded = pipeYamlDocs .| CC.map decode

-- | Creates a stream of decoded Yaml objects wrapped in 'Either' 'ParseException'
-- to allow for exception handling.
pipeEitherDecoded
  :: (MonadThrow m, FromJSON a)
  => ConduitM FileContents (Either ParseException a) m ()
pipeEitherDecoded = pipeYamlDocs .| CC.map decodeEither'

-- | Creates a stream of decoded Yaml objects along with the 'YamlDoc' they were
-- parsed from. These are wrapped in 'Either' 'ParseException' to allow for
-- exception handling.
pipeEitherDecodedKeep
  :: (MonadThrow m, FromJSON a)
  => ConduitM FileContents (Either ParseException (YamlDoc, a)) m ()
pipeEitherDecodedKeep = pipeYamlDocs .| CC.map decodeAndKeep
  where
    decodeAndKeep x = ((,) x) <$> decodeEither' x

-- | Creates a stream of 'YamlDoc' from the  upstream 'FileContents'.
pipeYamlDocs
  :: (MonadThrow m)
  => ConduitM FileContents YamlDoc m ()
pipeYamlDocs =
  CT.decode CT.utf8
  .| byDashes
  .| CT.encode CT.utf8
  .| (CC.filter $ not . BS.null)
  where
    byDashes = groupBySep $ \x -> x == "---"

-- | Splits the upstream 'Text' into chunks bounded by separator lines that
-- match the predicate.
groupBySep :: Monad m => (Text -> Bool) -> ConduitM Text Text m ()
groupBySep p = yieldFromJust (CT.foldLines groupBySep' Nothing)
  where
    groupBySep' acc = do
      let scanOnSep :: Maybe Text -> Text -> Maybe Text
          scanOnSep _ t        | p t = Nothing
          scanOnSep Nothing t  = Just t
          scanOnSep (Just x) t = Just (x <> "\n" <> t)

      acc' <- CL.fold scanOnSep acc
      case acc' of
        Nothing -> do
          yield $ maybe "" id acc
          return acc'
        x -> return x

yieldFromJust
  :: Monad m
  => ConduitM i o m (Maybe o)
  -> ConduitM i o m ()
yieldFromJust inner = do
  lastChunk <- inner
  CL.sinkNull
  maybe (return ()) (\chunk -> yield chunk >> return ()) lastChunk
