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
    KnownDirs(..)
  , ConvertPipeline(..)
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
import qualified Data.Map.Strict                     as Map
import           Data.Monoid                         ((<>))
import           Data.Text                           (Text)
import qualified Data.Text                           as Text
import qualified Data.Text.Encoding                  as Text
import           System.Directory
import           System.FilePath

import           Data.Aeson                          (FromJSON (..))
import           Data.Yaml
import           Lens.Micro.Platform
import           Text.Mustache

import           Data.Conduit
import qualified Data.Conduit.Combinators            as CC
import qualified Data.Conduit.Filesystem             as CF
import qualified Data.Conduit.List                   as CL
import qualified Data.Conduit.Text                   as CT

import           Sumikac.Types
import           Sumikac.Types.Rendered.CategoryPage
import           Sumikac.Types.Rendered.ProductPage

import           Paths_sumikac2018

-- | Identifies the various directories used during the site refresh
data KnownDirs = KnownDirs
  { downloads :: FilePath -- ^ the directory where downloaded files are saved
  , working   :: FilePath -- ^ the working directory where intermediate results are kept
  , site      :: FilePath -- ^ the site directory containing the generated output
  }

-- Env is an environment available to all processing pipelines.
--
-- Initially it just contains the currencies.
data Env = Env
  { productEnv      :: FullProductEnv
  , knownCategories :: NonEmpty Text
  , template        :: Template
  , dirs            :: KnownDirs
  }

-- | Load files that provide static configuration data.
loadEnv
  :: (MonadIO m)
  => KnownDirs
  -> m (Either ParseException Env)
loadEnv dirs = liftIO $ do
    let KnownDirs {downloads} = dirs
        ratesPath = downloads </> "latest_rates.yaml"
        categoriesPath = downloads </> "site_categories.yaml"
        currenciesPath = downloads </> "site_currencies.yaml"
        deliveryCostPath = downloads </> "ems_delivery_costs.yaml"
    fromUSD <- decodeFileEither ratesPath
    categories' <- decodeFileEither categoriesPath
    currencies <- decodeFileEither currenciesPath
    templateDir <- (flip (</>) "data/mustache") <$> getDataDir
    putStrLn $ "Loading templates in " ++ templateDir
    template <- compileMustacheDir "product_layout.html" templateDir
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
        , template
        , dirs
        }

-- | Run the pipes that regenerate the site.
runAll
  :: (MonadIO m, MonadThrow m, MonadBaseControl IO m)
  => KnownDirs -- ^ configures locations used by the pipeline
  -> m ()
runAll dirs = do
  let KnownDirs {downloads, working} = dirs
      prodDir = downloads </> "Products"
      descDir = prodDir </> "Description/English"

  env <- loadEnv dirs
  case env of
    Left e -> throwM e -- could not load files in the environment
    Right env' -> do
      runConduitRes $ convertFilesIn descDir $ mkLitDescPipe working
      let withEnv = flip runReaderT env' . runConduitRes
      withEnv $ convertFilesIn prodDir $ mkProductPipe working
      withEnv generateSiteHtml

-- | Generate the site html pages
generateSiteHtml
  :: (MonadIO m, MonadResource m, MonadReader Env m)
  => ConduitM i o m ()
generateSiteHtml = do
  Env { template, dirs, knownCategories } <- ask
  let KnownDirs { site, working } = dirs
      template' = template {templateActual = "category_layout.html"}
      generate cats = awaitForever $ \page -> do
        let cat = page ^. categoryId
            layout = mkCategoryLayoutPage (NonEmpty.fromList cats) page
            dst = site </> "categories" </> (Text.unpack cat) <> ".html"
            bytez  = Text.encodeUtf8 $ renderCategory template' layout
        liftIO $ putStrLn $ "... creating " ++ dst
        yield (dst, bytez)

  pages <- CC.yieldMany knownCategories
           .| awaitForever (yieldCategoryPage working)
           .| CC.sinkList
  let availableCats = map (^. categoryId) pages
  CC.yieldMany pages
    .| generate availableCats
    .| save
    .| CC.sinkNull
  generateProductHtml $ NonEmpty.fromList availableCats

-- | Generate the product HTML pages from the 'FullProduct' in the working dir.
generateProductHtml
  :: (MonadIO m, MonadResource m, MonadReader Env m)
  => NonEmpty Text    -- ^ available categories
  -> ConduitM i o m ()
generateProductHtml cats = do
  Env { dirs } <- ask
  let KnownDirs { working } = dirs
      filter' = CC.filter (isSuffixOf "-complete.yaml")
      decode' = CC.map decodeEither'
      sourceFile' = awaitForever $ \f -> CC.sourceFile f

  CF.sourceDirectory working
    .| filter'
    .| sourceFile'
    .| handleEither dumpParseException decode' generate
    .| save
    .| CC.sinkNull
  where
    generate = awaitForever $ \fp -> do
      Env { dirs, productEnv, template } <- ask
      let FullProductEnv { _fpeRates} = productEnv
          KnownDirs { site } = dirs
          currs = NonEmpty.fromList $ Map.keys _fpeRates
      case (mkProductLayoutPage cats currs fp) of
        Left err -> do
          let msg = "Failed to generate product HTML page: " ++ show err
          liftIO $ putStrLn msg
        Right page -> do
          let dst = site </> "products" </> "named" </> productPageBasename page
              bytez = Text.encodeUtf8 $ renderProduct template page
          liftIO $ putStrLn $ "... creating " ++ dst
          yield (dst, bytez)

-- | A pipeline that yields 'CategoryPage' for each product.
--
-- It scans the src directory for product yaml files, and yields
-- a CategoryPage for each product that is in the specified category
yieldCategoryPage
  :: (MonadIO m, MonadResource m)
  => FilePath  -- ^ target directory
  -> Text      -- ^ category name
  -> ConduitM i CategoryPage m ()
yieldCategoryPage src cat =
  CF.sourceDirectory src
  .| filter'
  .| sourceFile'
  .| handleEither dumpParseException decode' mkPage
  where
    filter' = CC.filter (isSuffixOf "-complete.yaml")
    decode' = CC.map decodeEither'
    sourceFile' = awaitForever $ \f ->  CC.sourceFile f

    mkPage = do
      accum <- (CL.mapMaybe $ categoryProduct cat) .| CC.sinkList
      case (length accum) of
        0 -> liftIO  $ putStrLn $ "No products found for: " ++ Text.unpack cat
        _ -> yield $ mkCategoryPage cat numColumns accum

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
convertFilesIn srcDir pipeline =
  CF.sourceDirectory srcDir
  .| CC.filterM (liftIO . doesFileExist) -- filter out directories
  .| sourceFile'
  .| handleEither cpError cpParse cpGo
  where
    ConvertPipeline{cpParse, cpGo, cpError} = pipeline
    sourceFile' = awaitForever $ \f -> do
      liftIO $ putStrLn "" >> (putStrLn $ "Processing " ++ f)
      CC.sourceFile f

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
dumpParseException = CC.map prettyPrintParseException .| CC.unlines .| CC.map BS.pack .| CC.stdout

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
  .| CC.map (flip (<>) "\n---\n") -- ^ Ensure trailing separator
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
