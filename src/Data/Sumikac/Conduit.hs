{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-|
Module      : Data.Sumikac.Conduit
Description : Conduit-based pipelines that generate the SumikaCrafts website.
Copyright   : (c) Tim Emiola, 2018
License     : None
Maintainer  : sam@sumikacrafts.com
Stability   : experimental
-}
module Data.Sumikac.Conduit
  (
  -- * Yaml Parsing
    ConvertPipeline(..)
  , convert1File
  , convertFilesIn
  , pipeDecoded
  , pipeEitherDecoded
  , mkProductPipe
  , mkLitDescPipe

  -- * Useful functions
  , groupBySep
  )
where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Aeson                   (FromJSON (..))
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import           Data.ByteString.Char8        (pack)
import           Data.Conduit
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.Combinators     as CC
import qualified Data.Conduit.Filesystem      as CF
import qualified Data.Conduit.List            as CL
import qualified Data.Conduit.Text            as CT
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Yaml                    (ParseException (..), decode,
                                               decodeEither', encode)
import           System.Directory             (createDirectoryIfMissing,
                                               doesFileExist)
import           System.FilePath

import           Data.Sumikac.Types


-- | Run the pipes that regenerate the site
runAll
  :: FilePath -- ^ the source directory when the files are download to
  -> FilePath -- ^ the destination directory to where the files are saved
  -> IO ()
runAll src dst = do
  let descDir = src </> "Description/English"
  runConduitRes $ convertFilesIn descDir $ mkLitDescPipe dst
  runConduitRes $ convertFilesIn src $ mkProductPipe dst

-- | Process the target YAML files in a srcDir into outputs in dstDir
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
  .| CC.filterM (liftIO . doesFileExist)
  .| awaitForever go
  where
    go f = do
      liftIO $ putStrLn "" >> (putStrLn $ "Processing " ++ f)
      convert1File (CC.sourceFile f) pipe

-- | Processes a target yaml file using a pipeline that places output in dstDir
--
-- E.g, to process a single file product file
-- @
--   runConduitRes $ convert1File dstDir source $ mkProductPipe dstDir
-- @
convert1File
  :: (MonadIO m, MonadThrow m)
  => ConduitM i FileContents m () -- ^ a producer of the contents of yaml files
  -> ConvertPipeline j o m -- ^ a pipeline that converts YamlDocs into j
  -> ConduitM i o m ()
convert1File source ConvertPipeline{..} =
  source .| handleEither cpError cpParse cpGo

-- FileContents are the entire contents for reading a file
type FileContents = ByteString

-- YamlDoc is a Yaml Document; there may be several of these in a file
type YamlDoc = ByteString

-- | Creates a 'ConvertPipeline' for 'LitDesc'
mkLitDescPipe
  :: (MonadThrow m, MonadIO m, MonadResource m)
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

-- | Creates a 'ConvertPipeline' for 'Product'
mkProductPipe
  :: (MonadThrow m, MonadIO m, MonadResource m)
  => FilePath -- ^ the destination directory
  -> ConvertPipeline (YamlDoc, Product) o m
mkProductPipe d = ConvertPipeline
  { cpParse = pipeEitherDecodedKeep
  , cpGo = CC.map pPath .| handleFullProduct .| save .| CC.map fst .| CC.print
  , cpError = dumpParseException
  }
  where
    pPath (content, p) = (d </> productBasename p, p)
    fpPath fp = (d </> fullProductBasename fp, encode fp)
    handleFullProduct = withDumpParseException pipeToFullProduct $ CC.map fpPath

-- | Determine the path of the descriptor Yaml given the path of the product
-- Yaml
toDescPath :: FilePath -> FilePath
toDescPath f =
  let ext = takeExtension f
  in addExtension (dropExtension f ++ "-descs") ext

pipeToFullProduct
  :: (MonadThrow m, MonadIO m, MonadResource m)
  => ConduitM (FilePath, Product) (Either ParseException FullProduct) m ()
pipeToFullProduct =
  awaitForever $ \(path, p) -> do
    let decodePlus (p, content) = FullProduct p <$> decodeEither' content
        descPath = toDescPath path
        wasMissing = Left NonScalarKey
    -- TODO: add real error handling
    exists <- (liftIO . doesFileExist) descPath
    if exists then CC.sourceFile (descPath)
                   .| CC.map ((,) p)
                   .| CC.map decodePlus
              else yield wasMissing

-- | Save the content to the indicated path
--
-- This is not sink - the inputs are yielded so that further downstream
-- processing is allowed
save
  :: (Monad m, MonadResource m)
  => ConduitM (FilePath, FileContents) (FilePath, FileContents) m ()
save = awaitForever $ \(path, content) -> do
  liftIO $ createDirectoryIfMissing True $ takeDirectory path
  yield content .| CC.sinkFile path
  yield (path, content)

-- | Generalize handling errors for Conduits that produce Either
handleEither
  :: Monad m
  => ConduitM b1 c m r1
  -> ConduitM a (Either b1 b2) m ()
  -> ConduitM b2 c m r2
  -> ConduitM a c m r2
handleEither error parser go = go'
  where
    go' = getZipConduit $ ZipConduit goP' <* ZipConduit goE'
    goE' = parser .| CC.concatMap left .| error
    goP' = parser .| CC.concatMap right .| go
    left = either Just (const Nothing)
    right = either (const Nothing) Just

-- | ConvertPipeline describes conduits used by convert1File' to
-- process a specific type of YAML file
data ConvertPipeline a o m = ConvertPipeline
  { cpParse :: ConduitM FileContents (Either ParseException a) m ()
  , cpGo    :: ConduitM a o m ()
  , cpError :: ConduitM ParseException o m ()
  }

-- | Writes the 'ParseException' to stderr
dumpParseException
  :: (MonadIO m)
  => ConduitM ParseException o m ()
dumpParseException = CC.map show .| CC.unlines .| CC.map pack .| CC.stderr

withDumpParseException
  :: (MonadThrow m, MonadIO m, MonadResource m)
  => ConduitM i (Either ParseException a) m ()
  -> ConduitM a o m r
  -> ConduitM i o m r
withDumpParseException = handleEither dumpParseException

-- | Splits the upstream 'FileContents' into a stream of decoded objects
pipeDecoded
  :: (Monad m, MonadThrow m, FromJSON a)
  => ConduitM FileContents (Maybe a) m ()
pipeDecoded = pipeYamlDocs .| CC.map decode

-- | Creates a stream of decoded Yaml objects wrapped in 'Either' 'ParseException'
-- to allow for exception handling
pipeEitherDecoded
  :: (Monad m, MonadThrow m, FromJSON a)
  => ConduitM FileContents (Either ParseException a) m ()
pipeEitherDecoded = pipeYamlDocs .| CC.map decodeEither'

-- | Creates a stream of decoded Yaml objects along with the 'YamlDoc' they were
-- parsed from. These are wrapped in 'Either' 'ParseException' to allow for
-- exception handling
pipeEitherDecodedKeep
  :: (Monad m, MonadThrow m, FromJSON a)
  => ConduitM FileContents (Either ParseException (YamlDoc, a)) m ()
pipeEitherDecodedKeep = pipeYamlDocs .| CC.map decodeAndKeep
  where
    decodeAndKeep x = ((,) x) <$> decodeEither' x

-- | Creates a stream of 'YamlDoc' from the  upstream 'FileContents'
pipeYamlDocs
  :: (Monad m, MonadThrow m)
  => ConduitM FileContents YamlDoc m ()
pipeYamlDocs =
  CT.decode CT.utf8
  .| byDashes
  .| CT.encode CT.utf8
  .| (CC.filter $ not . BS.null)
  where
    byDashes = groupBySep $ \x -> x == "---"

-- | Splits the upstream 'Text' into chunks bounded by separator lines that
-- match the predicate
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
        otherwise -> return acc'

yieldFromJust
  :: Monad m
  => ConduitM i o m (Maybe o)
  -> ConduitM i o m ()
yieldFromJust inner = do
  lastChunk <- inner
  CL.sinkNull
  maybe (return ()) (\chunk -> yield chunk >> return ()) lastChunk
