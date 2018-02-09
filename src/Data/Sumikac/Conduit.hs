{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
module Data.Sumikac.Conduit
  (
  -- * Yaml Parsing functions
    convert1File
  , convertFilesInDir
  , pipeDecoded
  , pipeEitherDecoded
  , mkProductPipe

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
                                               decodeEither')
import           System.Directory             (createDirectoryIfMissing,
                                               doesFileExist)
import           System.FilePath

import           Data.Sumikac.Types

-- | To generate from the combined data
-- save the products
-- save the en descriptions
-- scan the product directory, report on files that do not have product description pair
-- forward pairs use the combined data-type to the html generator pipeline

-- | Creates a 'ConvertPipeline' for 'LitDesc'
mkLitDescPipe
  :: (MonadThrow m, MonadIO m)
  => ConvertPipeline LitDesc o m
mkLitDescPipe = ConvertPipeline
  { cpParse = pipeLitDesc
  , cpGo = summarize .| toStdout
  , cpError = dumpParseException
  }
  where
    showBoth (name, content) = Text.pack name <> "\n" <> content
    summarize = CC.map summarizeLD .| CC.map showBoth
    toStdout = CC.map Text.unpack .| CC.unlines .| CC.map pack .| CC.stdout

-- | Specializes 'pipeEitherDecoded' for 'LitDesc'
pipeLitDesc
  :: (Monad m, MonadThrow m)
  => ConduitM ByteString (Either ParseException LitDesc) m ()
pipeLitDesc = pipeEitherDecoded

-- | Creates a 'ConvertPipeline' for 'LiteralDescription'
mkLiteralDescriptionPipe
  :: (MonadThrow m, MonadIO m)
  => FilePath -- ^ the destination directory
  -> ConvertPipeline LiteralDescription o m
mkLiteralDescriptionPipe d = ConvertPipeline
  { cpParse = pipeLiteralDescriptions
  , cpGo = yieldFileAndProductName d .| CC.map fst .| CC.unlines .| CC.map pack .| CC.stdout
  , cpError = dumpParseException
  }

-- | Specializes 'pipeEitherDecoded' for 'LitDesc'
pipeLiteralDescriptions
  :: (Monad m, MonadThrow m)
  => ConduitM ByteString (Either ParseException LiteralDescription) m ()
pipeLiteralDescriptions = pipeEitherDecoded

-- | Yields the filename and Yaml-encoded output of a 'Product'
yieldFileAndProductName
  :: (Monad m)
  => FilePath
  -> ConduitM LiteralDescription (FilePath, Text) m ()
yieldFileAndProductName dstDir = CC.map $ fileAndProductName dstDir

-- | Creates a 'ConvertPipeline' for 'Product'
mkProductPipe
  :: (MonadThrow m, MonadIO m, MonadResource m)
  => FilePath -- ^ the destination directory
  -> ConvertPipeline (ByteString, Product) o m
mkProductPipe d = ConvertPipeline
  { cpParse = pipeProductsKeep
  , cpGo = findPath d .| save
  , cpError = dumpParseException
  }
  where
    findPath
      :: (Monad m)
      => FilePath
      -> ConduitM (ByteString, Product) (FilePath, ByteString) m ()
    findPath dstDir = CC.map go where
      go (content, p) = (dstDir </> productBasename p, content)

    save :: (Monad m, MonadResource m) => ConduitM (FilePath, ByteString) o m ()
    save = awaitForever $ \(path, content) -> do
      liftIO $ createDirectoryIfMissing True $ takeDirectory path
      yield content .| CC.sinkFile path

-- | Specializes 'pipeEitherDecodedKeep' for 'Product'
pipeProductsKeep
  :: (Monad m, MonadThrow m)
  => ConduitM ByteString (Either ParseException (ByteString, Product)) m ()
pipeProductsKeep = pipeEitherDecodedKeep

-- | Process the target YAML files in a srcDir into outputs in dstDir
--
-- E.g, to process all the product info files in srcDir
-- @
--   runConduitRes $ convertFilesInDir srcDir dstDir $ mkProductPipe dstDir
-- @
convertFilesInDir
  :: (MonadIO m, MonadResource m)
  => FilePath -- ^ the source directory
  -> FilePath -- ^ the output directory
  -> ConvertPipeline j o m
  -> ConduitM i o m ()
convertFilesInDir srcDir dstDir pipe =
  CF.sourceDirectory srcDir
  .| CC.filterM (liftIO . doesFileExist)
  .| awaitForever go
  where
    go f = do
      liftIO $ putStrLn "" >> (putStrLn $ "Processing " ++ f)
      convert1File dstDir (CC.sourceFile f) pipe

-- | Processes a target yaml file using a pipeline that places output in dstDir
--
-- E.g, to process a single file product file
-- @
--   runConduitRes $ convert1File dstDir source $ mkProductPipe dstDir
-- @
convert1File
  :: (MonadIO m, MonadThrow m)
  => FilePath                   -- ^ the target directory
  -> ConduitM i ByteString m () -- ^ a producer of the contents of yaml files
  -> ConvertPipeline j o m      -- ^ a conversion pipeline
  -> ConduitM i o m ()
convert1File dstDir source pipe = source .| go
  where
    go = getZipConduit $ ZipConduit goP' <* ZipConduit goE'
    goE' = cpParse pipe .| CC.concatMap left .| cpError pipe
    goP' = cpParse pipe .| CC.concatMap right .| cpGo pipe
    left = either Just (const Nothing)
    right = either (const Nothing) Just

-- | ConvertPipeline describes the conduits used by convert1File' to process a
-- specific type of YAML file
data ConvertPipeline a o m  = ConvertPipeline
  { cpParse :: ConduitM ByteString (Either ParseException a) m ()
  , cpGo    :: ConduitM a o m ()
  , cpError :: ConduitM ParseException o m ()
  }

-- | Dumps a ParseError to stderr
dumpParseException
  :: (MonadIO m)
  => ConduitM ParseException o m ()
dumpParseException = CC.map show .| CC.unlines .| CC.map pack .| CC.stderr

-- | Splits the upstream 'ByteString' into objects decoded from Yaml documents
-- | using 'Either' 'ParseException' to allow exception handling
pipeEitherDecoded
  :: (Monad m, MonadThrow m, FromJSON a)
  => ConduitM ByteString (Either ParseException a) m ()
pipeEitherDecoded = pipeYamlDocs .| CC.map decodeEither'

-- | Splits the upstream 'ByteString' into objects decoded from Yaml documents
-- | using 'Either' 'ParseException' to allow exception handling, and keeps
-- | the input in Tuple with the output
pipeEitherDecodedKeep
  :: (Monad m, MonadThrow m, FromJSON a)
  => ConduitM ByteString (Either ParseException (ByteString, a)) m ()
pipeEitherDecodedKeep = pipeYamlDocs .| CC.map decodeAndKeep
  where
    decodeAndKeep x = ((,) x) <$> decodeEither' x

-- | Splits the upstream 'ByteString' into objects decoded from Yaml documnets
pipeDecoded
  :: (Monad m, MonadThrow m, FromJSON a)
  => ConduitM ByteString (Maybe a) m ()
pipeDecoded = pipeYamlDocs .| CC.map decode

-- | Splits the upstream 'ByteString' into Yaml documents
pipeYamlDocs
  :: (Monad m, MonadThrow m)
  => ConduitM ByteString ByteString m ()
pipeYamlDocs =
  CT.decode CT.utf8
  .| byDashes
  .| CT.encode CT.utf8
  .| (CC.filter $ not . BS.null)
  where byDashes = groupBySep $ \x -> x == "---"

-- | Splits the upstream 'Text' into chunks bounded by
-- | separator lines that match the predicate
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
