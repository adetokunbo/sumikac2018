{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
module Data.Sumikac.Conduit
  (
  -- * Yaml parsing conduits
    decodeYamlProducts
  , decodeYamlStream
  , decodeYamlStreamEither

  -- * Useful functions
  , groupByDashes
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
import           Data.Yaml                    (ParseException (..), decode,
                                               decodeEither')
import           System.FilePath

import           Data.Sumikac.Types

processProductFiles
  :: (MonadIO m, MonadResource m)
  => FilePath
  -> FilePath
  -> ConduitM i o m ()
processProductFiles srcDir dstDir =
  CF.sourceDirectory srcDir
  .| awaitForever (\f -> handleProductFile dstDir $ CC.sourceFile f)

handleProductFile
  :: (MonadIO m, MonadThrow m)
  => FilePath
  -> ConduitM i ByteString m ()
  -> ConduitM i o m ()
handleProductFile dstDir source = source .| handler
  where
    handler = combine decodeYamlProducts handleProducts dumpParseException
    handleProducts = yieldNameAndContent dstDir .| tmpDumpName
    combine parsed goP goE =  getZipConduit $ ZipConduit goP' <* ZipConduit goE'
      where
        goE' = parsed .| CC.concatMap left .| goE
        goP' = parsed .| CC.concatMap right .| goP
        left = either Just (const Nothing)
        right = either (const Nothing) Just

-- | Prints the name to stdout
tmpDumpName
  :: (MonadIO m)
  => ConduitM (FilePath, ByteString) o m ()
tmpDumpName = CC.map fst .| CC.unlines .| CC.map pack .| CC.stdout

-- | Yields the filename and Yaml-encoded output of a 'Product'
yieldNameAndContent
  :: (Monad m)
  => FilePath
  -> ConduitM Product (FilePath, ByteString) m ()
yieldNameAndContent dstDir = CC.map $ fileNameWithContent dstDir

-- | Dumps a ParseError to stderr
dumpParseException
  :: (MonadIO m)
  => ConduitM ParseException o m ()
dumpParseException = CC.map show .| CC.unlines .| CC.map pack .| CC.stderr

-- | Specializes 'decodeYamlStream' to a stream of Products from Yaml
decodeYamlProducts
  :: (Monad m, MonadThrow m)
  => ConduitM ByteString (Either ParseException Product) m ()
decodeYamlProducts = decodeYamlStreamEither

-- | Splits the upstream 'ByteString' into Yaml documents
-- | that yield `Left ParseException` on failures
decodeYamlStreamEither
  :: (Monad m, MonadThrow m, FromJSON a)
  => ConduitM ByteString (Either ParseException a) m ()
decodeYamlStreamEither = yamlDocStream .| CC.map decodeEither'

-- | Splits the upstream 'ByteString' into Yaml documents
decodeYamlStream
  :: (Monad m, MonadThrow m, FromJSON a)
  => ConduitM ByteString (Maybe a) m ()
decodeYamlStream = yamlDocStream .| CC.map decode

-- | Splits the upstream 'ByteString' into Yaml documents
yamlDocStream
  :: (Monad m, MonadThrow m)
  => ConduitM ByteString ByteString m ()
yamlDocStream = CT.decode CT.utf8 .| groupByDashes .| CT.encode CT.utf8 .| (CC.filter $ not . BS.null)

-- | Splits the upstream 'Text' into chunks bounded by
-- | separator lines containing "---"
groupByDashes :: Monad m => ConduitM Text Text m ()
groupByDashes = groupBySep $ \x -> x == "---"

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

yieldFromReturn
  :: Monad m
  => (r -> o)
  -> ConduitM i o m r
  -> ConduitM i o m ()
yieldFromReturn f inner = do
  result <- inner
  CL.sinkNull
  yield $ f result

unlinesText :: Monad m => Conduit Text m Text
unlinesText = awaitForever $ \x -> yield x >> yield "\n"
