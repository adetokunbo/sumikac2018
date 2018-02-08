{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
module Data.Sumikac.Conduit
  (
  -- * Yaml parsing conduits
    convert1File
  , convertFilesInDir
  , decodeYamlProducts
  , decodeYamlStream
  , decodeYamlStreamEither

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
import           Data.Yaml                    (ParseException (..), decode,
                                               decodeEither')
import           System.FilePath

import           Data.Sumikac.Types

-- | Convert the product YAML files in a source directory into output files in
-- another
convertFilesInDir
  :: (MonadIO m, MonadResource m)
  => FilePath -- ^ the source directory
  -> FilePath -- ^ the output directory
  -> ConduitM i o m ()
convertFilesInDir srcDir dstDir = CF.sourceDirectory srcDir .| awaitForever go
  where
    go f = do
      liftIO $ do
        putStrLn ""
        putStrLn $ "Processing " ++ f
      convert1File dstDir $ CC.sourceFile f

-- | Converts a product YAML file to an output in a target directory
convert1File
  :: (MonadIO m, MonadThrow m)
  => FilePath                   -- ^ the target directory
  -> ConduitM i ByteString m () -- ^ the conversion pipeline
  -> ConduitM i o m ()
convert1File dstDir source = source .| go
  where
    go = combine decodeYamlProducts handleProducts dumpParseException
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

-- | Specializes 'decodeYamlStream' to yield 'Product'
decodeYamlProducts
  :: (Monad m, MonadThrow m)
  => ConduitM ByteString (Either ParseException Product) m ()
decodeYamlProducts = decodeYamlStreamEither

-- | Splits the upstream 'ByteString' into objects decoded from Yaml documents
-- | using 'Either' 'ParseException' to allow exception handling
decodeYamlStreamEither
  :: (Monad m, MonadThrow m, FromJSON a)
  => ConduitM ByteString (Either ParseException a) m ()
decodeYamlStreamEither = yamlDocStream .| CC.map decodeEither'

-- | Splits the upstream 'ByteString' into objects decoded from Yaml documnets
decodeYamlStream
  :: (Monad m, MonadThrow m, FromJSON a)
  => ConduitM ByteString (Maybe a) m ()
decodeYamlStream = yamlDocStream .| CC.map decode

-- | Splits the upstream 'ByteString' into Yaml documents
yamlDocStream
  :: (Monad m, MonadThrow m)
  => ConduitM ByteString ByteString m ()
yamlDocStream =
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
