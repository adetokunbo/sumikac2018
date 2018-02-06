{-# LANGUAGE OverloadedStrings #-}

module Client.Drive
  (
  -- * Download files and/or folders
  downloadAFile
  , downloadFolder
  , downloadFolderToDir

  -- * List the contents of folders
  , listFolder
  , listFolderNames
  , listFolderNamesWithIds
  )

where
---------------------------------------------------------------------------------
import           Network.Google
import           Network.Google.Drive

import           Conduit
import           Control.Lens                 ((.~), (<&>), (^.))
import           Control.Monad                (forM_)
import           Control.Monad.Trans.Resource (liftResourceT, runResourceT)
import           Data.Conduit                 (($$+-))
import           Data.Maybe                   (catMaybes, listToMaybe)

import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as L
import           Formatting                   (Format, format, sformat, (%))
import           Formatting.Formatters        (text)

import           Path.Default                 (createDefaultDirIfMissing)

import           System.Directory             (createDirectoryIfMissing,
                                               getHomeDirectory)
import           System.FilePath              ((</>))
import           System.IO                    (stdout)


-- | Format for a query that searches for folders
folderQueryFmt :: Format r (L.Text -> r)
folderQueryFmt = "name = '" % text % "' and mimeType = 'application/vnd.google-apps.folder'"

-- | Obtain the query for searching for a folder with a given name
mkFolderQuery :: T.Text -> Maybe T.Text
mkFolderQuery name = Just $ sformat folderQueryFmt $ L.fromStrict name

-- | Format for a query that searchs for files belonging to a given parent
childQueryFmt :: Format r (L.Text -> r)
childQueryFmt = "'" % text % "' in parents and mimeType != 'application/vnd.google-apps.folder'"

-- | Obtain a query for finding a Folder's chidren from the FileList returned
-- Obtain the id and name from a filefrom searching for a Folder
mkChildQuery :: FileList -> Maybe T.Text
mkChildQuery fl = do
  f <- listToMaybe $ fl ^. flFiles
    -- ignore all Files except the first, this ok as there should be just one
  theId <- f ^. fId
  return $ L.toStrict $ format childQueryFmt $ L.fromStrict theId

-- | Obtain the namd and id from a file.
--
-- Implementation note: this is the applicative version of the following
--
-- @
-- nameAndId f = do
--   name <-  f ^. fName
--   itsId <- f ^. fId
--   return (name, itsId)
-- @
nameAndId :: File -> Maybe (T.Text, T.Text)
nameAndId f = (,) <$> (f ^. fName) <*> (f ^. fId)

-- | Convert the name attribute of a Drive 'File' to a local basename.
gdoc2base :: T.Text -> FilePath
gdoc2base = T.unpack . T.replace " " "_"

-- * Example functions
--
-- In order to be able to run these examples you need to create a service acount
-- from google's developers console and copy the dowloaded json file to
-- ~/.config/gcloud/application_default_credentials.json.
--
-- You must also share with your service the folder that you want to get the info
-- of. In order to do this you must share the folder with the email address of your
-- service which is in the downloaded service config file.

-- | Lists files in a Drive folder.
--
-- This dumps the 'FileList' object containing all the 'File' objects in the
-- folder, resulting in very dense output. To get a simplier view, use
-- 'exampleListFolderNames' or 'exampleListFolderNamesWithIds'
listFolder :: T.Text -> IO FileList
listFolder name = do
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ driveScope)
  runResourceT . runGoogle env $ do
    md <- send $ flQ .~ (mkFolderQuery name) $ filesList
    case mkChildQuery md of
      Nothing -> return $ fileList
      q       -> send $ flQ .~ q $ filesList

-- | Lists the names of all non-folder files in a Drive folder.
listFolderNames :: T.Text -> IO [T.Text]
listFolderNames name = do
  files <- (^. flFiles) <$> listFolder name
  return $ catMaybes $ (^. fName) <$> files

-- | Lists the names and ids of all non-folder files in a Drive folder.
listFolderNamesWithIds :: T.Text -> IO [(T.Text, T.Text)]
listFolderNamesWithIds name = do
  files <- (^. flFiles) <$> listFolder name
  return $ catMaybes $ nameAndId <$> files

-- | Download a file and save it in a local path.
downloadAFile :: T.Text -> FilePath -> IO ()
downloadAFile srcId dst = do
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ driveScope)
  runResourceT . runGoogle env $ do
    stream <- download (filesExport "text/plain" srcId)
    liftResourceT (stream $$+- sinkFile dst)

-- | Download the files in a folder to a local directory.
downloadFolderToDir :: FilePath -> T.Text -> IO ()
downloadFolderToDir root folder = do
  namesAndIds <- listFolderNamesWithIds folder
  forM_ namesAndIds $ \(name, itsId) -> do
    downloadAFile itsId $ root </> gdoc2base name ++ ".yaml"

-- | Download the files in a folder in a standard location.
downloadFolder :: T.Text -> IO ()
downloadFolder folder = do
  dst <- (</> gdoc2base folder) <$> createDefaultDirIfMissing
  createDirectoryIfMissing True dst
  downloadFolderToDir dst folder
