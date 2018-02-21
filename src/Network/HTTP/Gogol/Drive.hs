{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-|
Module      : Network.HTTP.Gogol.Drive
Description : Accesses content stored on Google Drive
Copyright   : (c) Tim Emiola, 2018
License     : None
Maintainer  : sam@sumikacrafts.com
Stability   : experimental
-}

module Network.HTTP.Gogol.Drive
  (
  -- * Download files and/or folders.
  downloadFile
  , downloadFile'
  , downloadFolder
  , downloadFolder'

  -- * List the contents of folders.
  , listFolder
  , listFolderNames
  , listFolderNamesWithIds
  )

where
---------------------------------------------------------------------------------
import           Control.Monad                (foldM, forM_)
import           Control.Monad.Catch
import           Control.Monad.Trans.Resource (liftResourceT)

import qualified Control.Exception            as Exc
import           Data.Maybe                   (catMaybes, listToMaybe)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           System.Directory             (createDirectoryIfMissing)
import           System.FilePath              ((</>))
import           System.IO                    (stdout)

import           Conduit
import           Control.Lens                 ((.~), (<&>), (^.))
import           Data.Conduit                 (($$+-))
import           Formatting                   (Format, sformat, (%))
import           Formatting.Formatters        (stext)
import           Network.Google
import           Network.Google.Drive


-- * Environment
--
-- In order to be able to run these examples you need to create a service acount
-- from google's developers console and copy the dowloaded json file to
-- ~/.config/gcloud/application_default_credentials.json.
--
-- You must also share with your service the folder that you want to get the info
-- of. In order to do this you must share the folder with the email address of your
-- service which is in the downloaded service config file.

-- | Thrown when downloadFile cannot find the requested file.
data DriveFileNotFound = DriveFileNotFound Text

instance Show DriveFileNotFound where
  show (DriveFileNotFound s) = "Could not find " ++ Text.unpack s

instance Exc.Exception DriveFileNotFound

-- | Download a file from its drive path to a local path.
downloadFile
  :: (MonadCatch m, MonadResource m, MonadBaseControl IO m)
  => Text -> FilePath -> m ()
downloadFile = runDownload downloadFile'

-- | Download the files in a Drive folder to a local directory.
downloadFolder
  :: (MonadCatch m, MonadResource m, MonadBaseControl IO m)
  => Text -> FilePath -> m ()
downloadFolder = runDownload downloadFolder'

-- | runDownload is convenience function used to run the downloadXXX' functions
-- of this module in the Resource and Google monads
runDownload
  :: (MonadResource m, MonadBaseControl IO m, MonadCatch m)
  => (t -> t1 -> Google '["https://www.googleapis.com/auth/drive"] b)
     -> t -> t1 -> m b
runDownload f name dst = do
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ driveScope)
  runResourceT . runGoogle env $ f name dst

-- | Download a file from its drive path to a local path.
--
-- If the file cannnot be found, DriveFileNotFound is thrown in the IO monad.
downloadFile'
  :: ( HasScope s FilesExport,
       HasScope s FilesGet,
       MonadGoogle s m,
       MonadBaseControl IO m,
       MonadResource m
     )
  => Text -> FilePath -> m ()
downloadFile' name dst = do
  f <- lookupFile name
  case maybe Nothing (^. fId) f of
    Nothing -> throwM $ DriveFileNotFound name
    Just theId -> do
      stream <- download (filesExport "text/plain" theId)
      liftResourceT (stream $$+- sinkFile dst)

-- | Download the files in a Drive folder to a local directory.
downloadFolder'
  :: ( HasScope s FilesExport,
       HasScope s FilesGet,
       MonadGoogle s m,
       MonadBaseControl IO m,
       MonadResource m
     )
  => Text -> FilePath -> m ()
downloadFolder' folder d = do
  let subd = d </> gdoc2base folder
  liftIO $ createDirectoryIfMissing True subd
  namesAndIds <- listFolderNamesWithIds folder
  forM_ namesAndIds $ \(name, itsId) -> do
    downloadFileById itsId $ subd </> gdoc2base name ++ ".yaml"

-- | Download a file and save it in a local path.
downloadFileById
  :: ( HasScope s FilesExport,
       MonadGoogle s m,
       MonadBaseControl IO m,
       MonadResource m
     )
  => Text -> FilePath -> m ()
downloadFileById srcId dst = do
  stream <- download (filesExport "text/plain" srcId)
  liftResourceT (stream $$+- sinkFile dst)

-- | Lists files in a Drive folder.
--
-- This dumps the 'FileList' object containing all the 'File' objects in the
-- folder, resulting in very dense output. To get a simplier view, use
-- 'exampleListFolderNames' or 'exampleListFolderNamesWithIds'
listFolder
  :: ( HasScope s FilesGet
     , MonadGoogle s m
     )
  => Text -> m FileList
listFolder name = do
  let parts = Text.split (== '/') name
  parentId <- foldM getSub Nothing parts
  case parentId of
    (Just (Just theId)) -> send $ flQ .~ (Just $ mkFilesQuery theId) $ filesList
    _ -> return $ fileList

-- | Lookup a file in a Drive folder.
--
-- This finds the 'File' corresponding to the named file
lookupFile
  :: ( HasScope s FilesGet
     , MonadGoogle s m
     )
  => Text -> m (Maybe File)
lookupFile name = do
  case Text.split (== '/') name of
    [] -> return $ Nothing
    [aPart] -> do
      let addQuery = flQ .~ (Just $ mkFileQuery aPart)
      fl <- send $ addQuery filesList
      return $ listToMaybe $ fl ^. flFiles
    parts -> do
      let folders = init parts
          base = last parts
      fl <- listFolder $ Text.intercalate "/" folders
      let namesWithFiles = catMaybes $ nameAndFile <$> fl ^. flFiles
      return $ lookup base namesWithFiles

getSub
  :: ( HasScope s FilesGet
     , MonadGoogle s m)
  => Maybe (Maybe Text)
  -> Text
  -> m (Maybe (Maybe Text))
getSub Nothing sub = do
  let modQuery = flQ .~ (Just $ mkFolderQuery sub)
      req = modQuery filesList
  childFileList <- send req
  return $ soleIdOf childFileList
getSub (Just Nothing) _ = return $ Just Nothing
getSub (Just (Just theId)) sub = do
  let modQuery = flQ .~ (Just $ mkSubQuery sub theId)
      req = modQuery filesList
  childFileList <- send req
  return $ soleIdOf childFileList

-- | Extract the first id from a FileList
soleIdOf :: FileList -> Maybe (Maybe Text)
soleIdOf fl = do
  f <- listToMaybe $ fl ^. flFiles
  return $ f ^. fId

-- | Lists the names of all non-folder files in a Drive folder.
listFolderNames
  :: ( HasScope s FilesGet
     , MonadGoogle s m
     )
  => Text -> m [Text]
listFolderNames name = do
  files <- (^. flFiles) <$> listFolder name
  return $ catMaybes $ (^. fName) <$> files

-- | Lists the names and ids of all non-folder files in a Drive folder.
listFolderNamesWithIds
  :: ( HasScope s FilesGet
     , MonadGoogle s m
     )
  => Text
  -> m [(Text, Text)]
listFolderNamesWithIds name = do
  files <- (^. flFiles) <$> listFolder name
  return $ catMaybes $ nameAndId <$> files

-- | Obtain the name and id from a file.
--
-- Implementation note: this is the applicative version of the following
--
-- @
-- nameAndId f = do
--   name <-  f ^. fName
--   itsId <- f ^. fId
--   return (name, itsId)
-- @
nameAndId :: File -> Maybe (Text, Text)
nameAndId f = (,) <$> (f ^. fName) <*> (f ^. fId)

-- | Obtain the name and id from a file.
--
-- Implementation note: this is the applicative version of the following
--
-- @
-- nameAndId f = do
--   name <-  f ^. fName
--   return (name, Just f)
-- @
nameAndFile :: File -> Maybe (Text, File)
nameAndFile f = (,) <$> (f ^. fName) <*> Just f

-- | Convert the name attribute of a Drive 'File' to a local basename.
gdoc2base :: Text -> FilePath
gdoc2base = Text.unpack . Text.replace " " "_"

-- | Format for a query that searches for folders
folderQueryFmt :: Format r (Text -> r)
folderQueryFmt =
  "name = '"
  % stext
  % "' and mimeType = 'application/vnd.google-apps.folder'"

-- | Make a query for searching for a folder with a given name
mkFolderQuery :: Text -> Text
mkFolderQuery = sformat folderQueryFmt

-- | Format for a query that searchs for a folder beneath a parent folder
subQueryFmt :: Format r (Text -> Text -> r)
subQueryFmt =
  "name = '"
  % stext
  % "' and '"
  % stext
  % "' in parents and mimeType = 'application/vnd.google-apps.folder'"

-- | Make a query for searching for a sub folder with a given name
mkSubQuery :: Text -> Text -> Text
mkSubQuery = sformat subQueryFmt

-- | Format for a query that searchs for files belonging to a given parent
filesQueryFmt :: Format r (Text -> r)
filesQueryFmt =
  "'"
  % stext
  % "' in parents and mimeType = 'application/vnd.google-apps.document'"

-- | Make a query for finding a Folder's chidren given its Id
mkFilesQuery :: Text -> Text
mkFilesQuery = sformat filesQueryFmt

-- | Format for a query that searchs for files given its name
fileQueryFmt :: Format r (Text -> r)
fileQueryFmt =
  "name = '"
  % stext
  % "' mimeType = 'application/vnd.google-apps.document'"

-- | Make a query for finding a file given its name
mkFileQuery :: Text -> Text
mkFileQuery = sformat fileQueryFmt
