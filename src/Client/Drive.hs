{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

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
import           Control.Monad                (forM_, foldM)
import           Control.Monad.Trans.Resource (liftResourceT, runResourceT)
import           Data.Conduit                 (($$+-))
import           Data.Maybe                   (catMaybes, listToMaybe)
import           Data.Text                    (Text, replace, unpack, split)
import           Formatting                   (Format, sformat, (%))
import           Formatting.Formatters        (stext)

import           Path.Default                 (createDefaultDirIfMissing)

import           System.Directory             (createDirectoryIfMissing,
                                               getHomeDirectory)
import           System.FilePath              ((</>))
import           System.IO                    (stdout)


-- | Format for a query that searches for folders
folderQueryFmt :: Format r (Text -> r)
folderQueryFmt =
  "name = '"
  % stext
  % "' and mimeType = 'application/vnd.google-apps.folder'"

-- | Make a query for searching for a folder with a given name
mkFolderQuery :: Text -> Text
mkFolderQuery name = sformat folderQueryFmt name

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
mkSubQuery name theId = sformat subQueryFmt name theId

-- | Format for a query that searchs for files belonging to a given parent
filesQueryFmt :: Format r (Text -> r)
filesQueryFmt =
  "'"
  % stext
  % "' in parents and mimeType = 'application/vnd.google-apps.document'"

-- | Make a query for finding a Folder's chidren given its Id
mkFilesQuery :: Text -> Text
mkFilesQuery parentId = sformat filesQueryFmt parentId

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
nameAndId :: File -> Maybe (Text, Text)
nameAndId f = (,) <$> (f ^. fName) <*> (f ^. fId)

-- | Convert the name attribute of a Drive 'File' to a local basename.
gdoc2base :: Text -> FilePath
gdoc2base = unpack . replace " " "_"

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
listFolder :: Text -> IO FileList
listFolder name = do
  let withId fl = do
        f <- listToMaybe $ fl ^. flFiles
        return $ f ^. fId

  -- | foldM helper function that traverses folders on Drive to get the driveId
  -- of the leaf folder
  let getSub Nothing name = do
        md <- send $ flQ .~ (Just $ mkFolderQuery name) $ filesList
        return $ withId md
      getSub (Just Nothing) _ = return $ Just Nothing
      getSub (Just (Just theId)) name = do
        md <- send $ flQ .~ (Just $ mkSubQuery name theId) $ filesList
        return $ withId md

  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ driveScope)
  runResourceT . runGoogle env $ do
    let parts = split (== '/') name
    parentId <- foldM getSub Nothing parts
    case parentId of
      (Just (Just theId)) -> send $ flQ .~ (Just $ mkFilesQuery theId) $ filesList
      _ -> return $ fileList

-- | Lists the names of all non-folder files in a Drive folder.
listFolderNames :: Text -> IO [Text]
listFolderNames name = do
  files <- (^. flFiles) <$> listFolder name
  return $ catMaybes $ (^. fName) <$> files

-- | Lists the names and ids of all non-folder files in a Drive folder.
listFolderNamesWithIds :: Text -> IO [(Text, Text)]
listFolderNamesWithIds name = do
  files <- (^. flFiles) <$> listFolder name
  return $ catMaybes $ nameAndId <$> files

-- | Download a file and save it in a local path.
downloadAFile :: Text -> FilePath -> IO ()
downloadAFile srcId dst = do
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ driveScope)
  runResourceT . runGoogle env $ do
    stream <- download (filesExport "text/plain" srcId)
    liftResourceT (stream $$+- sinkFile dst)

-- | Download the files in a folder to a local directory.
downloadFolderToDir :: Text -> FilePath -> IO FilePath
downloadFolderToDir folder d = do
  let subd = d </> gdoc2base folder
  createDirectoryIfMissing True subd
  namesAndIds <- listFolderNamesWithIds folder
  forM_ namesAndIds $ \(name, itsId) -> do
    downloadAFile itsId $ subd </> gdoc2base name ++ ".yaml"
  return subd

-- | Download the files in a folder in a standard location.
downloadFolder :: Text -> IO FilePath
downloadFolder f = createDefaultDirIfMissing >>= downloadFolderToDir f
