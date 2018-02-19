{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Path.Default
Description : Shared functions for manipulating paths and the filesystem
Copyright   : (c) Tim Emiola, 2018
License     : None
Maintainer  : sam@sumikacrafts.com
Stability   : experimental
-}
module Path.Default
  (
    createDefaultDirIfMissing
  , defaultDirRel
  , defaultDir
  , mkBasename
  )
where

import           Control.Monad.IO.Class
import           Data.Monoid
import           Data.Text              (Text, replace, unpack)
import           System.Directory       (createDirectoryIfMissing,
                                         getHomeDirectory)
import           System.FilePath        ((</>))

-- | The relative path of the root to the home directory.
defaultDirRel :: FilePath
defaultDirRel = ".sumikac" </> "downloads"

-- | The default directory on this system.
defaultDir :: (MonadIO m) => m FilePath
defaultDir = liftIO $ (</> defaultDirRel) <$> getHomeDirectory

-- | Ensures the default directory is available.
createDefaultDirIfMissing :: (MonadIO m) => m FilePath
createDefaultDirIfMissing = do
  dir <- defaultDir
  liftIO $ createDirectoryIfMissing True dir
  return dir

-- | Make files basename given its extension.
mkBasename :: Text -> Text -> FilePath
mkBasename ext = unpack . (<> ext) . replace "/" "-"
