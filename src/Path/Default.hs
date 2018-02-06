module Path.Default
  (
    createDefaultDirIfMissing
  , defaultDirRel
  , defaultDir
  )
where

import           System.Directory (createDirectoryIfMissing, getHomeDirectory)
import           System.FilePath              ((</>))

-- | The relative path of the root to the home directory.
defaultDirRel :: FilePath
defaultDirRel = ".sumikac" </> "downloads"

-- | The default directory on this system.
defaultDir :: IO FilePath
defaultDir = (</> defaultDirRel) <$> getHomeDirectory

-- | Ensures the default directory is available.
createDefaultDirIfMissing :: IO FilePath
createDefaultDirIfMissing = do
  dir <- defaultDir
  createDirectoryIfMissing True dir
  return dir
