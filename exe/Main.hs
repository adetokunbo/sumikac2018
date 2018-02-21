{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-|
Module      : Main.hs
Description : Top-level Main for create-sumikac-site.
Copyright   : (c) Tim Emiola, 2018
License     : None
Maintainer  : sam@sumikacrafts.com
Stability   : experimental
-}
module Main where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource

import           Data.Text                      (Text)
import           Network.HTTP.EmsSiteScrape
import           Network.HTTP.Gogol.Drive
import           Network.HTTP.Gogol.Picasa
import           Network.HTTP.OpenExchangeRates
import           Sumikac.Conduit
import           System.FilePath

main :: IO ()
main = runResourceT $ refresh' defaultConfig

refresh'
  :: (MonadBaseControl IO m, MonadResource m, MonadCatch m)
  => Config -> m ()
refresh' Config { user, dirs } = do
  let KnownDirs {working, downloads} = dirs
  scrapeEmsInfoTo $ downloads </> "ems_delivery_costs.yaml"
  liftIO $ downloadRatesTo $ downloads </> "latest_rates.yaml"
  downloadFile "Products/SiteInfo/Currencies" $ downloads </> "site_currencies.yaml"
  getWebAlbums working user
  downloadFolder "Products/Description/English" downloads
  downloadFolder "Products" downloads
  runAll downloads working

-- | Configures the site refresh
data Config = Config
  { user :: Text
  , dirs :: KnownDirs
  }

-- | Identifies the various directories used during the site refresh
data KnownDirs = KnownDirs
  { downloads :: FilePath
  , working   :: FilePath
  , site      :: FilePath
  }

defaultConfig :: Config
defaultConfig = Config
  { user = "maya.n@sumikacrafts.com"
  , dirs = KnownDirs
      { downloads = "/Users/tbetbetbe/tmp/test_download5"
      , working =  "/Users/tbetbetbe/tmp/test_wd5"
      , site =  "/Users/tbetbetbe/tmp/test_dst5"
      }
  }
