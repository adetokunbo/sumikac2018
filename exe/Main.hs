{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Main.hs
Description : Top-level Main for create-sumikac-site.
Copyright   : (c) Tim Emiola, 2018
License     : None
Maintainer  : sam@sumikacrafts.com
Stability   : experimental
-}
module Main where

import qualified Network.HTTP.Gogol.Picasa as GP
import qualified Network.HTTP.EmsSiteScrape as ESS
import           Sumikac.Conduit

mainParseProducts :: IO ()
mainParseProducts = do
  let src = "/Users/tbetbetbe/tmp/test_download2/"
  let dst = "/Users/tbetbetbe/tmp/remove_me"
  runAll src dst

mainWebAlbums :: IO ()
mainWebAlbums = do
  let dir = "/Users/tbetbetbe/tmp/test_download3/"
      user = "maya.n@sumikacrafts.com"
  GP.main dir user

mainEmsScrape :: IO ()
mainEmsScrape = do
  let dst = "/Users/tbetbetbe/tmp/remove_me/ems_rates.yaml"
  ESS.scrapeTo dst

main :: IO ()
main = mainParseProducts
