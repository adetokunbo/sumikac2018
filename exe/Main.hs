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
import           Sumikac.Conduit

mainDownloadProducts :: IO ()
mainDownloadProducts = do
  let src = "/Users/tbetbetbe/tmp/test_download2/"
  let dst = "/Users/tbetbetbe/tmp/remove_me"
  runAll src dst

mainWebAlbums :: IO ()
mainWebAlbums = do
  let dir = "/Users/tbetbetbe/tmp/test_download3/"
      user = "maya.n@sumikacrafts.com"
  GP.main dir user

main :: IO ()
main = mainWebAlbums
