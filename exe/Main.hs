{-|
Module      : Main.hs
Description : Top-level Main for create-sumikac-site.
Copyright   : (c) Tim Emiola, 2018
License     : None
Maintainer  : sam@sumikacrafts.com
Stability   : experimental
-}
module Main where

import           Data.Aeson
import           Data.ByteString.Lazy as LBS

import           Sumikac.Conduit
import           Sumikac.Types.Picasa

mainParse :: IO ()
mainParse = do
  let src = "/Users/tbetbetbe/tmp/test_download2/"
  let dst = "/Users/tbetbetbe/tmp/remove_me"
  runAll src dst

mainParsePicasa :: IO ()
mainParsePicasa = do
  let jsonFile = "/Users/tbetbetbe/tmp/test_download2/1album.json"
  bytes <- LBS.readFile jsonFile
  case eitherDecode bytes :: Either String RawImageGroups of
    Left err -> print err
    Right (RawImageGroups imgs) -> mapM_ (print . _poContent . unImageGroup) imgs

main :: IO ()
main = mainParsePicasa
