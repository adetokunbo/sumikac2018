module Main where

import Data.Sumikac.Conduit

main :: IO ()
main = do
  let src = "/Users/tbetbetbe/tmp/test_download2/"
  let dst = "/Users/tbetbetbe/tmp/remove_me"
  runAll src dst
