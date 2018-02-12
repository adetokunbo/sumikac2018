{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-|

Module      : Client.Currencies
Description : Accesses OpenExchangeRates.org to obtain currency exchange rates.
Copyright   : (c) Tim Emiola, 2018
License     : None
Maintainer  : sam@sumikacrafts.com
Stability   : experimental
-}
module Client.Currencies
  (
    download
  )

where

import Network.HTTP.Simple
import System.FilePath
import System.Directory
import Data.ByteString as BS

import Data.Sumikac.Types
import Data.Yaml (encode)

-- TODO: move apiId and URI to a config package
apiId :: String
apiId = "13eacae0bb8b41419d00037883e3cb29"

apiURI :: String
apiURI = "http://openexchangerates.org/api/latest.json?app_id=" ++ apiId

-- | download request the exchange rates and writes them to a path.
download :: FilePath -> IO ()
download path = do
  createDirectoryIfMissing True $ takeDirectory path
  req <- parseRequest apiURI
  resp :: Response Currencies <- httpJSON req -- see Note httpJSON vs httpJSONEither
  BS.writeFile path $ encode $ getResponseBody resp

{- Note httpJSON vs httpJSONEither
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

httpJSONEither allows handling of JSON parsing errors; whereas httpJSON
throws a runtime exception

httpJSONEither is not used here
 - the format of the JSON is stable
 - parsing will only fail if there is a programming bug, in which
   case the program should halt

-}
