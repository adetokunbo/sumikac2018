{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Network.HTTP.EmsSiteScrape
Description : Scrapes the EMS site to obtain delivery costs.
Copyright   : (c) Tim Emiola, 2018
License     : AllRightsReserved
Maintainer  : sam@sumikacrafts.com
Stability   : experimental
-}
module Network.HTTP.EmsSiteScrape
  (
    scrapeEmsInfoTo
  )
where

import           Control.Monad.IO.Class

import qualified Data.ByteString.Lazy.Char8     as LBC
import qualified Data.ByteString                as BS
import           Data.Text                      (Text)
import           Data.Text.Encoding.Error       (lenientDecode)
import           Data.Text.Lazy.Encoding        (decodeUtf8With)
import           System.Directory
import           System.FilePath

import           Control.Lens                   hiding (children, element,
                                                 elements)
import           Data.Yaml                      (encode)
import           Network.Wreq
import           Text.Taggy.Lens

import           Sumikac.Types.EmsDeliveryCosts

-- | Scrapes the ems site and downloads the delivery costs to path.
scrapeEmsInfoTo :: (MonadIO m) => FilePath -> m ()
scrapeEmsInfoTo path = liftIO $ do
  scraped <- toDeliveryCosts <$> get chargesURL
  case scraped of
    Left err -> putStrLn $ "Scrape failed: " ++ err
    Right scraped' -> do
      createDirectoryIfMissing True $ takeDirectory path
      BS.writeFile path $ encode scraped'

-- | The EMS page scraped for the charges information.
chargesURL :: String
chargesURL = "http://www.post.japanpost.jp/int/charge/list/ems_all_en.html"

toDeliveryCosts :: Response LBC.ByteString -> Either String EmsDeliveryCosts
toDeliveryCosts req = case allTableCells req of
  (x:xs) -> scrapeTable x xs
  _ -> Left "No data found, cold not scrape the site"

allTableCells :: Response LBC.ByteString -> [[Text]]
allTableCells req =
  let rows = drop 1 $ allDeliveryCostRows req  -- the first row can be ignored
  in map rowContents $ rows ^.. traverse . element . children

allDeliveryCostRows :: Response LBC.ByteString -> [Node]
allDeliveryCostRows req = maybe [] id $ req ^.. allTbodys ^? (ix 2) . children

allTbodys
  :: (Contravariant f, Applicative f)
  => (Element -> f Element)
  -> Response LBC.ByteString
  -> f (Response LBC.ByteString)
allTbodys = responseHtml . allNamed (only "tbody")

responseHtml
  :: (Contravariant f, Applicative f)
  => (Node -> f Node)
  -> Response LBC.ByteString
  -> f (Response LBC.ByteString)
responseHtml = responseBody . to (decodeUtf8With lenientDecode) . html

rowContents :: (HasElement a, Traversable t) => t a -> [Text]
rowContents rowNode = rowNode ^.. traverse . element . contents
