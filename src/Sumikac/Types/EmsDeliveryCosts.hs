{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Sumikac.Types.EmsDeliveryCosts
Description : A type that holds the delivery costs obtained from EMS.
Copyright   : (c) Tim Emiola, 2018
License     : AllRightsReserved
Maintainer  : sam@sumikacrafts.com
Stability   : experimental
-}
module Sumikac.Types.EmsDeliveryCosts
  ( EmsDeliveryCosts
  , EmsZone
  , CellHeader
  , CellValue
  , lookupAtWeight
  , scrapeTable
  , scrapeRow
  ) where

import Control.Applicative

import           Data.Aeson
import           Data.Foldable           (fold)
import           Data.List               (drop, isPrefixOf)
import qualified Data.Map                as Map
import           Data.Map.Strict         (Map)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Text.Read               (readEither)

import           Sumikac.Types.Weight
import           Sumikac.Types.YenAmount

-- | Convert a table [[CellValue]] to 'EmsDeliveryCosts'.
--
-- The first row of [CellValue] are taken to be [CellHeaders]
--
-- On conversion failures, the cause is reported as a String.
scrapeTable :: [CellHeader] -> [[CellValue]] -> Either String EmsDeliveryCosts
scrapeTable x xs = fold <$> (mapM scrapeRow $ map (\y -> zip x y) xs)

-- | Convert a [(header, value)] to a 'EmsDeliveryCosts'.
--
-- On conversion failures, the cause is reported as a String.
scrapeRow :: [(CellHeader, CellValue)] -> Either String EmsDeliveryCosts
scrapeRow (("Weight", w):xs) = do
  weight <- readWeight $ Text.replace "Up to " "" $ w
  zoneCosts <- fold <$> mapM scrapeZoneCost xs
  return $ Map.singleton weight zoneCosts

scrapeRow [] = Right Map.empty
scrapeRow _ = Left "Could not scrape a row of delivery costs"

scrapeZoneCost :: (Text, Text) -> Either String (Map EmsZone YenAmount)
scrapeZoneCost (z, amt) = Map.singleton <$> readHtmlEmsZone z <*> scrapeYen amt
  where scrapeYen = readYenAmount
          . Text.replace "," ""
          . Text.replace " yen" " JPY"

-- | Lookup the delivery costs to each zone at a given weight
lookupAtWeight :: Weight -> EmsDeliveryCosts -> Maybe (Map EmsZone YenAmount)
lookupAtWeight w costs =
  let biggest = Map.lookupGE w costs <|> Map.lookupMax costs
  in snd <$> biggest

-- | Types used to make the scrapeXXX signatures more readable.
type CellHeader = Text
type CellValue  = Text

-- | The EMS site provides a table of indicates the price for delivery to each
-- zone for various weight ranges. This information is stored in
-- EmsDeliveryCosts, with the weight key being the lower bound of a weight range.
type EmsDeliveryCosts = Map Weight (Map EmsZone YenAmount)

-- | EmsZone is a delivery zone of the EMS delivery service.
--
-- The delivery cost of a package varies by weight and delivery zone.
data EmsZone = Asia | Europe | Oceania | SouthAtlantic
  deriving (Eq, Ord, Show, Read)

instance ToJSON EmsZone where
  toJSON = toJSON . show
  toEncoding = toEncoding . show

instance FromJSON EmsZone where
  parseJSON = withText "EmsZone" $ \x -> do
    case readEmsZone x of
      Left err  -> fail err
      Right amt -> return amt

instance ToJSONKey EmsZone
instance FromJSONKey EmsZone

readEmsZone :: Text -> Either String EmsZone
readEmsZone x =
  case (readEither $ Text.unpack x) of
    Left _ -> Left "Could not parse an invalid EmsZone"
    ok     -> ok

-- | A newtype of HtmlEmsZone purely for scanning the HTML input.
--
-- This does not have a Aeson instances, but it's read and show match the expected titles in the HTML table
newtype HtmlEmsZone = HtmlEmsZone EmsZone
  deriving (Eq, Ord)

instance Show HtmlEmsZone where
  show (HtmlEmsZone Asia)          = "Asia"
  show (HtmlEmsZone Europe)        = "Europe"
  show (HtmlEmsZone Oceania)       = "Oceania, North America, Central America and the Middle East"
  show (HtmlEmsZone SouthAtlantic) = "South America and Africa"

instance Read HtmlEmsZone where
  readsPrec _ s
    | isPrefix (HtmlEmsZone Asia) s = withPrefixDropped (HtmlEmsZone Asia) s
    | isPrefix (HtmlEmsZone Europe) s = withPrefixDropped (HtmlEmsZone Europe) s
    | isPrefix (HtmlEmsZone Oceania) s = withPrefixDropped (HtmlEmsZone Oceania) s
    | isPrefix (HtmlEmsZone SouthAtlantic) s = withPrefixDropped (HtmlEmsZone SouthAtlantic) s
    | otherwise = []
    where withPrefixDropped z full = [(z, drop (length $ show z) full)]

readHtmlEmsZone :: Text -> Either String EmsZone
readHtmlEmsZone x =
  case (readEither $ Text.unpack x) of
    Left _ -> Left "Could not parse an invalid HtmlEmsZone"
    Right (HtmlEmsZone z)     -> Right z

isPrefix :: HtmlEmsZone -> String -> Bool
isPrefix z s = show z `isPrefixOf` s
