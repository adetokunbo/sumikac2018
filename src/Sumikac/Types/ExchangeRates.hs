{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Sumikac.Types.ExchangeRates
Description : Model the exchange rates used in the SumikaCrafts website
Copyright   : (c) Tim Emiola, 2018
License     : None
Maintainer  : sam@sumikacrafts.com
Stability   : experimental
-}
module Sumikac.Types.ExchangeRates where

import qualified Control.Exception as Exc

import           Data.Char         (toLower)
import           Data.List         (drop)
import           Data.Map.Strict   (Map)
import qualified Data.Map.Strict   as Map
import           Data.Text         (Text)

import           Data.Aeson
import           Data.Scientific   as Sci
import           Data.Yaml         (ParseException (..))

import           GHC.Generics


-- | FromUSD contains the rates of exchange between currencies.
data FromUSD = FromUSD
  {
    _fuRates :: Map Text Scientific
  } deriving (Show, Generic)

-- | Determine the conversion rates from Japanese Yen to the target currencies.
--
-- If the products were priced in USD, we could multiply their prices using the
-- values in FromUSD directly to get the currency specific rates. However, they
-- are priced in JPY so instead we compute conversion rates from JPY
--
-- Also, the site only displays a few currencies; the rest are dropped
mkYenRates
  :: [Text]   -- ^ the currencies in which Yen exchange rates are required
  -> FromUSD  -- ^ the USD based exchange rates
  -> Either ParseException (Map Text Double)
mkYenRates xs FromUSD {_fuRates} =
  let filteredRates = Map.filterWithKey (\k _ -> elem k xs) _fuRates
      sciDiv x y = (Sci.toRealFloat x :: Double) / (Sci.toRealFloat y :: Double)
  in
    case Map.lookup "JPY" _fuRates of
      Just rate -> Right $ Map.map (\x -> x `sciDiv` rate) filteredRates
      _         -> Left $ OtherParseException $ Exc.toException NoYenRates

-- | Exception that indicates that Yen conversion rates could not be derived.
data NoYenRates = NoYenRates

instance Show NoYenRates where
  show _ = "Could not compute the Yen conversion rates"

instance Exc.Exception NoYenRates

currenciesOptions :: Options
currenciesOptions = defaultOptions
  { fieldLabelModifier = transformFst toLower . drop 3
  , omitNothingFields = True
  }
  where
    transformFst _ []     = []
    transformFst f (x:xs) = (f x):xs

instance FromJSON FromUSD where
  parseJSON = genericParseJSON currenciesOptions

instance ToJSON FromUSD where
  toJSON = genericToJSON currenciesOptions
  toEncoding = genericToEncoding currenciesOptions
