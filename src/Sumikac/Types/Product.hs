{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-|
Module      : Sumikac.Types.Product
Description : Models the products displayed by the SumikaCrafts website.
Copyright   : (c) Tim Emiola, 2018
License     : None
Maintainer  : sam@sumikacrafts.com
Stability   : experimental
-}
module Sumikac.Types.Product
  (
    -- * Product definition
    FullProduct(..)
  , FullProductEnv(..)
  , Product(..)
  , fullProduct
  , fullProductBasename
  , productBasename
  , allDeliveryCosts
  )
where

import           Control.Applicative

import           Data.Char                      (toUpper)
import           Data.List                      (drop)
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as Map
import           Data.Text                      (Text)

import           Data.Aeson
import           Data.Scientific                as Sci

import           GHC.Generics

import           Path.Default
import           Sumikac.Types.Description
import           Sumikac.Types.EmsDeliveryCosts
import           Sumikac.Types.NamedDimensions
import           Sumikac.Types.Weight
import           Sumikac.Types.YenAmount
import           Sumikac.Types.Picasa

-- In Asuta Wan, there were 'Made by' which should have been supplier
-- In Bamboo_vase, there is an OriginalName; I'm not sure why

-- | The basename of the path to store the encoded 'FullProduct'.
fullProductBasename :: FullProduct-> FilePath
fullProductBasename =
  mkBasename "-complete.yaml" .  _fdInternalName . _fpFullDesc

-- | Smart constructor for creating a full product
fullProduct
  :: Product
  -> FullProductEnv
  -> [ImageGroup]
  -> FullDesc
  -> FullProduct
fullProduct p fpe@(FullProductEnv { _fpeRates, _fpeCosts }) imgs =
  let
    dc = allDeliveryCosts fpe p
    mkPrices :: RealFloat a => Product -> Map k a -> Map k Scientific
    mkPrices y = Map.map (\x -> Sci.fromFloatDigits $ x * (fromIntegral $ _price y))
  in
    FullProduct p dc (mkPrices p _fpeRates) imgs

-- | Context used when creating a 'FullProduct' with a product
data FullProductEnv = FullProductEnv
  { _fpeRates :: Map Text Double
  , _fpeCosts :: EmsDeliveryCosts
  }

-- | Determine the deliveryCosts for a single product in multiple currencies.
allDeliveryCosts
  :: FullProductEnv
  -> Product
  -> Maybe (Map EmsZone (Map Text Scientific))
allDeliveryCosts env p = Map.map expand <$> yenCosts
  where
    FullProductEnv { _fpeCosts, _fpeRates} = env
    expand = flip multiplyValues _fpeRates
    multiplyValues y = Map.map (\x -> Sci.fromFloatDigits $ x * (fromIntegral y))
    yenCosts = yenDeliveryCosts _fpeCosts p

-- | Determine the deliveryCosts in Yen of a single product.
yenDeliveryCosts :: EmsDeliveryCosts -> Product -> Maybe (Map EmsZone YenAmount)
yenDeliveryCosts costs p = calc theWeight
  where theWeight = _weight p <|> _weightAfterWrapping p
        calc Nothing  = snd <$> Map.lookupMin costs
        calc (Just x) = lookupAtWeight x costs

-- | FullProduct provies the information about a given product from different
-- sources in a single.
data FullProduct = FullProduct
  { _fpProduct       :: Product
  , _fpDeliveryCosts :: Maybe (Map EmsZone (Map Text Scientific))
  , _fpAllPrices     :: Map Text Scientific
  , _fpWebImages     :: [ImageGroup]
  , _fpFullDesc      :: FullDesc
  } deriving (Show, Generic)

instance FromJSON FullProduct where
  parseJSON = genericParseJSON drop3Options

instance ToJSON FullProduct where
  toJSON = genericToJSON drop3Options
  toEncoding = genericToEncoding drop3Options

drop3Options :: Options
drop3Options = defaultOptions
  { fieldLabelModifier = modifyFields
  , omitNothingFields = True
  }
  where
    modifyFields = transformFst toUpper . drop 3
    transformFst _ []     = []
    transformFst f (x:xs) = (f x):xs


-- | The basename of the path to store the encoded 'Product'.
productBasename :: Product -> FilePath
productBasename = mkBasename ".yaml" . _internalName

-- | A 'Product' is the core item that the sumikacrafts website gives access to
-- the public.
data Product = Product
  { _internalName        :: Text
  , _capacity            :: Maybe Text
  , _categories          :: [Text]
  , _colours             :: Maybe [Text]
  , _cost                :: YenAmount
  , _dimensions          :: Maybe Text
  , _manyDimensions      :: Maybe NamedDimensions
  , _expectedShippingFee :: Maybe YenAmount
  , _madeIn              :: Maybe Text
  , _materials           :: Maybe [Text]
  , _maxItems            :: Maybe Int
  , _originalName        :: Maybe Text
  , _patterns            :: Maybe [Text]
  , _price               :: YenAmount
  , _setSizes            :: Maybe [Int]
  , _shape               :: Maybe [Text]
  , _supplier            :: Maybe Text
  , _weight              :: Maybe Weight
  , _weightAfterWrapping :: Maybe Weight
  } deriving (Show, Generic)

productOptions :: Options
productOptions = defaultOptions
  { fieldLabelModifier = transformFst toUpper . drop 1
  , omitNothingFields = True
  }
  where
    transformFst _ []     = []
    transformFst f (x:xs) = (f x):xs

instance FromJSON Product where
  parseJSON = genericParseJSON productOptions

instance ToJSON Product where
  toJSON = genericToJSON productOptions
  toEncoding = genericToEncoding productOptions
