{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
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
  -- * Full Products
    FullProduct(..)
  , FullProductEnv(..)
  , fullProduct
  , fullProductBasename

  -- * Lenses for Full Product
  , allPrices
  , core
  , deliveryCosts
  , fullDesc
  , imageGroups

  -- * Products
  , Product(..)
  , productBasename

  -- * Lenses for Product
  , internalName
  , categories
  )
where

import           Control.Applicative

import           Data.List.NonEmpty             (NonEmpty(..))
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as Map
import           Data.Text                      (Text)

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Scientific                as Sci
import           Lens.Micro.Platform

-- Hide (to) as it conflicts with Lens.Micro.Platform
import           GHC.Generics                   hiding (to)

import           Path.Default
import           Sumikac.Types.Description
import           Sumikac.Types.EmsDeliveryCosts
import           Sumikac.Types.NamedDimensions
import           Sumikac.Types.Picasa
import           Sumikac.Types.Weight
import           Sumikac.Types.YenAmount

-- In Asuta Wan, there were 'Made by' which should have been supplier
-- In Bamboo_vase, there is an OriginalName; I'm not sure why

-- | A 'Product' is the core item that the sumikacrafts website gives access to
-- the public.
data Product = Product
  { _internalName        :: Text
  , _capacity            :: Maybe Text
  , _categories          :: NonEmpty Text
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

makeLensesFor [
  ("_categories", "categories"),
  ("_internalName", "internalName")
  ] ''Product

productOptions :: Options
productOptions =  (aesonDrop 1 pascalCase) { omitNothingFields = True }

instance FromJSON Product where
  parseJSON = genericParseJSON productOptions

instance ToJSON Product where
  toJSON = genericToJSON productOptions
  toEncoding = genericToEncoding productOptions

-- | Smart constructor for creating a 'FullProduct'.
fullProduct
  :: Product
  -> FullProductEnv
  -> NonEmpty ImageGroup
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
  where theWeight = _weightAfterWrapping p <|> _weight p
        calc Nothing  = snd <$> Map.lookupMin costs
        calc (Just x) = lookupAtWeight x costs

-- | FullProduct provies the information about a given product from different
-- sources in a single.
data FullProduct = FullProduct
  { _fpCore          :: Product
  , _fpDeliveryCosts :: Maybe (Map EmsZone (Map Text Scientific))
  , _fpAllPrices     :: Map Text Scientific
  , _fpImageGroups   :: NonEmpty ImageGroup
  , _fpFullDesc      :: FullDesc
  } deriving (Show, Generic)

makeLensesWith abbreviatedFields ''FullProduct

-- | The basename of the path to store the encoded 'FullProduct'.
fullProductBasename :: FullProduct-> FilePath
fullProductBasename fp = mkBasename "-complete.yaml" (fp ^. core . internalName)

instance FromJSON FullProduct where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

instance ToJSON FullProduct where
  toJSON = genericToJSON $ aesonPrefix pascalCase
  toEncoding = genericToEncoding $ aesonPrefix pascalCase

-- | The basename of the path to store the encoded 'Product'.
productBasename :: Product -> FilePath
productBasename = mkBasename ".yaml" . _internalName
