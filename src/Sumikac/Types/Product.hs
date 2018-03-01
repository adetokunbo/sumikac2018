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
    FullProduct
  , FullProductEnv(..)
  , fullProduct
  , fullProductBasename

  -- * Lenses into a 'FullProduct'
  , allPrices
  , core
  , coreDerived
  , deliveryCosts
  , fullDesc
  , galleryImages
  , imageGroups

  -- * Products
  , Product(..)
  , productBasename

  -- * Lenses into a 'Product'
  , internalName
  , categories
  )
where

import           Control.Applicative

import qualified Control.Exception              as Exc
import           Data.List.NonEmpty             (NonEmpty (..))
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as Map
import           Data.Text                      (Text)
import qualified Data.Text                      as Text

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

-- | Fields needed in to display a product that are derived from its other
-- fields
data ProductDerivations = ProductDerivations
  { _ppAllMaterials      :: Maybe Text -- ^ all the materials comma-separated
  , _ppDeliveryWeight    :: Weight -- ^ the delivery weight
  , _ppHasManyDimensions :: Bool -- ^ if there are many dimensions
  , _ppManyDimensions    :: [NamedDimension] -- ^ the text of many dimensions
  , _ppHasColours        :: Bool -- ^ if there any colours
  , _ppHasSetSizes       :: Bool -- ^ if there any set sizes
  , _ppHasShapes         :: Bool -- ^ if there any shapes
  , _ppHasPatterns       :: Bool -- ^ if there are any patterns
  , _ppIsNormal          :: Bool  -- ^ if there no colours, set sizes, shapes or patterns
  } deriving (Show, Generic)

instance FromJSON ProductDerivations where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON ProductDerivations where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding  $ aesonPrefix snakeCase

-- | Exception that indicates that a product had no delivery weight
data NoDeliveryWeight = NoDeliveryWeight

instance Show NoDeliveryWeight where
  show _ = "Could not compute a delivery weight"

instance Exc.Exception NoDeliveryWeight

mkDerivations :: Product -> Either NoDeliveryWeight ProductDerivations
mkDerivations p =
  let
    allM = Text.intercalate "," <$> _materials p
    deliveryW = _weightAfterWrapping p <|> _weight p
    hasMd = has _Just $ _manyDimensions p
    md = maybe [] unNamedDimensions $ _manyDimensions p
    withErr = maybe $ Left NoDeliveryWeight
    hasShapes' = has _Just $ _shape p
    hasColours' = has _Just $ _colours p
    hasSetSizes' = has _Just $ _setSizes p
    hasPatterns' = has _Just $ _patterns p
    isNormal' = not (hasShapes' || hasColours' || hasSetSizes' || hasPatterns')
    mkIt w =
      Right $ ProductDerivations
      { _ppAllMaterials = allM
      , _ppDeliveryWeight = w
      , _ppHasManyDimensions = hasMd
      , _ppManyDimensions = md
      , _ppHasShapes = hasShapes'
      , _ppHasColours = hasColours'
      , _ppHasPatterns = hasPatterns'
      , _ppHasSetSizes = hasSetSizes'
      , _ppIsNormal = isNormal'
      }
  in
    withErr mkIt deliveryW

-- | Smart constructor for creating a 'FullProduct'.
fullProduct
  :: Product
  -> FullProductEnv
  -> NonEmpty ImageGroup
  -> FullDesc
  -> Either NoDeliveryWeight FullProduct
fullProduct p fpe@(FullProductEnv { _fpeRates, _fpeCosts }) imgs desc =
  let
    mkIt d = FullProduct
      { _fpCore = p,
        _fpCoreDerived = d
      , _fpDeliveryCosts = allDeliveryCosts fpe p
      , _fpAllPrices = mkPrices p _fpeRates
      , _fpImageGroups = imgs
      , _fpGalleryImages = asGalleryImages imgs
      , _fpFullDesc = desc
      }

    mkPrices :: RealFloat a => Product -> Map k a -> Map k Scientific
    mkPrices y = Map.map (\x -> Sci.fromFloatDigits $ x * (fromIntegral $ _price y))
  in
    either Left (Right . mkIt) $ mkDerivations p

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
  , _fpCoreDerived   :: ProductDerivations
  , _fpDeliveryCosts :: Maybe (Map EmsZone (Map Text Scientific))
  , _fpAllPrices     :: Map Text Scientific
  , _fpImageGroups   :: NonEmpty ImageGroup
  , _fpGalleryImages :: NonEmpty GalleryImage
  , _fpFullDesc      :: FullDesc
  } deriving (Show, Generic)

makeLensesWith abbreviatedFields ''FullProduct

-- | The basename of the path to store the encoded 'FullProduct'.
fullProductBasename :: FullProduct-> FilePath
fullProductBasename fp = mkBasename "-complete.yaml" (fp ^. core . internalName)

instance FromJSON FullProduct where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON FullProduct where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

-- | The basename of the path to store the encoded 'Product'.
productBasename :: Product -> FilePath
productBasename = mkBasename ".yaml" . _internalName
