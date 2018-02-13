{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-|
Module      : Sumikac.Types
Description : Types that represent components of the SumikaCrafts website.
Copyright   : (c) Tim Emiola, 2018
License     : None
Maintainer  : sam@sumikacrafts.com
Stability   : experimental
-}
module Sumikac.Types
  (
    -- * Product definition
    FullProduct(..)
  , Product(..)
  , fullProduct
  , fullProductBasename
  , productBasename

    -- * Product components
  , FromUSD
  , NoYenRates
  , YenAmount
  , mkYenRates

    -- * Product description
  , DescAccum(..)
  , FullDesc(..)
  , LitDesc(..)
  , LabelledBlock(..)
  , ShortDesc(..)
  , addLitDesc
  , asFullDescs
  , descAccum
  , fullDescBasename
  )
where

import           Control.Applicative
import qualified Control.Exception   as Exc
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char           (toLower, toUpper)
import           Data.Foldable       (foldl')
import qualified Data.HashMap.Strict as HM
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Maybe
import           Data.Monoid         ((<>))
import           Data.Scientific     as Sci
import           Data.Text           (Text, replace, unpack)
import qualified Data.Text           as T
import           Data.Yaml           (ParseException (..))

import           Data.List           (drop, isPrefixOf)
import           GHC.Generics
import           Numeric             (readDec)
import           Text.Read           (readEither)

-- In Asuta Wan, there were 'Made by' which should have been supplier
-- In Bamboo_vase, there is an OriginalName; I'm not sure why


-- | The basename of the path to store the encoded 'FullProduct'.
fullProductBasename :: FullProduct-> FilePath
fullProductBasename =
  mkBasename "-complete.yaml" .  _fdInternalName . _fpFullDesc

-- | FullProduct provies the information about a given product from different
-- sources in a single.
data FullProduct = FullProduct
  { _fpProduct  :: Product
  , _fpAllPrices :: Map Text Scientific
  , _fpFullDesc :: FullDesc
  } deriving (Show, Generic)

-- | Smart constructor for creating a full product
fullProduct :: RealFloat a => Product -> Map Text a -> FullDesc -> FullProduct
fullProduct p curr = FullProduct p $ mkPrices p curr

instance FromJSON FullProduct where
  parseJSON = genericParseJSON drop3Options

instance ToJSON FullProduct where
  toJSON = genericToJSON drop3Options
  toEncoding = genericToEncoding drop3Options

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
  , _weight              :: Maybe Text
  , _weightAfterWrapping :: Maybe Text
  } deriving (Show, Generic)

productOptions :: Options
productOptions = defaultOptions
  { fieldLabelModifier = transformFst toUpper . drop 1
  , omitNothingFields = True
  }

instance FromJSON Product where
  parseJSON = genericParseJSON productOptions

instance ToJSON Product where
  toJSON = genericToJSON productOptions
  toEncoding = genericToEncoding productOptions

data NamedDimension = NamedDimension
  { _ndName  :: Text
  , _ndValue :: Text
  } deriving (Show)

newtype NamedDimensions = NamedDimensions
  { unNamedDimensions :: [NamedDimension]}
  deriving (Show)

instance FromJSON NamedDimensions where
  parseJSON = parseNamedDimensions

instance ToJSON NamedDimensions where
  toJSON = namedDimensionsToJSON

parseNamedDimensions :: Value -> Parser NamedDimensions
parseNamedDimensions = withObject "manyDimensions" $ \o -> do
  let mk (n, rawValue) = do
        v <- parseJSON rawValue
        return NamedDimension { _ndName = n, _ndValue = v}
  inner <- mapM mk (HM.toList o)
  return NamedDimensions { unNamedDimensions = inner}

namedDimensionsToJSON :: NamedDimensions -> Value
namedDimensionsToJSON = toJSON . HM.fromList . asList
  where
    asList = map toKeyValue . unNamedDimensions
    toKeyValue (NamedDimension n v) = (n,  toJSON v)

-- | LitDesc models the literal descripion Yaml as two distinct data formats
--
-- One is a short description, the other a block of text with a label.
data LitDesc
  = Block LabelledBlock
  | Short ShortDesc
  deriving (Show, Generic)

ldOptions :: Options
ldOptions = defaultOptions
  { sumEncoding = UntaggedValue
  , omitNothingFields = True
  }

instance FromJSON LitDesc where
  parseJSON = genericParseJSON ldOptions

instance ToJSON LitDesc where
  toJSON = genericToJSON ldOptions
  toEncoding = genericToEncoding ldOptions

-- | ShortDesc are a minimal description of the product, along with any links
-- that might occur in its paragraphs.
data ShortDesc = ShortDesc
  { _sdInternalName :: Text
  , _sdProductName  :: Text
  , _sdLinks        :: Maybe [Text]
  } deriving (Show, Generic)

instance FromJSON ShortDesc where
  parseJSON = genericParseJSON drop3Options

instance ToJSON ShortDesc where
  toJSON = genericToJSON drop3Options
  toEncoding = genericToEncoding drop3Options

-- | LabelledBlock holds text for display under a given heading.
--
-- It includes a list of the internal names it is to be shown for
data LabelledBlock = LabelledBlock
  { _lbLabel   :: Text
  , _lbText    :: Text
  , _lbShownBy :: Maybe [Text]
  } deriving (Show, Generic)

instance FromJSON LabelledBlock where
  parseJSON = genericParseJSON drop3Options

instance ToJSON LabelledBlock where
  toJSON = genericToJSON drop3Options
  toEncoding = genericToEncoding drop3Options

-- The Label names and ProductIds are both 'Text' values
type Label = Text
type ProductId = Text

-- | Sections contains named textual sections of the description
type Sections = Map Label Text

-- | CommonDesc contains the description data shared between the different
-- products.
data CommonDesc = CommonDesc
  { cdLinks    :: Maybe [Text] -- ^ all the links of all products in the files
  , cdSections :: Sections     -- ^ the global sections
  }

-- | SoloDesc contains description data that is specific to a product.
data SoloDesc = SoloDesc (Maybe ShortDesc) Sections

-- | DescAccum contains both the shared description and all the solo product descriptions.
data DescAccum = DescAccum CommonDesc (Map ProductId SoloDesc)

-- | A descAccum with nothing added to it.
descAccum :: DescAccum
descAccum = DescAccum CommonDesc{cdLinks=Nothing, cdSections=Map.empty} Map.empty

-- | Add a 'LitDesc' to a 'DescAccum'.
addLitDesc :: DescAccum -> LitDesc -> DescAccum

addLitDesc (DescAccum cd@CommonDesc {..} solos) (Block LabelledBlock {..}) =
  let
    addSectionToAll = foldl' (flip $ Map.alter addSection) solos
  in
    case _lbShownBy of
      -- Either add the section from the block to the common sections
      Nothing           -> DescAccum cd {cdSections = cdSections'} solos

      -- Or add it the SoloDesc for each name
      (Just productIds) -> DescAccum cd $ addSectionToAll productIds
  where
    section' = Map.singleton _lbLabel _lbText
    cdSections' = cdSections <> section'

    addSection Nothing                 = Just $ SoloDesc Nothing $ section'
    addSection (Just (SoloDesc sd ss)) = Just $ SoloDesc sd (ss <> section')

addLitDesc (DescAccum cd@CommonDesc {..} solos) (Short sd@ShortDesc {..}) =
  let
    solos' = Map.alter addSection _sdInternalName solos
    cdLinks' = mergeLinks cdLinks _sdLinks
  in
    DescAccum cd {cdLinks = cdLinks'} solos'
  where
    -- Update the common links if possible
    mergeLinks (Just x) (Just y) = Just (x <> y)
    mergeLinks x y               = x <|> y

    -- Add the short desc to the appropriate SoloDesc
    addSection Nothing                = Just $ SoloDesc (Just sd) Map.empty
    addSection (Just (SoloDesc _ ss)) = Just $ SoloDesc (Just sd) ss

-- | The basename of the path to store the encoded 'FullDesc'.
fullDescBasename :: FullDesc -> FilePath
fullDescBasename = mkBasename "-descs.yaml" .  _fdInternalName

-- | A 'FullDesc' contains contains all relevant information about the product.
data FullDesc = FullDesc
  { _fdInternalName  :: Text
  , _fdProductName   :: Text
  , _fdDescription   :: Maybe Text
  , _fdLinks         :: Maybe [Text]
  , _fdOverview      :: Maybe Text
  , _fdOtherSections :: Map Label Text
  } deriving (Show, Generic)

instance FromJSON FullDesc where
  parseJSON = genericParseJSON drop3Options

instance ToJSON FullDesc where
  toJSON = genericToJSON drop3Options
  toEncoding = genericToEncoding drop3Options

-- | Unfolds a 'DescAccum' into a list of 'FullDesc'.
asFullDescs :: DescAccum -> [FullDesc]
asFullDescs (DescAccum CommonDesc {..} solos) =
  -- drop any productId where there is no ShortDesc; TODO log the dropped productIds
  catMaybes $ map (convert . snd) $ Map.toList solos
  where
    convert (SoloDesc Nothing _) = Nothing
    convert (SoloDesc (Just ShortDesc {..}) sections) =
      let sections' = Map.unionWith (\x _ -> x) sections cdSections
          filterOthers = Map.filterWithKey others
          others k _ = k /= "Description" && k /= "Overview"
      in
        Just FullDesc
        { _fdInternalName = _sdInternalName
        , _fdProductName = _sdProductName
        , _fdDescription = Map.lookup "Description" sections'
        , _fdLinks = cdLinks
        , _fdOverview = Map.lookup "Overview" sections'
        , _fdOtherSections = filterOthers sections'
        }

-- | Transform first letter of 'String' using the given function.
transformFst :: (Char -> Char) -> String -> String
transformFst _ []     = []
transformFst f (x:xs) = (f x):xs

-- | Make files basename given its extension
mkBasename :: Text -> Text -> FilePath
mkBasename ext = unpack . (<> ext) . replace "/" "-"

drop3Options :: Options
drop3Options = defaultOptions
  { fieldLabelModifier = modifyFields
  , omitNothingFields = True
  }
  where
    modifyFields = transformFst toUpper . drop 3

-- | YenAmount represents a price or cost in Japanese Yen.
--
-- All Sumikacrafts products are priced in Yen
newtype YenAmount = YenAmount
  { unYenAmount :: Int} deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Show YenAmount where
  show = (++ " JPY"). show . unYenAmount

instance Read YenAmount where
  readsPrec _ s = case (readDec s) of
    [(x, rest)] | " JPY" `isPrefixOf` rest -> [(YenAmount x, drop 4 rest)]
    _           -> []

instance ToJSON YenAmount where
  toJSON = toJSON . show
  toEncoding = toEncoding . show

readYenAmount :: Text -> Either String YenAmount
readYenAmount x =
  case (readEither $ T.unpack x) of
    Left _  -> Left "Must be an amount followed by ' JPY'"
    Right y | y < 0 -> Left "Must be a positive Yen amount"
    z       -> z

instance FromJSON YenAmount where
  parseJSON = withText "Yen Amount" $ \x -> do
    case readYenAmount x of
      Left err  -> fail err
      Right amt -> return amt

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
mkYenRates
  :: Foldable t
  => FromUSD -> t Text -> Either ParseException (Map Text Double)
mkYenRates FromUSD {_fuRates} xs =
  let chosenCurr = Map.filterWithKey (\k _ -> elem k xs) _fuRates
      sciDiv x y = (Sci.toRealFloat x :: Double) / (Sci.toRealFloat y :: Double)
  in
    case Map.lookup "JPY" _fuRates of
      Just rate -> Right $ Map.map (\x -> x `sciDiv` rate) chosenCurr
      _         -> Left $ OtherParseException $ Exc.toException NoYenRates

-- | Derive the prices in multiple currencies from the prices in Yen
mkPrices :: RealFloat a => Product -> Map k a -> Map k Scientific
mkPrices p = Map.map (\x -> Sci.fromFloatDigits $ x * (fromIntegral $ _price p))

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

instance FromJSON FromUSD where
  parseJSON = genericParseJSON currenciesOptions

instance ToJSON FromUSD where
  toJSON = genericToJSON currenciesOptions
  toEncoding = genericToEncoding currenciesOptions
