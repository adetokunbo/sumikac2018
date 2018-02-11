{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Sumikac.Types
  (
    -- Products
    FullProduct(..)
  , Product(..)
  , fullProductBasename
  , productBasename

    -- Description
  , DescAccum(..)
  , FullDesc(..)
  , LitDesc(..)
  , LabelledBlock(..)
  , ShortDesc(..)
  , addLitDesc
  , asFullDescs
  , descAccum
  , fullDescBasename
  , summarizeLD
  )
where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString      (ByteString)
import           Data.ByteString.Lazy (toStrict)
import           Data.Char            (toUpper)
import           Data.Foldable        (foldl')
import qualified Data.HashMap.Strict  as HM
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Maybe
import           Data.Monoid          ((<>))
import           Data.Text            (Text, replace, unpack)
import qualified Data.Text            as T
import qualified Data.Yaml            as Y

import           GHC.Generics
import           System.FilePath

-- In Asuta Wan, there were 'Made by' which should have been supplier
-- In Bamboo_vase, there is an OriginalName; I'm not sure why

-- | The basename of the path to store the encoded 'FullProduct'
fullProductBasename :: FullProduct-> FilePath
fullProductBasename =
  mkBasename "-complete.yaml" .  _fdInternalName . _fpFullDesc

-- | FullProduct provies the information about a given product from different
-- sources in a single
data FullProduct = FullProduct
  { _fpProduct :: Product
  , _fpFullDesc :: FullDesc
  } deriving (Show, Generic)

instance FromJSON FullProduct where
  parseJSON = genericParseJSON drop3Options

instance ToJSON FullProduct where
  toJSON = genericToJSON drop3Options
  toEncoding = genericToEncoding drop3Options

-- | The basename of the path to store the encoded 'Product'
productBasename :: Product -> FilePath
productBasename = mkBasename ".yaml" . _internalName

-- A 'Product' is the core item that the sumikacrafts website gives access to
-- the public
data Product = Product
  { _internalName        :: Text
  , _capacity            :: Maybe Text
  , _categories          :: [Text]
  , _colours             :: Maybe [Text]
  , _cost                :: Text -- a currency quantity
  , _dimensions          :: Maybe Text
  , _manyDimensions      :: Maybe NamedDimensions
  , _expectedShippingFee :: Maybe Text -- a currency quantity
  , _madeIn              :: Maybe Text
  , _materials           :: Maybe [Text]
  , _maxItems            :: Maybe Int
  , _originalName        :: Maybe Text
  , _patterns            :: Maybe [Text]
  , _price               :: Text -- a currency quantity
  , _setSizes            :: Maybe [Int]
  , _shape               :: Maybe [Text]
  , _supplier            :: Maybe Text
  , _weight              :: Maybe Text
  , _weightAfterWrapping :: Maybe Text
  } deriving (Show, Generic)

productOptions :: Options
productOptions = defaultOptions
  { fieldLabelModifier = modifyFields
  , omitNothingFields = True
  }
  where
    modifyFields = transformFst toUpper . drop 1

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
-- One is a short description, the other a block of text with a label
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

-- | Short Descriptions are a minimal description of the product, along with any
-- links that might occur in its paragraphs
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

-- | Summarizes the contents of a 'LitDesc'
summarizeLD :: LitDesc -> (FilePath, Text)
summarizeLD (Block LabelledBlock{..}) = ("Label", _lbLabel)
summarizeLD (Short ShortDesc{..}) = (fullName, _sdProductName)
  where
    fullName = (unpack . normalize) _sdInternalName
    normalize = (<> ".yaml") . replace "/" "-"

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

-- | SoloDesc contains description data that is specific to a product
data SoloDesc = SoloDesc (Maybe ShortDesc) Sections

-- | DescAccum contains both the shared description and all the solo product descriptions.
-- DescAccum is used to produce a sequence of
data DescAccum = DescAccum CommonDesc (Map ProductId SoloDesc)

-- | A descAccum with nothing added to it
descAccum :: DescAccum
descAccum = DescAccum CommonDesc{cdLinks=Nothing, cdSections=Map.empty} Map.empty

-- | Add a 'LitDesc' to a 'DescAccum'
addLitDesc :: DescAccum -> LitDesc -> DescAccum

addLitDesc (DescAccum cd@CommonDesc {..} solos) (Block LabelledBlock {..}) =
  case _lbShownBy of
    -- Either add the section from the block to the common sections
    Nothing      -> DescAccum cd {cdSections = cdSections'} solos

    -- Or add it the SoloDesc for each name
    (Just names) -> DescAccum cd $ foldMap (Map.alter addSection) names solos
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

-- | The basename of the path to store the encoded 'FullDesc'
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

asFullDescs :: DescAccum -> [FullDesc]
asFullDescs (DescAccum CommonDesc {..} solos) =
  -- drop any productId where there is no ShortDesc; TODO log the dropped productIds
  catMaybes $ map (convert . snd) $ Map.toList solos
  where
    convert (SoloDesc Nothing _) = Nothing
    convert (SoloDesc (Just ShortDesc {..}) sections) =
      let sections' = Map.unionWith (\x y -> x) sections cdSections
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

-- | Transform first letter of 'String' using the function given.
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
