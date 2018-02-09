{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Sumikac.Types
  (
    LiteralDescription(..)
  , LabelledBlock(..)
  , LitDesc(..)
  , Product(..)
  , ShortDescription(..)
  , fileAndProductName
  , productBasename
  , summarizeLD
  )
where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString      (ByteString)
import           Data.ByteString.Lazy (toStrict)
import           Data.Char            (toUpper)
import qualified Data.HashMap.Strict  as HM
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Monoid          ((<>))
import           Data.Text            (Text, replace, unpack)
import qualified Data.Text            as T
import qualified Data.Yaml            as Y

import           GHC.Generics
import           System.FilePath

-- In Asuta Wan, there were 'Made by' which should have been supplier
-- In Bamboo_vase, there is an OriginalName; I'm not sure why
-- Chopsticks_and_Soap_Rest, there is ManyDimensions, that is just not handled

-- | Gets the basename of a file that content relating the product should be saved
productBasename :: Product -> FilePath
productBasename = unpack . normalize . _internalName
  where
      normalize = (<> ".yaml") . replace "/" "-"

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
  , _productName         :: Text
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
  where modifyFields = transformFst toUpper . drop 1

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

drop3Options :: Options
drop3Options = defaultOptions
  { fieldLabelModifier = modifyFields
  , omitNothingFields = True
  }
  where modifyFields = transformFst toUpper . drop 3

namedDimensionsToJSON :: NamedDimensions -> Value
namedDimensionsToJSON = toJSON . HM.fromList . asList
  where
    asList = map toKeyValue . unNamedDimensions
    toKeyValue (NamedDimension n v) = (n,  toJSON v)

-- | Gets the file name and content for saving a product to a file
fileAndProductName
  :: FilePath -- ^ the path of the directory in which to save the product
  -> LiteralDescription  -- ^ the product to save
  -> (FilePath, Text)
fileAndProductName dir LiteralDescription{..} = (fullName, content) where
  mkName n = dir </> (unpack $  (normalize n))
  fullName = maybe "" mkName $ _ldInternalName
  content = maybe "" id $ _ldProductName
  normalize = (<> ".yaml") . replace "/" "-"

-- | A 'LiteralDescription' is the literal form of the product descriptions
-- in the description Yaml files.
--
-- The form is compact; descriptions for multiple products from a given
-- manufacturer often use the same text, so the files are organized so that
-- entries for products can share descriptions - any text that is shared is only
-- written once.
--
-- This means that often the literal entries in a given file may be related to
-- each other.
data LiteralDescription = LiteralDescription
  { _ldInternalName :: Maybe Text
  , _ldLabel        :: Maybe Text
  , _ldProductName  :: Maybe Text
  , _ldText         :: Maybe Text
  , _ldLinks        :: Maybe [Text]
  , _idShownBy      :: Maybe [Text]
  } deriving (Show, Generic)

instance FromJSON LiteralDescription where
  parseJSON = genericParseJSON drop3Options

instance ToJSON LiteralDescription where
  toJSON = genericToJSON drop3Options
  toEncoding = genericToEncoding drop3Options

-- | A 'FullDescription' is the form of a product's description
-- that contains all relevant information about the product.
--
-- During parsing, these are generated from 'LiteralDescriptions'
data FullDescription = FullDescription
  { _fdInternalName :: Text
  , _fdProductName  :: Text
  , _fdDescription  :: Text
  , _fdOverview     :: Maybe Text
  , _fdUsage        :: Maybe Text
  , _fdCare         :: Maybe Text
  , _fdLinks        :: Maybe [Text]
  } deriving (Show, Generic)

instance ToJSON FullDescription where
  toJSON = genericToJSON drop3Options

-- | SomeDescriptions holds the state obtained from a sequence of related
-- 'LiteralDescription' in order to derive the appropriate FullDescriptions
-- from the sequence
data SomeDescriptions = SomeDescriptions
  { sdLinks        :: Maybe [Text]
  , sdDescriptions :: Map Text FullDescription
  }

-- | Short Descriptions represent the most compact form
--
-- They contain the minimal description of the product, and any links that might
-- occur in its paragraphs
data ShortDescription = ShortDescription
  { _sdInternalName :: Text
  , _sdProductName  :: Text
  , _sdLinks        :: Maybe [Text]
  } deriving (Show, Generic)

instance FromJSON ShortDescription where
  parseJSON = genericParseJSON drop3Options

instance ToJSON ShortDescription where
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
summarizeLD
  :: LitDesc  -- ^ the product to save
  -> (FilePath, Text)
summarizeLD (Block LabelledBlock{..}) = ("Label", _lbLabel)
summarizeLD (Short ShortDescription{..}) = (fullName, _sdProductName) where
  fullName = (unpack . normalize) _sdInternalName
  normalize = (<> ".yaml") . replace "/" "-"

-- | LitDesc is an alternative to LiteralDescription that will replace it if
-- tests OK
--
-- It acknowledges that there are in fact two distinct data formats present in the literal yaml streams, and attempts to parse these directly
data LitDesc
  = Block LabelledBlock
  | Short ShortDescription
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

-- | Transform first letter of 'String' using the function given.
transformFst :: (Char -> Char) -> String -> String
transformFst _ []     = []
transformFst f (x:xs) = (f x):xs
