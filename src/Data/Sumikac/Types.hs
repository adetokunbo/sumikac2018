{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Sumikac.Types
  (
    LiteralDescription(..)
  ,  Product(..)
  , fileNameWithContent
  , fileAndProductName
  )
where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString      (ByteString)
import           Data.ByteString.Lazy (toStrict)
import           Data.Char            (toUpper)
import qualified Data.HashMap.Strict  as HM
import           Data.Monoid          ((<>))
import           Data.Text            (Text, replace, unpack)
import qualified Data.Text            as T
import qualified Data.Yaml            as Y

import           GHC.Generics
import           System.FilePath

-- In Asuta Wan, there were 'Made by' which should have been supplier
-- In Bamboo_vase, there is an OriginalName; I'm not sure why
-- Chopsticks_and_Soap_Rest, there is ManyDimensions, that is just not handled

-- | Gets the file name and content for saving a product to a file
fileNameWithContent
  :: FilePath -- ^ the path of the directory in which to save the product
  -> Product  -- ^ the product to save
  -> (FilePath, ByteString)
fileNameWithContent dir prod = (fullName, content) where
  fullName = dir </> (unpack $  (normalize . _internalName) prod)
  content = (toStrict . encode) prod
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
productOptions = defaultOptions { fieldLabelModifier = modifyFields }
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

namedDimensionsToJSON :: NamedDimensions -> Value
namedDimensionsToJSON = toJSON . HM.fromList . asList
  where
    asList = map toKeyValue . unNamedDimensions
    toKeyValue (NamedDimension n v) = (n,  toJSON v)

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
  , _ldLanguage     :: Maybe Text
  , _ldProductName  :: Maybe Text
  , _ldText         :: Maybe Text
  , _ldLinks        :: Maybe [Text]
  , _idShownBy      :: Maybe [Text]
  } deriving (Show, Generic)

-- | Gets the file name and content for saving a product to a file
fileAndProductName
  :: FilePath -- ^ the path of the directory in which to save the product
  -> LiteralDescription  -- ^ the product to save
  -> (FilePath, Text)
fileAndProductName dir ld = (fullName, content) where
  mkName n = dir </> (unpack $  (normalize n))
  fullName = maybe "" mkName $ _ldInternalName ld
  content = maybe "" id $ _ldProductName ld
  normalize = (<> ".yaml") . replace "/" "-"


ldOptions :: Options
ldOptions = defaultOptions { fieldLabelModifier = modifyFields }
  where modifyFields = transformFst toUpper . drop 3

instance FromJSON LiteralDescription where
  parseJSON = genericParseJSON ldOptions

instance ToJSON LiteralDescription where
  toJSON = genericToJSON ldOptions
  toEncoding = genericToEncoding ldOptions

-- | Transform first letter of 'String' using the function given.
transformFst :: (Char -> Char) -> String -> String
transformFst _ []     = []
transformFst f (x:xs) = (f x):xs
