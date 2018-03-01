{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-|
Module      : Sumikac.Types.Rendered.CategoryPage
Description : Models shared by multiple page templates
Copyright   : (c) Tim Emiola, 2018
License     : None
Maintainer  : sam@sumikacrafts.com
Stability   : experimental
-}
module Sumikac.Types.Rendered.Common
  ( BasePage
  , JsonObjects
  , defaultBase
  , updateCategoryList
  , updateJsonText

  -- * Lenses into 'JsonObjects'
  , currencies
  , productObj

  -- * Lenses into 'BasePage'
  , asJson
  )
where

import qualified Data.ByteString.Lazy     as LBS
import           Data.List.NonEmpty       (NonEmpty (..))
import qualified Data.List.NonEmpty       as NonEmpty
import           Data.Monoid              ((<>))
import           Data.Text                (Text)
import           Data.Text.Encoding
import           Data.Text.Encoding.Error

import           Data.Aeson
import           Data.Aeson.Casing
import           Lens.Micro.Platform

import           GHC.Generics

-- | Fields used in the document head.
data Head = Head
  { _hdTitlePrefix   :: Text
  , _hdTitleUri      :: Text
  , _hdTitleMetaDesc :: Text
  } deriving (Show, Generic)

makeLensesWith abbreviatedFields ''Head

instance ToJSON Head where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

-- | Fields that represent the text of JSON objects to be placed in script tags
data JsonObjects = JsonObjects
  { _joCurrencies :: Maybe Text
  , _joCategories :: Maybe Text
  , _joProductObj :: Maybe Text
  } deriving (Show, Generic)

makeLensesWith abbreviatedFields ''JsonObjects

-- | Use a lens to update (Maybe Text) value with the json encoding of 'ToJSON'
updateJsonText ::
  ToJSON js =>
  ASetter s t a (Maybe Text) -> s -> js -> Either UnicodeException t
updateJsonText aLens jo jsonObj = do
  t <- asText jsonObj
  return $ jo & aLens ?~ t

asText :: (ToJSON a) => a -> Either UnicodeException Text
asText = decodeUtf8' . LBS.toStrict . encode

instance ToJSON JsonObjects where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

-- | Fields used in the document footer.
data Footer = Footer
  { _ftCopyrightYear   :: Text
  } deriving (Show, Generic)

instance ToJSON Footer where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

-- | The list items that display the available categories.
data CategoryListItem = CategoryListItem
  { _cvName      :: Text
  , _cvUri       :: Text
  , _cvIsCurrent :: Bool
  } deriving (Show, Generic)

instance ToJSON CategoryListItem where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

-- | A model containing template fields common to all pages.
data BasePage = BasePage
  { _bpHtmlHead      :: Head
  , bpFooter         :: Footer
  , _bpAsJson        :: JsonObjects
  , _bpCategories    :: NonEmpty CategoryListItem
  , bpTheDomainName  :: Text
  , bpTheCompanyName :: Text
  } deriving (Show, Generic)

makeLensesWith abbreviatedFields ''BasePage

instance ToJSON BasePage where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

theDomain, uriPrefix :: Text
theDomain = "www.sumikacrafts.com"
uriPrefix = "https://" <> theDomain

-- | An instance of BasePage with reasonable default values.
defaultBase :: BasePage
defaultBase = BasePage
  { _bpHtmlHead = Head
    { _hdTitlePrefix = "SumikaCrafts"
    , _hdTitleMetaDesc =
      "SumikaCrafts Japanese Home Decor - Simple, Beautiful, Refined"
    , _hdTitleUri = "https://www.sumikacrafts.com"
    }
  ,bpFooter = Footer
    { _ftCopyrightYear = "2018"
    }
  , _bpCategories = CategoryListItem
    { _cvName = "Boxes"
    , _cvIsCurrent = False
    , _cvUri = "/categories/boxes.html"
    } :| []
  , bpTheDomainName = theDomain
  , bpTheCompanyName = "SumikaCrafts"
  , _bpAsJson = JsonObjects
    { _joCurrencies = Nothing
    , _joCategories = Nothing
    , _joProductObj = Nothing
    }
  }

-- | Update the base model to indicate the availble categories and maybe the
-- selected category.
updateCategoryList
  :: BasePage -> Maybe Text -> NonEmpty Text -> BasePage
updateCategoryList page current allKnown =
  page
  & categories .~ items
  & htmlHead . titlePrefix .~ tp
  & htmlHead . titleUri .~ tu
  & htmlHead . titleMetaDesc .~ md
  where
    md =  maybe (hHead ^. titleMetaDesc) ((<>) "SumikaCrafts ") current
    tu = mkFullUri current
    tp = maybe (hHead ^. titlePrefix) ((<>) " | ") current
    items = NonEmpty.map mkLi allKnown
    mkLi name = CategoryListItem
      { _cvName = name
      , _cvIsCurrent = maybe False (name ==) current
      , _cvUri = "/categories/" <> name <> ".html"
      }
    mkFullUri = maybe (hHead ^. titleUri) $ ((<>) uriPrefix) . mkUri
    mkUri name = "/categories/" <> name <> ".html"
    hHead = page ^. htmlHead
