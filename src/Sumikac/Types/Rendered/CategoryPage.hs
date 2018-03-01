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
Module      : Sumikac.Types.Rendered.CategoryPage
Description : Models used to populate the Category page template
Copyright   : (c) Tim Emiola, 2018
License     : None
Maintainer  : sam@sumikacrafts.com
Stability   : experimental
-}
module Sumikac.Types.Rendered.CategoryPage
  (
  -- * Products as used in Category Pages
  CategoryLayoutPage
  , CategoryPage
  , CategoryProduct
  , categoryId
  , categoryProduct
  , mkCategoryPage
  , mkCategoryLayoutPage
  , numColumns
  )
where

import           Data.List                     (unfoldr)
import           Data.List.NonEmpty            (NonEmpty (..))
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Text                     (Text)

import           Data.Aeson
import           Data.Aeson.Casing
import           Lens.Micro.Platform

-- Hide (to) as it conflicts with Lens.Micro.Platform
import           GHC.Generics                  hiding (to)

import           Sumikac.Types.Description
import           Sumikac.Types.Picasa
import           Sumikac.Types.Product

import           Sumikac.Types.Rendered.Common

categoryProduct
  :: Text -> FullProduct -> Maybe (CategoryProduct, NonEmpty WebImage)
categoryProduct c fp
  | c `notElem` fp ^. core . categories = Nothing
  | otherwise =
    Just (
      CategoryProduct
      { _cpInternalName = fp ^. core . internalName
      , _cpProductName = fp ^. fullDesc . productName
      , _cpGalleryImage = NonEmpty.head $ ig ^. content
      , _cpThumbnail = NonEmpty.head $ ig ^. thumbnails
      },
      images
    )
    where ig = NonEmpty.head igs
          igs = fp ^. imageGroups
          images = NonEmpty.map (^. content . to NonEmpty.head) igs

-- | The fragment of the data in a FullProduct that's displayed on a category
-- page.
data CategoryProduct = CategoryProduct
  { _cpInternalName :: Text
  , _cpProductName  :: Text
  , _cpGalleryImage :: WebImage
  , _cpThumbnail    :: WebImage
  } deriving (Show, Generic)

instance FromJSON CategoryProduct where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON CategoryProduct where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

-- | Models the rows of products on a category page.
data CategoryRow = CategoryRow
  { _crCategoryCols :: [CategoryProduct]
  } deriving (Show, Generic)

instance FromJSON CategoryRow where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON CategoryRow where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

-- | Models the contents of a category page.
data CategoryPage = CategoryPage
  { crCategoryRows  :: [CategoryRow]
  , crGalleryImages :: NonEmpty GalleryImage
  , _crCategoryId   :: Text
  } deriving (Show, Generic)

makeLensesFor [
  ("_crCategoryId", "categoryId")
  ] ''CategoryPage

instance FromJSON CategoryPage where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON CategoryPage where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

numColumns, maxGallerySize :: Int
maxGallerySize = 8 -- ^ The maxiumum number of gallery images
numColumns = 4     -- ^ The number of columns of images.

-- | Construct a 'CategoryPage' from the CategoryProducts and ImageGroups in a
-- page
mkCategoryPage :: Text -> Int -> [(CategoryProduct, NonEmpty WebImage)] -> CategoryPage
mkCategoryPage c n extracted =
  CategoryPage
  { crCategoryRows = rows
  , crGalleryImages = mkGalleryImages images
  , _crCategoryId = c
  }
  where (cps, extractedImgs) = unzip extracted
        rows = map CategoryRow $ chunks n cps
        asList = map NonEmpty.toList extractedImgs
        images = NonEmpty.fromList $ take maxGallerySize $ oneByOne asList

chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile (not.null) . unfoldr (Just . splitAt n)

-- | Unfold the items in a list of lists one by one.
oneByOne :: [[a]] -> [a]
oneByOne = concat . concatMap id . oneByOne'
  where
    oneByOne' = go . unzip . (map $ splitAt 1)
    go (heads, [])    = [nonNull heads]
    go (heads, tails) = (nonNull heads) : oneByOne' (nonNull tails)
    nonNull = filter $ not . null

-- | Combines the base page data with the information specific to categories.
data CategoryLayoutPage = CategoryLayoutPage
  { _clpBase :: BasePage
  , _clpSelf :: CategoryPage
  } deriving (Show, Generic)

instance ToJSON CategoryLayoutPage where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

mkCategoryLayoutPage
  :: NonEmpty Text       -- ^ the other categories
  -> CategoryPage        -- ^ the category page
  -> CategoryLayoutPage  -- ^ the full category page
mkCategoryLayoutPage otherCats page =
  let
    aCat = page ^. categoryId
    theBase =
      updateCategoryList defaultBasePage (Just aCat) otherCats
  in
    CategoryLayoutPage
    { _clpSelf = page
    , _clpBase = theBase
    }
