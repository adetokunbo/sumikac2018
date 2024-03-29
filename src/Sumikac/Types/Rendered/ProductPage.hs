{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Sumikac.Types.Rendered.ProductPage
Description : Models used to populate the product page template
Copyright   : (c) Tim Emiola, 2018
License     : None
Maintainer  : sam@sumikacrafts.com
Stability   : experimental
-}
module Sumikac.Types.Rendered.ProductPage
  ( ProductLayoutPage
  , mkProductLayoutPage
  , productPageBasename
  , renderProduct
  )

where

import           Data.List.NonEmpty            (NonEmpty (..))
import           Data.Text                     (Text)
import           Data.Text.Encoding.Error
import qualified Data.Text.Lazy                as LT

import           Data.Aeson
import           Data.Aeson.Casing
import           Lens.Micro.Platform           ((^.))
import           Text.Mustache

import           GHC.Generics

import           Path.Default
import           Sumikac.Types.Product
import           Sumikac.Types.Rendered.Common

-- | Combines the base page data with information specific to a product.
data ProductLayoutPage = ProductLayoutPage
  { _clpBase :: BasePage
  , _clpSelf :: FullProduct
  } deriving (Show, Generic)

instance ToJSON ProductLayoutPage where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

-- | Make the model used to render a product layout page.
mkProductLayoutPage
  :: NonEmpty Text      -- ^ the available categories
  -> NonEmpty Text      -- ^ the available currencies
  -> FullProduct        -- ^ the product
  -> Either UnicodeException ProductLayoutPage  -- ^ the full category page
mkProductLayoutPage theCats theCurrs prod = do
  let withCats = updateCategoryList defaultBase Nothing theCats
  withJsonCurr' <- updateJsonText (asJson . currencies) withCats theCurrs
  withTheProd' <- updateJsonText (asJson . productObj) withJsonCurr' prod
  return $ ProductLayoutPage
    { _clpSelf = prod
    , _clpBase = withTheProd'
    }

-- | The basename of the path to store the encoded 'ProductLayoutPage'.
productPageBasename :: ProductLayoutPage-> FilePath
productPageBasename page = mkBasename ".html" name
  where
    name = (fp ^. core . internalName)
    fp = _clpSelf page

-- | Render a ProductLayoutPage using a "Template".
renderProduct :: Template -> ProductLayoutPage -> Text
renderProduct t page = LT.toStrict $ renderMustache t $ toJSON page
