{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-|
Module      : Sumikac.Types.Description
Description : Types that model the description of products.
Copyright   : (c) Tim Emiola, 2018
License     : None
Maintainer  : sam@sumikacrafts.com
Stability   : experimental
-}
module Sumikac.Types.Description
  (
    -- * Product description
  DescAccum(..)
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

import           Data.Char           (toUpper)
import           Data.Foldable       (foldl')
import           Data.List           (drop)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Maybe
import           Data.Monoid         ((<>))
import           Data.Text           (Text)

import           Data.Aeson

import           GHC.Generics

import           Path.Default


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

drop3Options :: Options
drop3Options = defaultOptions
  { fieldLabelModifier = modifyFields
  , omitNothingFields = True
  }
  where
    modifyFields = transformFst toUpper . drop 3
    transformFst _ []     = []
    transformFst f (x:xs) = (f x):xs
