{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Sumikac.Types.Picasa
Description : Types that represent components of the SumikaCrafts website
              derived from Picasa Web Albums.
Copyright   : (c) Tim Emiola, 2018
License     : None
Maintainer  : sam@sumikacrafts.com
Stability   : experimental
-}
module Sumikac.Types.Picasa
    (
      AlbumEntry(..)
    , ImageGroup(..)
    , ProductImages(..)
    , RawAlbums(..)
    , RawAlbumEntry(..)
    , RawImageGroups(..)
    , RawImageGroup(..)
    ) where


import           Data.Char    (toLower, toUpper)
import           Data.List    (drop)
import           Data.Text    (Text)

import           Data.Aeson

import           GHC.Generics

-- | All the images found on Picasa album tagged with the product's internal
-- name.
data ProductImages= ProductImages
  { _piInternalName :: Text
  , _piImages       :: [ImageGroup]
  } deriving (Show, Generic)

instance FromJSON ProductImages where
  parseJSON = genericParseJSON drop3Options

instance ToJSON ProductImages where
  toJSON = genericToJSON drop3Options
  toEncoding = genericToEncoding drop3Options

-- | An entry from a Picasa web album.
data AlbumEntry = AlbumEntry
  { _aeTitle   :: Text
  , _aeSummary :: Text
  , _aeId      :: Text
  , _aeName    :: Text
  } deriving (Show, Generic)

instance FromJSON AlbumEntry where
  parseJSON = genericParseJSON drop3Options

instance ToJSON AlbumEntry where
  toJSON = genericToJSON drop3Options
  toEncoding = genericToEncoding drop3Options

-- | Same as 'AlbumEntry' wrapped in a newtype to allow it parsed using the more
-- complex parser required for the Picasa API json output.
newtype RawAlbumEntry = RawAlbumEntry {unAlbumEntry :: AlbumEntry}
  deriving (Show)

instance FromJSON RawAlbumEntry where
  parseJSON = withObject "entry" $ \o -> do
    rawTitle <- o .: "title"
    _aeTitle <- rawTitle .: "$t"
    rawSummary <- o .: "summary"
    _aeSummary <- rawSummary .: "$t"
    rawId <- o .: "gphoto$id"
    _aeId <- rawId .: "$t"
    rawName <- o .: "gphoto$name"
    _aeName <- rawName .: "$t"
    return $ RawAlbumEntry $ AlbumEntry _aeTitle _aeSummary _aeId _aeName

-- | A collection of RawAlbums parsed from the Picasa API json.
data RawAlbums = RawAlbums [RawAlbumEntry]
  deriving (Show)

instance FromJSON RawAlbums where
  parseJSON = withObject "picasa gdata" $ \o -> do
    feed <- o .: "feed"
    entries <- feed .: "entry"
    return $ RawAlbums entries
-- | The description of a direct image link accessible from Picasa web albums.
data WebImage = WebImage
  { _wiUrl    :: Text
  , _wiHeight :: Int
  , _wiWidth  :: Int
  } deriving (Show, Generic)

instance FromJSON WebImage where
  parseJSON = genericParseJSON webImageOptions

instance ToJSON WebImage where
  toJSON = genericToJSON webImageOptions
  toEncoding = genericToEncoding webImageOptions

-- | A group of 'WebImage's related to each single photo in a Picasa album.
data ImageGroup = ImageGroup
  { _poContent   :: [WebImage]
  , _poThumbnail :: [WebImage]
  } deriving (Show, Generic)

instance FromJSON ImageGroup where
  parseJSON = genericParseJSON drop3Options

instance ToJSON ImageGroup where
  toJSON = genericToJSON drop3Options
  toEncoding = genericToEncoding drop3Options

-- | Same as 'ImageGroup' wrapped in a newtype to allow it to be parsed using
-- directl from the complex Picasa API json.
newtype RawImageGroup = RawImageGroup {unImageGroup :: ImageGroup}
  deriving (Show)

instance FromJSON RawImageGroup where
  parseJSON = withObject "entry" $ \o -> do
    top <- o .: "media$group"
    _poContent <- top .: "media$content"
    _poThumbnail <- top .: "media$thumbnail"
    return $ RawImageGroup $ ImageGroup _poContent _poThumbnail

-- | A list of 'RawImageGroup' parsed directly from Picasa API JSON.
data RawImageGroups = RawImageGroups [RawImageGroup]
  deriving (Show)

instance FromJSON RawImageGroups where
  parseJSON = withObject "picasa photo gdata" $ \o -> do
    feed <- o .: "feed"
    entries <- feed .: "entry"
    return $ RawImageGroups entries

drop3Options :: Options
drop3Options = defaultOptions
  { fieldLabelModifier = modifyFields
  , omitNothingFields = True
  }
  where
    modifyFields = transformFst toUpper . drop 3

webImageOptions :: Options
webImageOptions = defaultOptions
  { fieldLabelModifier = modifyFields
  , omitNothingFields = True
  }
  where
    modifyFields = transformFst toLower . drop 3

-- | Transform first letter of 'String' using the given function.
transformFst :: (Char -> Char) -> String -> String
transformFst _ []     = []
transformFst f (x:xs) = (f x):xs
