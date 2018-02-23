{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

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
      WebAlbum(..)
    , WebImage(..)
    , ImageGroup(..)
    , WebAlbumException
    , webAlbumBasename
    , decodeWebAlbums
    , decodeImageGroups
    , isSumikaCrafts

    -- * Lenses for ImageGroup
    , content
    , thumbnails
    ) where


import qualified Control.Exception    as Exc
import           Data.ByteString.Lazy (ByteString)
import           Data.Char            (toLower, toUpper)
import           Data.List            (drop)
import           Data.List.NonEmpty   (NonEmpty)
import           Data.Monoid
import           Data.Text            (Text, isPrefixOf, replace, unpack)

import           Data.Aeson
import           Lens.Micro.Platform

import           GHC.Generics

-- | Indicates that the Picasa Web JSON could not be parsed.
data WebAlbumException = WebAlbumException String

instance Show WebAlbumException where
  show (WebAlbumException s) = "Could not parse Picasa JSON: " ++ s

instance Exc.Exception WebAlbumException

-- | Decodes a list of 'WebAlbum' from a 'ByteString' containing a Picasa
-- Album JSON response.
decodeWebAlbums :: ByteString -> Either WebAlbumException [WebAlbum]
decodeWebAlbums b = case eitherDecode b of
    Left s               -> Left $ WebAlbumException s
    Right (RawAlbums xs) -> Right $ map unWebAlbum xs

-- | Filter albums that indicates where albums are SumikaCrafts image albums
isSumikaCrafts :: WebAlbum -> Bool
isSumikaCrafts = isPrefixOf scPrefix . _aeSummary

-- | The prefix of all SumikaCrafts Web Albums.
scPrefix :: Text
scPrefix = "sc/"

-- | The basename of the path to store the images from an 'WebAlbum'
webAlbumBasename :: WebAlbum -> FilePath
webAlbumBasename =
  mkBasename "-web-images.yaml"
  . replace scPrefix ""
  .  _aeSummary

-- | An entry from a Picasa web album.
data WebAlbum = WebAlbum
  { _aeTitle   :: Text
  , _aeSummary :: Text
  , _aeId      :: Text
  , _aeName    :: Text
  } deriving (Show, Generic)

instance FromJSON WebAlbum where
  parseJSON = genericParseJSON drop3Options

instance ToJSON WebAlbum where
  toJSON = genericToJSON drop3Options
  toEncoding = genericToEncoding drop3Options

-- | Same as 'WebAlbum' wrapped in a newtype to allow it parsed using the more
-- complex parser required for the Picasa API json output.
newtype RawWebAlbum = RawWebAlbum {unWebAlbum :: WebAlbum}
  deriving (Show)

instance FromJSON RawWebAlbum where
  parseJSON = withObject "entry" $ \o -> do
    rawTitle <- o .: "title"
    _aeTitle <- rawTitle .: "$t"
    rawSummary <- o .: "summary"
    _aeSummary <- rawSummary .: "$t"
    rawId <- o .: "gphoto$id"
    _aeId <- rawId .: "$t"
    rawName <- o .: "gphoto$name"
    _aeName <- rawName .: "$t"
    return $ RawWebAlbum $ WebAlbum _aeTitle _aeSummary _aeId _aeName

-- | A collection of RawAlbums parsed from the Picasa API json.
data RawAlbums = RawAlbums [RawWebAlbum]
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

-- | Make files basename given its extension.
mkBasename :: Text -> Text -> FilePath
mkBasename ext = unpack . (<> ext) . replace "/" "-"

drop3Options :: Options
drop3Options = defaultOptions
  { fieldLabelModifier = modifyFields
  , omitNothingFields = True
  }
  where
    modifyFields = transformFst toUpper . drop 3

-- | A group of 'WebImage's related to each single photo in a Picasa album.
data ImageGroup = ImageGroup
  { _poContent    :: NonEmpty WebImage
  , _poThumbnails :: NonEmpty WebImage
  } deriving (Show, Generic)

makeLensesWith abbreviatedFields ''ImageGroup

instance FromJSON ImageGroup where
  parseJSON = genericParseJSON drop3Options

instance ToJSON ImageGroup where
  toJSON = genericToJSON drop3Options
  toEncoding = genericToEncoding drop3Options

-- | Decodes a list of 'ImageGroup' from a 'ByteString' containing a Picasa
-- Photo JSON response.
decodeImageGroups :: ByteString -> Either WebAlbumException [ImageGroup]
decodeImageGroups b =  case eitherDecode b of
    Left s                      -> Left $ WebAlbumException s
    Right (RawImageGroups imgs) -> Right $ map unImageGroup imgs

-- | Same as 'ImageGroup' wrapped in a newtype to allow it to be parsed using
-- directl from the complex Picasa API json.
newtype RawImageGroup = RawImageGroup {unImageGroup :: ImageGroup}
  deriving (Show)

instance FromJSON RawImageGroup where
  parseJSON = withObject "entry" $ \o -> do
    top <- o .: "media$group"
    _poContent <- top .: "media$content"
    _poThumbnails <- top .: "media$thumbnail"
    return $ RawImageGroup $ ImageGroup _poContent _poThumbnails

-- | A list of 'RawImageGroup' parsed directly from Picasa API JSON.
data RawImageGroups = RawImageGroups [RawImageGroup]
  deriving (Show)

instance FromJSON RawImageGroups where
  parseJSON = withObject "picasa photo gdata" $ \o -> do
    feed <- o .: "feed"
    entries <- feed .: "entry"
    return $ RawImageGroups entries
