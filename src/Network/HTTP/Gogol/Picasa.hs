{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-|
Module      : Network.HTTP.Gogol.Picasa
Description : Accesses Picasa Web Albums.
Copyright   : (c) Tim Emiola, 2018
License     : AllRightsReserved
Maintainer  : sam@sumikacrafts.com
Stability   : experimental
-}
module Network.HTTP.Gogol.Picasa
  ( main
  , newDownloadEnv
  , mkDownloadPipe
  , DownloadEnv
  )
where

import           Data.ByteString.Lazy         (ByteString, toStrict)
import           Data.Monoid
import           Data.Proxy
import           Data.Text                    (Text, intercalate, pack, unpack)
import           Data.Text.Encoding           (encodeUtf8)
import           System.Directory
import           System.FilePath              (takeDirectory, (</>))

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource

import           Control.Lens                 ((.~), (<&>))
import           Data.Yaml                    (encode)
import           Formatting                   (Format, sformat, (%))
import           Formatting.Formatters        (stext)
import           Network.URI

import           Network.Google.Auth          (AllowScopes, authorize)
import           Network.Google.Env           (Env (..), envScopes, newEnv)

import           Data.Conduit
import qualified Data.Conduit.Combinators     as CC
import qualified Network.HTTP.Conduit         as HC


import           Sumikac.Types.Picasa

-- | Performs a download of the Web Album.
main
  :: (MonadBaseControl IO m, MonadCatch m, MonadIO m)
  => FilePath  -- ^ The directory where the downloaded info is stored
  -> Text      -- ^ The username whose images are downloaded
  -> m ()
main dir user = newDownloadEnv dir user >>= runConduitRes . mkDownloadPipe

-- | Contains the specification for a WebAlbum download.
data DownloadEnv a = DownloadEnv
  { env  :: Env a    -- ^ Used to obtain Google authorization credentials
  , dir  :: FilePath -- ^ The directory where the downloaded info is stored
  , user :: Text    -- ^ The username whose images are downloaded
  }

-- | Creates the specification of a Web Album download.
newDownloadEnv
  :: (MonadIO m, MonadCatch m)
  => FilePath  -- ^ The directory where the downloaded info is stored
  -> Text      -- ^ The username whose images are downloaded
  -> m (DownloadEnv '["https://picasaweb.google.com/data/"])
newDownloadEnv dir user = do
  env <- newEnv <&> (envScopes .~ picasaScope)
  return $ DownloadEnv env dir user

-- | Constructs the conduit that performs the Web Album download.
mkDownloadPipe
  :: (AllowScopes b, MonadCatch m, MonadResource m, MonadBaseControl IO m)
  => DownloadEnv b -- ^ the specification of the download
  -> ConduitM a c m ()
mkDownloadPipe dEnv = do
  let DownloadEnv{..} = dEnv
      Env{..} = env
      saveWebImages = mkSaveWebImagesPipe dEnv
  req <- authorize (mkAlbumRequest user) _envStore _envLogger _envManager
  resp <- HC.httpLbs req _envManager
  yield (HC.responseBody resp) .| decodeAlbum .| saveWebImages

decodeAlbum :: (MonadThrow m) => ConduitM ByteString WebAlbum m ()
decodeAlbum = awaitForever $ \b -> do
  case decodeWebAlbums b of
    Left e    -> throwM e
    Right aes -> CC.yieldMany $ filter isSumikaCrafts aes

mkSaveWebImagesPipe
  :: (AllowScopes a, MonadBaseControl IO m, MonadResource m, MonadCatch m)
  => DownloadEnv a
  -> ConduitM WebAlbum o m ()
mkSaveWebImagesPipe dEnv = do
  let DownloadEnv{..} = dEnv
      Env{..} = env
  awaitForever $ \ae -> do
    let outPath = dir </> webAlbumBasename ae
        withPath x = (outPath, x)
        req = mkPhotoRequest user $ _aeId ae
    req' <- authorize req _envStore _envLogger _envManager
    resp <- HC.httpLbs req' _envManager
    yield (HC.responseBody resp) .| CC.map withPath .| saveWebImagesAsYaml

saveWebImagesAsYaml
  :: (MonadIO m, MonadResource m)
  => ConduitM (FilePath, ByteString) o m ()
saveWebImagesAsYaml = awaitForever $ \(outPath, b) -> do
  case decodeImageGroups b of
    Left e -> throwM e
    Right images -> do
      liftIO $ createDirectoryIfMissing True $ takeDirectory outPath
      yield (encode images) .| CC.sinkFile outPath

-- | Format of the user album URI.
userAlbumUriFmt :: Format r (Text -> Text -> r)
userAlbumUriFmt =
  "/data/feed/api/user/"
  % stext
  % "?kind=album&imgmax=1024&alt=json&"
  % stext

mkUserAlbumUri :: Text -> Text
mkUserAlbumUri x = sformat userAlbumUriFmt x $ mkFieldsPart userAlbumFields

userAlbumFields :: Text
userAlbumFields =
  "entry("
  <> intercalate ","
  [ "summary"
  , "title"
  , "gphoto:name"
  , "gphoto:id"
  , "gphoto:timestamp"
  , "gphoto:numphotos"
  ]  <> ")"

mkAlbumRequest :: Text -> HC.Request
mkAlbumRequest user = HC.defaultRequest
  { HC.host = toStrict picasaHost
  , HC.port = 443
  , HC.secure = True
  , HC.path = encodeUtf8 $ mkUserAlbumUri user
  }

-- | Format of the user photo URI.
userPhotoUriFmt :: Format r (Text -> Text -> Text -> r)
userPhotoUriFmt =
  "/data/feed/api/user/"
  % stext
  % "/albumid/"
  % stext
  % "?kind=photo&imgmax=1024&alt=json&"
  % stext

userPhotoFields :: Text
userPhotoFields =
  "entry("
  <> intercalate ","
  [ "media:group(media:content[@medium='image']"
  , "media:thumbnail"
  , "media:description)"
  ]
  <> ")"

mkUserPhotoUri :: Text -> Text -> Text
mkUserPhotoUri x y = sformat userPhotoUriFmt x y $ mkFieldsPart userPhotoFields

mkPhotoRequest :: Text -> Text -> HC.Request
mkPhotoRequest user albumId = HC.defaultRequest
  { HC.host = toStrict picasaHost
  , HC.port = 443
  , HC.secure = True
  , HC.path = encodeUtf8 $ mkUserPhotoUri user albumId
  }

picasaHost :: ByteString
picasaHost = "picasaweb.google.com"

-- | The OAuth2 scope for accessing Picasa photos.
picasaScope :: Proxy '["https://picasaweb.google.com/data/"]
picasaScope = Proxy;

mkFieldsPart :: Text -> Text
mkFieldsPart = pack . (escapeURIString isUnescapedInURIComponent) . unpack
