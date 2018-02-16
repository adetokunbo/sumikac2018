{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-|
Module      : Sumikac.Types.YenAmount
Description : Type that represents an amount in Japanese Yen
Copyright   : (c) Tim Emiola, 2018
License     : AllRightsReserved
Maintainer  : sam@sumikacrafts.com
Stability   : experimental
-}
module Sumikac.Types.YenAmount
  (
    YenAmount,
    readYenAmount
  )

where

import           Data.Aeson
import           Data.List        (drop, isPrefixOf)
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Numeric          (readDec)
import           Text.Read        (readEither)

-- | YenAmount represents a price or cost in Japanese Yen.
--
-- All Sumikacrafts products are priced in Yen
newtype YenAmount = YenAmount
  { unYenAmount :: Int} deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Show YenAmount where
  show = (++ " JPY"). show . unYenAmount

instance Read YenAmount where
  readsPrec _ s = case (readDec s) of
    [(x, rest)] | " JPY" `isPrefixOf` rest -> [(YenAmount x, drop 4 rest)]
    _           -> []

instance ToJSON YenAmount where
  toJSON = toJSON . show
  toEncoding = toEncoding . show

instance FromJSON YenAmount where
  parseJSON = withText "Yen Amount" $ \x -> do
    case readYenAmount x of
      Left err  -> fail err
      Right amt -> return amt

readYenAmount :: Text -> Either String YenAmount
readYenAmount x =
  case (readEither $ T.unpack x) of
    Left _  -> Left "Must be an amount followed by ' JPY'"
    Right y | y < 0 -> Left "Must be a positive Yen amount"
    z       -> z
