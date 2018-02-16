{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-|
Module      : Sumikac.Types.Weight
Description : A type that represents a metric weight in grammes.
Copyright   : (c) Tim Emiola, 2018
License     : AllRightsReserved
Maintainer  : sam@sumikacrafts.com
Stability   : experimental
-}
module Sumikac.Types.Weight
  (
    Weight,
    readWeight
  )
where

import           Data.Aeson
import           Data.List  (drop, isPrefixOf)
import           Data.Text  (Text)
import qualified Data.Text  as T
import           Numeric    (readFloat)
import           Text.Read  (readEither)

-- | readWeight reads a weight from it's textual representation.
readWeight :: Text -> Either String Weight
readWeight x =
  case (readEither $ T.unpack x) of
    Left _ -> Left $
      "Bad weight (" ++ show x
      ++ "). Must be an amount followed by ' g' or ' kg'"
    Right y | y < 0 -> Left "Must be a positive weight value"
    z       -> z

-- | Weight represents the metric weight of a product in grammes.
--
newtype Weight = Weight
  { unWeight :: Int} deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Read Weight where
  readsPrec _ s = case (readFloat s) of
    [(x, rest)] | " g" `isPrefixOf` rest -> [(weight x, drop 2 rest)]
    [(x, rest)] | "g" `isPrefixOf` rest -> [(weight x, drop 1 rest)]
    [(x, rest)] | " kg" `isPrefixOf` rest -> [(weight (1000 * x), drop 3 rest)]
    [(x, rest)] | "kg" `isPrefixOf` rest -> [(weight (1000 * x), drop 2 rest)]
    _           -> []

instance Show Weight where
    show = (++ " g"). show . unWeight

instance ToJSON Weight where
  toJSON = toJSON . show
  toEncoding = toEncoding . show

instance ToJSONKey Weight

instance FromJSON Weight where
  parseJSON = withText "Weight" $ \x -> do
    case readWeight x of
      Left err  -> fail err
      Right amt -> return amt

weight :: Double -> Weight
weight =  Weight . round
