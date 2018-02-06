{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
module Data.Sumikac.Conduit
  (
    groupByDashes
  )
where

import           Control.Monad.Trans.Resource
import           Data.Conduit
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.Combinators     as CC
import qualified Data.Conduit.List            as CL
import qualified Data.Conduit.Text            as CT
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import           System.IO                    (stdout)

-- | Splits the upstream 'Text' into chunks bounded by
-- | separator lines containing "---"
groupByDashes :: Monad m => ConduitM Text Text m ()
groupByDashes = groupBySep $ \x -> x == "---"

-- | Splits the upstream 'Text' into chunks bounded by
-- | separator lines that match the predicate
groupBySep :: Monad m => (Text -> Bool) -> ConduitM Text Text m ()
groupBySep p = yieldFromJust (CT.foldLines groupBySep' Nothing)
  where
    groupBySep' acc = do
      let scanOnSep :: Maybe Text -> Text -> Maybe Text
          scanOnSep _ t | p t = Nothing
          scanOnSep Nothing t  = Just t
          scanOnSep (Just x) t = Just (x <> "\n" <> t)

      acc' <- CL.fold scanOnSep acc
      case acc' of
        Nothing -> do
          yield $ maybe "" id acc
          return acc'
        otherwise -> return acc'

yieldFromJust
  :: Monad m
  => ConduitM i o m (Maybe o)
  -> ConduitM i o m ()
yieldFromJust inner = do
  lastChunk <- inner
  CL.sinkNull
  maybe (return ()) (\chunk -> yield chunk >> return ()) lastChunk

unlinesText :: Monad m => Conduit Text m Text
unlinesText = awaitForever $ \x -> yield x >> yield "\n"

main :: IO ()
main = runResourceT
     $ CB.sourceFile "timings.log"
    $$ CT.decode CT.utf8
    =$ groupByDashes
    =$ unlinesText
    =$ CT.encode CT.utf8
    =$ CB.sinkHandle stdout
