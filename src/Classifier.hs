{-# LANGUAGE RecordWildCards #-}

module Classifier (
      runClassifiers
    , extractIssuesFromLogs
    ) where

import Data.List (sort)

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding       as LT

import Data.ByteString.Lazy as LBS

import Data.Vector (Vector)
import qualified Data.Vector as V

import           Types (Knowledge(..), KnowledgeBase)

extractIssuesFromLogs :: KnowledgeBase -> [LBS.ByteString] -> [Knowledge]
extractIssuesFromLogs kbase logs = 
    let extractedKnownErrors = V.concat $ Prelude.map (runClassifiers kbase) logs
    in sort $ V.toList $ vNub extractedKnownErrors

-- | Run analysis on given file
runClassifiers :: KnowledgeBase -> LBS.ByteString -> Vector Knowledge
runClassifiers kbase logfile = 
    let eachLine = V.fromList $ LT.lines $ LT.decodeUtf8 logfile -- this is an array which is way too slow
    in filterMaybe $ V.map (analyzeLine kbase) eachLine

-- | Run analysis on given line
analyzeLine :: KnowledgeBase -> LT.Text -> Maybe Knowledge
analyzeLine [] _ = Nothing
analyzeLine (k@Knowledge{..}:xs) str =
    if kErrorText `LT.isInfixOf` str
     then Just k
     else analyzeLine xs str

-- | Filter out Nothing from list of Maybe a
filterMaybe :: Vector (Maybe a) -> Vector a
filterMaybe = V.foldr (\x acc -> case x of
                          Nothing -> acc
                          Just ele -> ele `V.cons` acc) V.empty

-- | Vector version of nub
vNub :: (Eq a) => Vector a -> Vector a
vNub = V.foldr (\x acc -> if x `V.notElem` acc
                              then x `V.cons` acc
                              else acc) V.empty