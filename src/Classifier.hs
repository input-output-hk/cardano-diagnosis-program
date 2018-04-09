{-# LANGUAGE RecordWildCards #-}

module Classifier (
      runClassifiers
    , sortKnownIssue
    ) where

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding       as LT

import Data.ByteString.Lazy as LBS

import           Types (Knowledge(..), KnowledgeBase)

-- | Run analysis on given file
runClassifiers :: KnowledgeBase -> LBS.ByteString -> [Maybe Knowledge]
runClassifiers kbase logfile = 
    let eachLine = LT.lines $ LT.decodeUtf8 logfile -- this is an array which is way too slow
    in Prelude.map (analyzeLine kbase) eachLine

-- | Run analysis on given line
analyzeLine :: KnowledgeBase -> LT.Text -> Maybe Knowledge
analyzeLine [] _ = Nothing
analyzeLine (k@Knowledge{..}:xs) str =
    if kErrorText `LT.isInfixOf` str
     then Just k
     else analyzeLine xs str

-- | Sort known issues by priority
sortKnownIssue :: [Knowledge] -> [Knowledge]
sortKnownIssue [] = []
sortKnownIssue (x:xs) = sortKnownIssue gteq ++ [x] ++ sortKnownIssue lt
    where
        lt = Prelude.filter (< x) xs
        gteq = Prelude.filter (>= x) xs
