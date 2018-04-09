{-# LANGUAGE RecordWildCards #-}

module Classifier (
      runClassifier
    , sortKnownIssue
    ) where

import qualified Data.Text.Lazy as LT

import           Types (Knowledge(..), KnowledgeBase)

-- | Run analysis on given line
runClassifier :: KnowledgeBase -> LT.Text -> Maybe Knowledge
runClassifier [] _ = Nothing
runClassifier (k@Knowledge{..}:xs) str =
    if kErrorText `LT.isInfixOf` str
     then Just k
     else runClassifier xs str

-- | Sort known issues by priority
sortKnownIssue :: [Knowledge] -> [Knowledge]
sortKnownIssue [] = []
sortKnownIssue (x:xs) = sortKnownIssue gteq ++ [x] ++ sortKnownIssue lt
    where
        lt = Prelude.filter (< x) xs
        gteq = Prelude.filter (>= x) xs
