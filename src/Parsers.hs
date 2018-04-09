{-# LANGUAGE RecordWildCards #-}

module Parsers (
      runAnalysis
    , sortKnownIssue
    ) where

import qualified Data.Text.Lazy as LT

import           Types

-- | Run analysis on given line
runAnalysis :: KnowledgeBase -> LT.Text -> Maybe Knowledge
runAnalysis [] _ = Nothing
runAnalysis (k@Knowledge{..}:xs) str =
    if kErrorText `LT.isInfixOf` str
     then Just k
     else runAnalysis xs str

-- | Sort known issues by priority
sortKnownIssue :: [Knowledge] -> [Knowledge]
sortKnownIssue [] = []
sortKnownIssue (x:xs) = sortKnownIssue gteq ++ [x] ++ sortKnownIssue lt
    where
        lt = Prelude.filter (< x) xs
        gteq = Prelude.filter (>= x) xs
