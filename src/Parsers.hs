{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parsers (
      runAnalysis
    , sortKnownIssue
    ) where

import Data.Text

import Types

-- | Run analysis on given line
runAnalysis :: KnowledgeBase -> Text-> Maybe Knowledge
runAnalysis [] _ = Nothing
runAnalysis (k@Knowledge{..}:xs) str =
    if not (kErrorText `isInfixOf` str)
     then runAnalysis xs str
     else Just k

-- | Sort known issues by priority
sortKnownIssue :: [Knowledge] -> [Knowledge]
sortKnownIssue [] = []
sortKnownIssue (x:xs) = sortKnownIssue gteq ++ [x] ++ sortKnownIssue lt
    where
        lt = Prelude.filter (< x) xs
        gteq = Prelude.filter (>= x) xs