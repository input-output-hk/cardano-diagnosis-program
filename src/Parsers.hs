{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parsers (
      runAnalysis
    , sortKnownIssue
    ) where

import Data.Text.Lazy

import Analysis
import Types

-- Todo: User reader
runAnalysis :: Text -> Maybe Knowledge
runAnalysis str = analyzeLine knowledgeBase str

-- Todo: Use reader
analyzeLine :: KnowledgeBase -> Text-> Maybe Knowledge
analyzeLine [] _ = Nothing
analyzeLine (k@Knowledge{..}:xs) str =
    if not (kErrorText `isInfixOf` str)
     then analyzeLine xs str
     else Just k

-- | Sort known issues by priority
sortKnownIssue :: [Knowledge] -> [Knowledge]
sortKnownIssue [] = []
sortKnownIssue (x:xs) = sortKnownIssue gteq ++ [x] ++ sortKnownIssue lt
    where
        lt = Prelude.filter (< x) xs
        gteq = Prelude.filter (>= x) xs