{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parsers (
      runAnalysis
    , sortKnownIssue
    ) where

import Data.Text

import Types

-- | Run analysis on the given text
runAnalysis :: KnowledgeBase -> Text -> Maybe Knowledge
runAnalysis kb str = analyzeLine kb str

-- | Try to match the text with knowledge base
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