{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parsers (
      runAnalysis
    , sortKnownIssue
    ) where

import Data.Text.Lazy

import Analysis
import Types
import qualified Data.Map as M

-- Todo: User reader
runAnalysis :: Text -> Maybe (Behavior, Analysis)
runAnalysis str = analyzeLine (M.keys knowledgeBase) knowledgeBase str

-- Todo: Use reader
analyzeLine :: [Behavior] -> KnowledgeBase -> Text-> Maybe (Behavior, Analysis)
analyzeLine [] _  _ = Nothing
analyzeLine (b@Behavior{..}:xs) base str =
    if not (bErrorText `isInfixOf` str)
     then analyzeLine xs base str
     else case M.lookup b base of
        Just a -> Just (b, a)
        Nothing -> Nothing

-- | Sort known issues by priority
sortKnownIssue :: [(Behavior, Analysis)] -> [(Behavior, Analysis)]
sortKnownIssue [] = []
sortKnownIssue (x:xs) = sortKnownIssue gteq ++ [x] ++ sortKnownIssue lt
    where
        lt = Prelude.filter (< x) xs
        gteq = Prelude.filter (>= x) xs