{-# LANGUAGE RecordWildCards #-}

module Classifier (
      runClassifiers
    , extractIssuesFromLogs
    ) where

import           Data.Text.Encoding.Error (ignore)
import qualified Data.Text.Lazy           as LT
import qualified Data.Text.Lazy.Encoding  as LT
import qualified Data.ByteString.Lazy     as LBS

import           Control.Monad.State      (State, get, put, when)

import qualified Data.Map                 as Map

import           Types                    (Analysis, Knowledge (..))

-- |Run classifiers on each log file
extractIssuesFromLogs :: [LBS.ByteString] -> State Analysis ()
extractIssuesFromLogs file = do
    mapM_ runClassifiers file
    filterAnalysis

-- |Run analysis on given file
runClassifiers :: LBS.ByteString -> State Analysis ()
runClassifiers logfile = do
    let eachLine = LT.lines $ LT.decodeUtf8With ignore logfile
    mapM_ analyzeLine eachLine

-- |Analyze each line
analyzeLine :: LT.Text -> State Analysis ()
analyzeLine str = do
    aMap <- get
    -- Todo: Think of clever ways of doing this
    let ks = extractErrorTexts  $ Map.keys aMap
    mapM_ (compareWithKnowledge str) ks

-- |Extract errortext from knowledge
extractErrorTexts :: [Knowledge] -> [(LT.Text, Knowledge)]
extractErrorTexts = foldr (\k@Knowledge{..} acc -> (kErrorText, k) : acc) []

-- |Compare the line with knowledge lists
compareWithKnowledge :: LT.Text -> (LT.Text, Knowledge) -> State Analysis ()
compareWithKnowledge str (etext, k) = do
    aMap <- get
    when (etext `LT.isInfixOf` str) $
      put (Map.update (\acc -> Just $ str : acc) k aMap)

-- |Filter out any record that has empty (i.e couldn't catch any string related)
filterAnalysis :: State Analysis ()
filterAnalysis = do
    aMap <- get
    let filteredMap = Map.filter (/= []) aMap
    put filteredMap
