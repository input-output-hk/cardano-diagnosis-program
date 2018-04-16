{-# LANGUAGE RecordWildCards #-}

module Classifier
       (
         extractIssuesFromLogs
       ) where

import           Control.Monad.State      (State, get, put)
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.Map                 as Map
import           Data.Text.Encoding.Error (ignore)
import qualified Data.Text.Lazy           as LT
import qualified Data.Text.Lazy.Encoding  as LT

import           Types                    (Analysis, Knowledge (..))

-- | Run classifiers on each log file
extractIssuesFromLogs :: [LBS.ByteString] -> State Analysis ()
extractIssuesFromLogs file = do
    mapM_ runClassifiers file
    filterAnalysis

-- | Run analysis on given file
runClassifiers :: LBS.ByteString -> State Analysis ()
runClassifiers logfile = do
    let eachLine = LT.lines $ LT.decodeUtf8With ignore logfile
    mapM_ analyzeLine eachLine

-- | Analyze each line
analyzeLine :: LT.Text -> State Analysis ()
analyzeLine str = do
    aMap <- get
    put $ Map.mapWithKey (compareWithKnowledge str) aMap

-- | Compare the line with knowledge lists
compareWithKnowledge :: LT.Text -> Knowledge -> [LT.Text] -> [LT.Text]
compareWithKnowledge str Knowledge{..} xs =
    if kErrorText `LT.isInfixOf` str
    then str : xs
    else xs

-- | Filter out any record that has empty (i.e couldn't catch any string related)
filterAnalysis :: State Analysis ()
filterAnalysis = do
    aMap <- get
    let filteredMap = Map.filter (/= []) aMap
    put filteredMap
