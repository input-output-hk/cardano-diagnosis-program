{-# LANGUAGE RecordWildCards #-}

module Classifier
       (
         extractIssuesFromLogs
       ) where

import qualified Data.ByteString.Lazy     as LBS
import           Data.List                (foldl')
import qualified Data.Map.Strict          as Map
import           Data.Text.Encoding.Error (ignore)
import qualified Data.Text.Lazy           as LT
import qualified Data.Text.Lazy.Encoding  as LT

import           Types                    (Analysis, Knowledge (..))

-- | Analyze each log file based on the knowlodgebases' data.
extractIssuesFromLogs :: [LBS.ByteString] -> Analysis -> Analysis
extractIssuesFromLogs files analysis = filterAnalysis $ foldl' runClassifiers analysis files

-- | Run analysis on given file
runClassifiers :: Analysis -> LBS.ByteString -> Analysis
runClassifiers analysis logfile =
    let logLines = LT.lines $ LT.decodeUtf8With ignore logfile
    in foldl' analyzeLine analysis logLines

-- | Analyze each line
analyzeLine :: Analysis -> LT.Text -> Analysis
analyzeLine analysis str = Map.mapWithKey (compareWithKnowledge str) analysis

-- | Compare the line with knowledge lists
compareWithKnowledge :: LT.Text -> Knowledge -> [LT.Text] -> [LT.Text]
compareWithKnowledge str Knowledge{..} xs =
    if kErrorText `LT.isInfixOf` str
    then str : xs
    else xs

-- | Filter out any records that are empty (i.e couldn't catch any string related)
filterAnalysis :: Analysis -> Analysis
filterAnalysis = Map.filter (/= [])
