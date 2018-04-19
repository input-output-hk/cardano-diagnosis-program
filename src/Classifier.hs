{-# LANGUAGE RecordWildCards #-}

module Classifier
       (
         extractIssuesFromLogs
       ) where


import qualified Data.ByteString.Lazy     as LBS
import qualified Data.Map                 as Map
import           Data.Text.Encoding.Error (ignore)
import qualified Data.Text.Lazy           as LT
import qualified Data.Text.Lazy.Encoding  as LT

import           Types                    (Analysis, Knowledge (..))

-- | Analyze each log file based on the knowlodgebases' data.
extractIssuesFromLogs :: [LBS.ByteString] -> Analysis -> Analysis
extractIssuesFromLogs files analysis = filterAnalysis $ foldr runClassifiers analysis files

-- | Run analysis on given file
runClassifiers :: LBS.ByteString -> Analysis -> Analysis
runClassifiers logfile analysis =
    let logLines = LT.lines $ LT.decodeUtf8With ignore logfile
    in foldr analyzeLine analysis logLines

-- | Analyze each line
analyzeLine :: LT.Text -> Analysis -> Analysis
analyzeLine str = Map.mapWithKey (compareWithKnowledge str)

-- | Compare the line with knowledge lists
compareWithKnowledge :: LT.Text -> Knowledge -> [LT.Text] -> [LT.Text]
compareWithKnowledge str Knowledge{..} xs =
    if kErrorText `LT.isInfixOf` str
    then str : xs
    else xs

-- | Filter out any record that has empty (i.e couldn't catch any string related)
filterAnalysis :: Analysis -> Analysis
filterAnalysis = Map.filter (/= [])
