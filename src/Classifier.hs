{-# LANGUAGE RecordWildCards #-}

module Classifier (
      runClassifiers
    , extractIssuesFromLogs
    ) where

import           Data.List               (sort, nub)

import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LT
import Data.Text.Encoding.Error (ignore)

import qualified Data.ByteString.Lazy    as LBS

import           Control.Monad.Reader

import           Types                   (Knowledge (..), KnowledgeBase, Analysis (..))

extractIssuesFromLogs :: [LBS.ByteString] -> Reader KnowledgeBase [Analysis]
extractIssuesFromLogs logs = filterLogs <$> mapM runClassifiers logs
   where
      filterLogs = sort . nub . join

-- | Run analysis on given file
runClassifiers :: LBS.ByteString -> Reader KnowledgeBase [Analysis]
runClassifiers logfile = do
    let eachLine = LT.lines $ LT.decodeUtf8With ignore logfile
    analysis <- mapM analyzeLine eachLine
    return $ join analysis

-- | Todo: accumulate the log lines
analyzeLine :: LT.Text -> Reader KnowledgeBase [Analysis]
analyzeLine str = do
    kbase <- ask
    let compareWithKnowledge :: LT.Text -> Knowledge -> Maybe Analysis
        compareWithKnowledge s Knowledge{..} = 
          if kErrorText `LT.isInfixOf` s
              then Just $ Analysis kErrorCode kProblem kSolution s
              else Nothing
    return $ filterMaybe $ compareWithKnowledge str <$> kbase

-- | Filter out Nothing from list of Maybe a
filterMaybe :: [Maybe a] -> [a]
filterMaybe = foldr (\x acc -> case x of
                          Nothing  -> acc
                          Just ele -> ele : acc) []
