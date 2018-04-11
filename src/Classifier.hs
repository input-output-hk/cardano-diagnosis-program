{-# LANGUAGE RecordWildCards #-}

module Classifier (
      runClassifiers
    , extractIssuesFromLogs
    ) where

import           Data.List               (sort)

import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LT
import Data.Text.Encoding.Error (ignore)

import           Data.ByteString.Lazy    as LBS

import           Data.Vector             (Vector)
import qualified Data.Vector             as V

import           Control.Monad.Reader

import           Types                   (Knowledge (..), KnowledgeBase)

extractIssuesFromLogs :: [LBS.ByteString] -> Reader KnowledgeBase [Knowledge]
extractIssuesFromLogs logs = filterLogs <$> mapM runClassifiers logs
   where
      filterLogs = sort . V.toList . vNub . V.concat

-- | Run analysis on given file
runClassifiers :: LBS.ByteString -> Reader KnowledgeBase (Vector Knowledge)
runClassifiers logfile = do
    let eachLine = V.fromList $ LT.lines $ LT.decodeUtf8With ignore logfile -- this is an array which is way too slow
    analysis <- V.mapM analyzeLine eachLine
    return $ join analysis

-- | Maybe we should use vector on knowledgebase as well..
-- | need to accumulate the logs
analyzeLine :: LT.Text -> Reader KnowledgeBase (Vector Knowledge)
analyzeLine str = do
    kbase <- ask
    let compareWithKnowledge :: LT.Text -> Knowledge -> Maybe Knowledge
        compareWithKnowledge s k@Knowledge{..} = 
          if kErrorText `LT.isInfixOf` s
              then Just k
              else Nothing
    return $ vFilterMaybe $ V.fromList $ compareWithKnowledge str <$> kbase

-- | Filter out Nothing from list of Maybe a
vFilterMaybe :: Vector (Maybe a) -> Vector a
vFilterMaybe = V.foldr (\x acc -> case x of
                          Nothing  -> acc
                          Just ele -> ele `V.cons` acc) V.empty

-- | Vector version of nub
vNub :: (Eq a) => Vector a -> Vector a
vNub = V.foldr (\x acc -> if x `V.notElem` acc
                              then x `V.cons` acc
                              else acc) V.empty
