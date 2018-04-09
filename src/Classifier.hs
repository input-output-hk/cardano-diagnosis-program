{-# LANGUAGE RecordWildCards #-}

module Classifier (
      runClassifiers
    , extractIssuesFromLogs
    ) where

import           Data.List               (sort)

import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LT

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
    kbase <- ask
    let eachLine = V.fromList $ LT.lines $ LT.decodeUtf8 logfile -- this is an array which is way too slow
    return $ vFilterMaybe $ V.map (analyzeLine kbase) eachLine

-- | Run analysis on given line
analyzeLine :: KnowledgeBase -> LT.Text -> Maybe Knowledge
analyzeLine [] _ = Nothing
analyzeLine (k@Knowledge{..}:xs) str =
    if kErrorText `LT.isInfixOf` str
     then Just k
     else analyzeLine xs str

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
