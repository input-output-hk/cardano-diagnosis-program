{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Text.Lazy (Text)
import Data.Map (Map)

data ErrorCode =
    FileNotFound 
  | TimeSync
  | DBError
  | ShortStorage
  | NetworkError
  | ConnectionRefused
  | Error
  | Unknown
  deriving (Eq, Ord, Show)

data Behavior =
    Behavior {
      bErrorText :: !Text
    , bErrorCode :: !ErrorCode
    } deriving (Eq, Ord, Show)

data Analysis = 
    Analysis {
      aProblem  :: !Text
    , aSolution :: ![Text]
    } deriving (Eq, Ord, Show)

type KnowledgeBase = Map Behavior Analysis