{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Text (Text)

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

data Knowledge =
  Knowledge {
     kErrorText :: !Text
  ,  kErrorCode :: !ErrorCode
  ,  kProblem   :: !Text
  ,  kSolution  :: !Text
  } deriving (Eq, Ord, Show)

type KnowledgeBase = [Knowledge]