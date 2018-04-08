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
  } deriving (Eq, Show)

type KnowledgeBase = [Knowledge]

instance Ord Knowledge where
  k1 <= k2 = kErrorCode k1 <= kErrorCode k2