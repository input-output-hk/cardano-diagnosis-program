module Types (
   ErrorCode (..)
 , Knowledge (..)
 , KnowledgeBase
) where

import qualified Data.Text.Lazy as LT

data ErrorCode =
    Error
  | Unknown
  | ConnectionRefused
  | NetworkError
  | FileNotFound
  | TimeSync
  | DBError
  | ShortStorage
  deriving (Eq, Ord, Show)

data Knowledge =
  Knowledge {
     kErrorText :: !LT.Text
  ,  kErrorCode :: !ErrorCode
  ,  kProblem   :: !LT.Text
  ,  kSolution  :: !LT.Text
  } deriving (Eq, Show)

type KnowledgeBase = [Knowledge]

instance Ord Knowledge where
  k1 <= k2 = kErrorCode k1 <= kErrorCode k2