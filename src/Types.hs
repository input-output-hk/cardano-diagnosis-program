module Types (
   ErrorCode (..)
 , Knowledge (..)
 , Analysis  (..)
 , KnowledgeBase
) where

import Text.Blaze.Html5 (ToMarkup, toMarkup)
import qualified Data.Text.Lazy as LT

data ErrorCode =
      ShortStorage
    | UserNameError
    | TimeSync
    | FileNotFound
    | StaleLockFile
    | DBError
    | PermissonError
    | BalanceError
    | NetworkError
    | ConnectionRefused
    | Unknown
    | Error
    deriving (Eq, Ord, Show)

data Knowledge =
  Knowledge {
     kErrorText :: !LT.Text
  ,  kErrorCode :: !ErrorCode
  ,  kProblem   :: !LT.Text
  ,  kSolution  :: !LT.Text
  } deriving (Eq, Show)

type KnowledgeBase = [Knowledge]

data Analysis =
  Analysis {
    aErrorCode :: !ErrorCode
  , aProblem   :: !LT.Text
  , aSolution  :: !LT.Text
  , aErrorText :: !LT.Text
  } deriving (Show)

instance Ord Knowledge where
  k1 <= k2 = kErrorCode k1 <= kErrorCode k2

instance Eq Analysis where
  a1 == a2 = aErrorCode a1 == aErrorCode a2

instance Ord Analysis where
  a1 <= a2 = aErrorCode a1 <= aErrorCode a2

instance ToMarkup ErrorCode where
  toMarkup err = toMarkup $ show err