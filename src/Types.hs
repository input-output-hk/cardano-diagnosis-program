module Types (
   ErrorCode (..)
 , Knowledge (..)
 , KnowledgeBase
) where

import Text.Blaze.Html5 (ToMarkup, toMarkup)
import qualified Data.Text.Lazy as LT

data ErrorCode =
    Error
  | Unknown
  | ConnectionRefused
  | NetworkError
  | PermissonError
  | BalanceError
  | FileNotFound
  | StaleLockFile
  | TimeSync
  | DBError
  | UserNameError
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


instance ToMarkup ErrorCode where
  toMarkup err = toMarkup $ show err