module Types (
   ErrorCode (..)
 , Knowledge (..)
 , Analysis  (..)
 , KnowledgeBase
) where

import qualified Data.Text.Lazy   as LT
import           Text.Blaze.Html5 (ToMarkup, toMarkup)

-- Every error should have its unique errorcode
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
  }

type KnowledgeBase = [Knowledge]

data Analysis =
  Analysis {
    aErrorCode :: !ErrorCode
  , aProblem   :: !LT.Text
  , aSolution  :: !LT.Text
  , aErrorText :: !LT.Text
  } deriving (Show)

instance Eq Analysis where
  a1 == a2 = aErrorCode a1 == aErrorCode a2

instance Ord Analysis where
  a1 <= a2 = aErrorCode a1 <= aErrorCode a2

instance ToMarkup ErrorCode where
  toMarkup err = toMarkup $ show err
