{-# LANGUAGE FlexibleInstances #-}

module Types (
   ErrorCode (..)
 , Knowledge (..)
 , KnowledgeBase
 , Analysis
) where

import qualified Data.Text.Lazy   as LT
import           Text.Blaze.Html5 (ToMarkup, toMarkup)

import Data.Map (Map)

-- Every error should have its unique errorcode
data ErrorCode =
      ShortStorage
    | UserNameError
    | TimeSync
    | FileNotFound
    | StaleLockFile
    | DBError
    | PermissonError
    | DBPath
    | CannotGetDBSize
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

type Analysis = Map Knowledge [LT.Text]

instance ToMarkup ErrorCode where
  toMarkup err = toMarkup $ show err

instance Eq Knowledge where
  e1 == e2 = kErrorCode e1 == kErrorCode e2

instance Ord Knowledge where
  e1 <= e2 = kErrorCode e1 <= kErrorCode e2