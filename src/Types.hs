{-# LANGUAGE FlexibleInstances #-}

module Types
       (
         Analysis
       , ErrorCode (..)
       , Knowledge (..)
       ) where

import qualified Data.Text.Lazy   as LT
import           Text.Blaze.Html5 (ToMarkup, toMarkup)

import           Data.Map         (Map)

-- | Identifier for each error
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

-- | Record identifying the issue
data Knowledge = Knowledge
  {  kErrorText :: !LT.Text    -- ^ Text used for matching error lines
  ,  kErrorCode :: !ErrorCode  -- ^ Identity for error code
  ,  kProblem   :: !LT.Text    -- ^ Text describing what is the problem
  ,  kSolution  :: !LT.Text    -- ^ Text describing how to solve the issue
  }

-- | Map used to collect error lines
type Analysis = Map Knowledge [LT.Text]

instance ToMarkup ErrorCode where
    toMarkup err = toMarkup $ show err

instance Eq Knowledge where
    e1 == e2 = kErrorCode e1 == kErrorCode e2

instance Ord Knowledge where
    e1 <= e2 = kErrorCode e1 <= kErrorCode e2
