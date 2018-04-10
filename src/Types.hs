{-# LANGUAGE RecordWildCards #-}

module Types (
   ErrorCode (..)
 , Knowledge (..)
 , KnowledgeBase
 , PrintData (..)
 , formatData
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

type Problem = LT.Text
type Solution = LT.Text
type ErrorText = LT.Text

newtype PrintData = PrintData ([Problem], [Solution], [ErrorCode], [ErrorText])

-- Todo: refactor this code
formatData' :: [Knowledge] -> PrintData -> PrintData
formatData' [] fd                           = fd
formatData' (Knowledge{..}:xs) (PrintData (p,s,ec,es)) = 
  formatData' xs (PrintData (nubList kProblem p, nubList kSolution s, nubList kErrorCode ec, nubList kErrorText es))
    where
      nubList :: (Ord a) => a -> [a] -> [a]
      nubList a ary = if a `elem` ary
                      then ary
                      else a:ary

formatData :: [Knowledge] -> PrintData
formatData xs = formatData' xs (PrintData ([],[],[],[]))