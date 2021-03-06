{-# LANGUAGE FlexibleInstances #-}

module Types
       (
         Analysis
       , ErrorCode (..)
       , Knowledge (..)
       , setupAnalysis
       ) where

import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import qualified Data.Text.Lazy        as LT

-- | Identifier for each error
data ErrorCode
    = ShortStorage      -- ^ Not enough space on hard drive to store block data
    | UserNameError     -- ^ User is using non-latin characters for username
    | TimeSync          -- ^ User's PC's time is out of sync
    | FileNotFound      -- ^ Some of the files were not installed properly
    | StaleLockFile     -- ^ Open.lock file is corrupted
    | DBError           -- ^ Local block data is corrupted
    | DBPath            -- ^ Daedalus cannot find certain files
    | CannotGetDBSize   -- ^ Error message of Couidn't pack log files shows up
    | BalanceError      -- ^ Daedalus shows wrong Ada amount
    | NetworkError      -- ^ Firewall is blocking the connection
    | ConnectionRefused -- ^ Firewall is blocking the connection
    | ResourceVanished  -- ^ Network error
    | Unknown           -- ^ Unknown error (currently not used)
    | Error             -- ^ Error (currently not used)
    deriving (Eq, Ord, Show)

-- | Record identifying the issue
data Knowledge = Knowledge
  {  kErrorText :: !LT.Text   -- ^ Text used for matching error lines
  ,  kErrorCode :: !ErrorCode -- ^ Identity for error code
  ,  kProblem   :: !LT.Text   -- ^ Text describing what is the problem
  ,  kSolution  :: !LT.Text   -- ^ Text describing how to solve the issue
  }

-- | Map used to collect error lines
type Analysis = Map Knowledge [LT.Text]

-- | Create initial analysis environment
setupAnalysis :: [Knowledge] -> Analysis
setupAnalysis kbase = Map.fromList $ map (\kn -> (kn, [])) kbase

instance Eq Knowledge where
    e1 == e2 = kErrorCode e1 == kErrorCode e2

instance Ord Knowledge where
    e1 <= e2 = kErrorCode e1 <= kErrorCode e2
