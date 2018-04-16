module LogExtractor
       (
         extractLogsFromDirectory
       ) where

import qualified Data.ByteString.Lazy as LBS
import           Data.Monoid          ((<>))
import           GHC.Stack            (HasCallStack)
import           System.Directory     (doesDirectoryExist,
                                       getAppUserDataDirectory,
                                       getHomeDirectory, listDirectory)
import           System.Info          (os)

-- | WIP : Need to test them on each operating system
extractLogsFromDirectory :: IO [LBS.ByteString]
extractLogsFromDirectory = case os of
                      "darwin"  -> extractLogOnMac
                      "linux"   -> extractLogOnLinux
                      "mingw32" -> extractLogOnWindows
                      _         -> error $ "Unknown operating system: " <> os

-- | Extract log file from mac
--
-- @ /Users/shioihiroto/Library/Application Support/Daedalus/Logs/pub
extractLogOnMac :: HasCallStack => IO [LBS.ByteString]
extractLogOnMac = do
    home <- getHomeDirectory
    let path2Pub = home <> "/Library/Application Support/Daedalus/Logs/pub/"
    extractLogFiles path2Pub

-- | Extract log file from Windows
--
-- @ /C:/Users/<user>/AppData/Roaming/<app>/
extractLogOnWindows :: HasCallStack => IO [LBS.ByteString]
extractLogOnWindows = do
    path2Pub <- getAppUserDataDirectory "Daedalus/Logs/pub/"
    extractLogFiles path2Pub

-- | Extract log file from linux
--
-- @ /.local/share/Daedalus/mainnet/
extractLogOnLinux :: HasCallStack => IO [LBS.ByteString]
extractLogOnLinux = do
    home <- getHomeDirectory
    let path2Pub = home <> "/.local/share/Daedalus/mainnet/Logs/pub/"
    extractLogFiles path2Pub

-- | Extract log file from Daedalus/Logs/pub
extractLogFiles :: HasCallStack => FilePath -> IO [LBS.ByteString]
extractLogFiles path2Pub = do
    putStrLn $ "Diagnosis is running on " <> os
    putStrLn $ "Path to pub folder is: " <> path2Pub
    doesExist <- doesDirectoryExist path2Pub
    if not doesExist
    then error "FilePath error: File does not exist"
    else do
      fileList <- listDirectory path2Pub
      let logFiles = map (\file -> path2Pub ++ file) (take 5 $ filter (/= ".DS_Store") fileList)
      mapM LBS.readFile logFiles