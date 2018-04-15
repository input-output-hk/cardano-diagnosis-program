module LogExtractor where

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
                      "darwin"  -> extractLogFromMac
                      "windows" -> extractLogFromWindows
                      _         -> extractLogFromLinux

-- | Extract log file from mac
--
-- __/Users/shioihiroto/Library/Application Support/Daedalus/Logs/pub__
extractLogFromMac :: HasCallStack => IO [LBS.ByteString]
extractLogFromMac = do
    home <- getHomeDirectory
    let path2Pub = home <> "/Library/Application Support/Daedalus/Logs/pub/"
    extractLogFiles path2Pub

-- | Extract log file from Windows
--
-- __/C:/Users/<user>/AppData/Roaming/<app>)__
extractLogFromWindows :: HasCallStack => IO [LBS.ByteString]
extractLogFromWindows = do
    path2Pub <- getAppUserDataDirectory "Daedalus/Logs/pub/"
    extractLogFiles path2Pub

-- | Extract log file from linux
--
-- __~/.local/share/Daedalus/mainnet/__
extractLogFromLinux :: HasCallStack => IO [LBS.ByteString]
extractLogFromLinux = do
    let path2Pub = "~/.local/share/Daedalus/mainnet/Logs/pub"
    extractLogFiles path2Pub

-- | Extract log file from Daedalus/Logs/pub
extractLogFiles :: HasCallStack => FilePath -> IO [LBS.ByteString]
extractLogFiles path2Pub = do
    putStrLn $ "Given path is: " <> path2Pub
    doesExist <- doesDirectoryExist path2Pub
    if not doesExist
    then error "FilePath error: File does not exist"
    else do
      fileList <- listDirectory path2Pub
      let logFiles = map (\file -> path2Pub ++ file) (take 5 $ filter (/= ".DS_Store") fileList)
      mapM LBS.readFile logFiles