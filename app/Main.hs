{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Codec.Archive.Zip               as Zip
import           Control.Exception.Safe          (throw)
import           Data.Attoparsec.Text.Lazy       (eitherResult, parse)
import qualified Data.ByteString.Lazy            as LBS
import           Data.List                       (sort)
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Monoid                     ((<>))
import qualified Data.Text.Lazy.Encoding         as LT
import           Data.Time.Calendar              (showGregorian)
import           Data.Time.Clock                 (UTCTime (..), getCurrentTime)
import           System.Directory                (createDirectoryIfMissing,
                                                  doesDirectoryExist,
                                                  doesPathExist,
                                                  getAppUserDataDirectory,
                                                  getHomeDirectory,
                                                  listDirectory)
import           System.Environment              (getArgs)
import           System.Info                     (os)
import           Text.Blaze.Html.Renderer.Pretty (renderHtml)

import           Classifier                      (extractIssuesFromLogs)
import           Exceptions
import           HtmlReportGenerator.Generator   (generateErrorReport,
                                                  generateReport2Html)
import           KnowledgebaseParser.CSVParser   (parseKnowLedgeBase)
import           Types                           (Analysis, setupAnalysis)

-- | Path to the knowledge base
knowledgeBaseFile :: FilePath
knowledgeBaseFile = "./knowledgebase/knowledge.csv"

-- | Create error report
handleError :: ExtractorException -> IO a
handleError e = do
    createDirectoryIfMissing True "./result"
    writeFile "./result/error.html" $ renderHtml $ generateErrorReport e
    throw e

-- | Read knowledgebase csv file and return analysis environment
setupAnalysisEnv :: FilePath -> IO Analysis
setupAnalysisEnv path = do
    kfile <- LBS.readFile path
    let kb = parse parseKnowLedgeBase (LT.decodeUtf8 kfile)
    case eitherResult kb of
        Left e    -> handleError $ FileNotFound e
        Right res -> return $ setupAnalysis res

-- | Read zip file
readZip ::LBS.ByteString -> Either String (Map FilePath LBS.ByteString)
readZip rawzip = case Zip.toArchiveOrFail rawzip of
    Left err      -> Left err
    Right archive -> Right $ finishProcessing archive
    where
    finishProcessing :: Zip.Archive -> Map FilePath LBS.ByteString
    finishProcessing  = Map.fromList . map handleEntry . Zip.zEntries
    handleEntry :: Zip.Entry -> (FilePath, LBS.ByteString)
    handleEntry entry = (Zip.eRelativePath entry, Zip.fromEntry entry)

-- | Read zip file
readZippedPub :: FilePath -> IO (Map FilePath LBS.ByteString)
readZippedPub path = do
    fileExist <- doesPathExist path
    if fileExist
      then do
        file <- LBS.readFile path
        let zipMap = readZip file
        case zipMap of
            Left e        -> handleError $ FileNotFound e
            Right fileMap -> return fileMap
      else handleError $ FileNotFound path

-- | Extract log file from given zip file
extractLogsFromZip :: Int -> FilePath -> IO [LBS.ByteString]
extractLogsFromZip numberOfFiles path = do
    zipMap <- readZippedPub path                             -- Read File
    let extractedLogs = Map.elems $ Map.take numberOfFiles zipMap        -- Extract selected logs
    return extractedLogs

-- | Get filepath to pub folder depending on the operating system
getFilePath2Pub :: IO FilePath
getFilePath2Pub = case os of
            "darwin"  -> getPathOnMac
            "linux"   -> getPathOnLinux
            "mingw32" -> getPathOnWindows
            _         -> handleError $ UnknownOS os

-- | Extract log file from mac
--
-- @ /Users/shioihiroto/Library/Application Support/Daedalus/Logs/pub
getPathOnMac :: IO FilePath
getPathOnMac = do
    home <- getHomeDirectory
    let path2Pub = home <> "/Library/Application Support/Daedalus/Logs/pub/"
    return path2Pub

-- | Extract log file from Windows
--
-- @ /C:/Users/<user>/AppData/Roaming/<app>/
getPathOnWindows :: IO FilePath
getPathOnWindows = getAppUserDataDirectory "Daedalus/Logs/pub/"

-- | Extract log file from linux
--
-- @ /.local/share/Daedalus/mainnet/
getPathOnLinux :: IO FilePath
getPathOnLinux = do
    home <- getHomeDirectory
    let path2Pub = home <> "/.local/share/Daedalus/mainnet/Logs/pub/"
    return path2Pub

-- | Extract log file from Daedalus/Logs/pub
extractLogsFromDirectory :: Int -> IO [LBS.ByteString]
extractLogsFromDirectory numberOfFiles = do
    path2Pub <- getFilePath2Pub
    putStrLn $ "Diagnosis is running on " <> os
    putStrLn $ "Path to pub folder is: " <> path2Pub
    doesExist <- doesDirectoryExist path2Pub
    if not doesExist
    then handleError $ DirectoryNotFound path2Pub
    else do
      fileList <- listDirectory path2Pub
      let logFiles = map (\file -> path2Pub ++ file) (take numberOfFiles $ filter (/= ".DS_Store") fileList)
      mapM LBS.readFile logFiles

main :: IO ()
main = do
    analysisEnv <- setupAnalysisEnv knowledgeBaseFile  -- Read & create knowledge base
    args  <- getArgs
    let numberOfFiles = 5
    extractedLogs   <- case args of                    -- Extract logs depending on the args
        (logFilePath: _) -> extractLogsFromZip numberOfFiles logFilePath
        _                -> extractLogsFromDirectory numberOfFiles
    putStrLn "Running analysis on logs"
    currTime <- getCurrentTime
    let analysisResult = extractIssuesFromLogs extractedLogs analysisEnv  -- Parse log files
        resultFilename = "result-" <> showGregorian (utctDay currTime) <> ".html"
    createDirectoryIfMissing True "./result"
    writeFile ("./result/" <> resultFilename) $ renderHtml $ generateReport2Html (sort $ Map.toList analysisResult)
    putStrLn $ "Analysis done successfully!! See " <> resultFilename
