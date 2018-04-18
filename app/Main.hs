{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Codec.Archive.Zip               as Zip
import           Control.Monad.State             (execState)
import           Data.Attoparsec.Text.Lazy       (parse, eitherResult)
import qualified Data.ByteString.Lazy            as LBS
import           Data.List                       (sort)
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Monoid                     ((<>))
import Data.Text (Text)
import qualified Data.Text.Lazy.Encoding         as LT
import           Data.Time.Calendar              (showGregorian)
import           Data.Time.Clock                 (UTCTime (..), getCurrentTime)
import           GHC.Stack                       (HasCallStack)
import           System.Environment              (getArgs)
import           System.Directory                (createDirectoryIfMissing, doesPathExist)
import           Text.Blaze.Html.Renderer.Pretty (renderHtml)

import           Classifier                      (extractIssuesFromLogs)
import           HtmlReportGenerator.Generator   (generateReport2Html, generateErrorReport)
import           KnowledgebaseParser.CSVParser   (parseKnowLedgeBase)
import           LogExtractor                    (extractLogsFromDirectory)
import           Types                           (Analysis, setupAnalysis)

-- | Path to the knowledge base
knowledgeBaseFile :: FilePath
knowledgeBaseFile = "./knowledgebase/knowledge.csv"

-- | Create error report
handleError :: HasCallStack => String -> IO a
handleError str = do
    createDirectoryIfMissing True "./result"
    writeFile "./result/error.html" $ renderHtml $ generateErrorReport str
    error str

-- | Read knowledgebase csv file and return analysis environment
setupAnalysisEnv :: HasCallStack => FilePath -> IO Analysis
setupAnalysisEnv path = do
    kfile <- LBS.readFile path
    let kb = parse parseKnowLedgeBase (LT.decodeUtf8 kfile)
    case eitherResult kb of
        Left e    -> handleError $ "File not found: " <> e
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
readZippedPub :: HasCallStack => FilePath -> IO (Map FilePath LBS.ByteString)
readZippedPub path = do
    fileExist <- doesPathExist path
    if fileExist
      then do 
        file <- LBS.readFile path
        let zipMap = readZip file
        case zipMap of
            Left e        -> handleError $ "Error occured: " <> e
            Right fileMap -> return fileMap
      else handleError $ "File not found: " <> path

-- | Extract log file from given zip file
extractLogsFromZip :: HasCallStack => FilePath -> IO [LBS.ByteString]
extractLogsFromZip path = do
    zipMap <- readZippedPub path                             -- Read File
    let extractedLogs = Map.elems $ Map.take 5 zipMap        -- Extract selected logs
    return extractedLogs

main :: IO ()
main = do
    analysisEnv <- setupAnalysisEnv knowledgeBaseFile  -- Read & create knowledge base
    args  <- getArgs
    extractedLogs   <- case args of                    -- Extract logs depending on the args
        (logFilePath: _) -> extractLogsFromZip logFilePath
        _                -> extractLogsFromDirectory
    putStrLn "Running analysis on logs"
    currTime <- getCurrentTime
    let analysisResult = execState (extractIssuesFromLogs extractedLogs) analysisEnv  -- Parse log files
        resultFilename = "result-" <> showGregorian (utctDay currTime) <> ".html"
    createDirectoryIfMissing True "./result"
    writeFile ("./result/" <> resultFilename) $ renderHtml $ generateReport2Html (sort $ Map.toList analysisResult)
    putStrLn $ "Analysis done successfully!! See " <> resultFilename
    -- Todo: generate different html based on the result
