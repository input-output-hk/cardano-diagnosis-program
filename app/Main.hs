{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Monoid                     ((<>))

import           Data.List                       (sort)
import           GHC.Stack                       (HasCallStack)
import           System.Environment              (getArgs)

import qualified Codec.Archive.Zip               as Zip

import qualified Data.ByteString.Lazy            as LBS
import qualified Data.Text.Lazy.Encoding         as LT

import           Data.Time.Calendar              (showGregorian)
import           Data.Time.Clock                 (UTCTime (..), getCurrentTime)
import           Text.Blaze.Html.Renderer.Pretty (renderHtml)

import           Data.Map                        (Map)
import qualified Data.Map                        as Map

import           Data.Attoparsec.Text.Lazy

import           Control.Monad.State             (execState)

import           Classifier                      (extractIssuesFromLogs)
import           HtmlReportGenerator.Generator   (generateReport2Html)
import           KnowledgebaseParser.CSVParser   (parseKnowLedgeBase)
import           Types                           (Analysis, Knowledge)

-- | Path to the knowledge base
knowledgeBaseFile :: FilePath
knowledgeBaseFile = "./knowledgebase/knowledge.csv"

-- | Read knowledgebase csv file and return analysis environment
setupAnalysisEnv :: HasCallStack => FilePath -> IO Analysis
setupAnalysisEnv path = do
    kfile <- LBS.readFile path
    let kb = parse parseKnowLedgeBase (LT.decodeUtf8 kfile)
    case eitherResult kb of
        Left e    -> error $ "File not found" <> e
        Right res -> return $ setAnalysis res

setAnalysis :: [Knowledge] -> Analysis
setAnalysis kbase = Map.fromList $ map (\kn -> (kn, [])) kbase

-- | Read zip file
readZip :: LBS.ByteString -> Either String (Map FilePath LBS.ByteString)
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
    file <- LBS.readFile path
    let zipMap = readZip file
    case zipMap of
        Left e        -> error $ "Error occured: " <> e
        Right fileMap -> return fileMap

-- | Extract log file from given zip file
extractLogFromZip :: FilePath -> IO [LBS.ByteString]
extractLogFromZip path = do
    zipMap <- readZippedPub path                             -- Read File
    let extractedLogs = Map.elems $ Map.take 5 zipMap        -- Extract selected logs
    return extractedLogs

-- | Get log file from directory
getLogsFromDirectory :: IO [LBS.ByteString]
getLogsFromDirectory = undefined

main :: IO ()
main = do
    analysisEnv <- setupAnalysisEnv knowledgeBaseFile     -- Read & create knowledge base
    args  <- getArgs
    extractedLogs   <- case args of
        (logFilePath: _) -> extractLogFromZip logFilePath
        _                -> getLogsFromDirectory
    putStrLn "Running analysis on logs"
    currTime <- getCurrentTime
    let analysisResult = execState (extractIssuesFromLogs extractedLogs) analysisEnv
        resultFilename = "result-" <> showGregorian (utctDay currTime) <> ".html"
    writeFile resultFilename $ renderHtml $ generateReport2Html (sort $ Map.toList analysisResult)
    putStrLn $ "Analysis done successfully!! See " <> resultFilename
    -- Todo: generate different html based on the result
