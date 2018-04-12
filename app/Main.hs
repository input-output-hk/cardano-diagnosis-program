{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Monoid                     ((<>))

import Data.List (sort)
import           GHC.Stack                       (HasCallStack)
import           System.Environment              (getArgs)

import qualified Codec.Archive.Zip               as Zip

import qualified Data.ByteString.Lazy            as LBS

import           Text.Blaze.Html.Renderer.Pretty (renderHtml)
import           Data.Time.Calendar              (showGregorian)
import           Data.Time.Clock                 (UTCTime (..), getCurrentTime)

import           Control.Monad.State
import           Data.Map                        (Map)
import qualified Data.Map                        as Map

import           Classifier                      (extractIssuesFromLogs)
import           HtmlReportGenerator.Generator   (generateReport2Html)

import           KBSetup                         (knowledgeBaseFile, setupKB)

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

main :: IO ()
main = do
    kbase <- setupKB knowledgeBaseFile                       -- Read & create knowledge base
    (logFilePath: _)      <- getArgs
    zipMap <- readZippedPub logFilePath                      -- Read File
    putStrLn "Running analysis on log file"
    let analysis = Map.fromList $ map (\kn -> (kn, [])) kbase
        extractedLogs = Map.elems $ Map.take 5 zipMap        -- Extract selected logs
        filteredKnownErrors = execState (extractIssuesFromLogs extractedLogs) analysis -- Analyze logs
    currTime <- getCurrentTime
    let resultFilename = "result-" <> showGregorian (utctDay currTime) <> ".html"
    writeFile resultFilename $ renderHtml $ generateReport2Html (sort $ Map.toList filteredKnownErrors)
    putStrLn $ "Analysis done successfully!! See " <> resultFilename
    -- Todo: generate different html based on the result
