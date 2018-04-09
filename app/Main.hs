{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Maybe                    (fromMaybe)
import           Data.Monoid                   ((<>))
import           GHC.Stack                     (HasCallStack)


import qualified Codec.Archive.Zip             as Zip

import qualified Data.ByteString.Lazy          as LBS

import qualified Data.Text.Lazy.Encoding       as LT

import           Data.Attoparsec.Text.Lazy

import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import Control.Monad.Reader

import           Classifier                    (extractIssuesFromLogs)
import           KnowledgebaseParser.CSVParser (parseKnowLedgeBase)
import           Types                         (KnowledgeBase)

logFile :: FilePath
logFile = "./logs/node.pub"

knowledgeBaseFile :: FilePath
knowledgeBaseFile = "./knowledgebase/knowledge.csv"

zLogFile :: FilePath
zLogFile = "./logs/pub.zip"

file2Lookup :: [FilePath]
file2Lookup = ["pub/Daedalus.log", "pub/node.pub", "pub/node.pub.0", "pub/node.pub.1"]

-- | Read knowledgebase csv file
setupKB :: HasCallStack => FilePath -> IO KnowledgeBase
setupKB path = do
    kfile <- LBS.readFile path
    let kb = parse parseKnowLedgeBase (LT.decodeUtf8 kfile)
    case eitherResult kb of
        Left e    -> error $ "File not found" <> e
        Right res -> return res

-- | Read zip file
readZip :: HasCallStack => LBS.ByteString -> Either String (Map FilePath LBS.ByteString)
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

-- | Extract selected log file from the given map
extractLogs :: Map FilePath LBS.ByteString -> [FilePath] -> [LBS.ByteString]
extractLogs zipmap = map (extractLog zipmap)
    where
        extractLog :: Map FilePath LBS.ByteString -> FilePath -> LBS.ByteString
        extractLog zipmaps path = fromMaybe (error $ "couldn't find the file: " <> path)
                                            (Map.lookup path zipmaps)

main :: IO ()
main = do
    kbase <- setupKB knowledgeBaseFile                       -- Read & create knowledge base
    zipMap <- readZippedPub zLogFile                         -- Read File
    let extractedLogs = extractLogs zipMap file2Lookup       -- Extract selected logs
        filteredKnownErrors = runReader (extractIssuesFromLogs extractedLogs) kbase -- Analyze logs
    mapM_ print filteredKnownErrors