{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Monoid                   ((<>))
import           GHC.Stack                     (HasCallStack)

import qualified Data.ByteString.Lazy          as LBS

import qualified Data.Text.Lazy                as LT
import qualified Data.Text.Lazy.Encoding       as LT

import qualified Codec.Archive.Zip             as Zip

import           Data.Attoparsec.Text.Lazy

import           Data.List                     (nub)
import           Data.Map                      (Map)
import qualified Data.Map                      as Map

import           Classifier                    (runClassifier, sortKnownIssue)
import           KnowledgebaseParser.CSVParser (parseKnowLedgeBase)
import           Types                         (KnowledgeBase)

logFile :: FilePath
logFile = "./logs/node.pub"

knowledgeBaseFile :: FilePath
knowledgeBaseFile = "./knowledgebase/knowledge.csv"

zLogFile :: FilePath
zLogFile = "./logs/pub.zip"

-- | Read knowledgebase csv file
setupKB :: HasCallStack => FilePath -> IO KnowledgeBase
setupKB path = do
    kfile <- LBS.readFile path-- Load knowledebase
    let kb = parse parseKnowLedgeBase (LT.decodeUtf8 kfile)
    case eitherResult kb of
        Left e    -> error $ "File not found" <> e-- Ugh need to do something better
        Right res -> return res

-- | Filter out Nothing from list of Maybe a
filterMaybe :: [Maybe a] -> [a]
filterMaybe [] = []
filterMaybe (x:xs) = case x of
                Nothing  -> filterMaybe xs
                (Just a) -> a :filterMaybe xs

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

-- | Read zip file and return files that will be analyzed
readZippedPub :: HasCallStack => FilePath -> IO LBS.ByteString
readZippedPub path = do
    file <- LBS.readFile path
    let zipMap = readZip file
    case zipMap of
        Left e -> error $ "Error occured: " <> e
        Right fileMap -> case Map.lookup "pub/node.pub" fileMap of
            Nothing -> error "cannot find node.pub file"
            Just node -> return node-- just take cardano.log

main :: IO ()
main = do
    kbase <- setupKB knowledgeBaseFile                       -- Read & create knowledge base
    file <- readZippedPub zLogFile                             -- Read File
    let eachLine        = LT.lines $ LT.decodeUtf8 file
        knownErrors     = map (runClassifier kbase) eachLine -- Parse file
        filteredErrors  = nub $ sortKnownIssue $ filterMaybe knownErrors
    mapM_ print filteredErrors