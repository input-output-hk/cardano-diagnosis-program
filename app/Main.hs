{-# LANGUAGE OverloadedStrings #-}

module Main where

import           GHC.Stack                     (HasCallStack)
import           Data.Monoid                   ((<>))

import qualified Data.ByteString.Lazy          as LBS

import qualified Data.Text.Lazy                as LT
import qualified Data.Text.Lazy.Encoding       as LT

import qualified Codec.Archive.Zip             as Zip

import           Data.Attoparsec.Text.Lazy

import           Data.List                     (nub)
import           Data.Map                      (Map)
import qualified Data.Map                      as Map

import           KnowledgebaseParser.CSVParser (parseKnowLedgeBase)
import           Parsers
import           Types                         (KnowledgeBase)

logFile :: FilePath
logFile = "./logs/node.pub"

knowledgeBaseFile :: FilePath
knowledgeBaseFile = "./knowledgebase/knowledge.csv"

-- | Read knowledgebase CSV file
readKB :: HasCallStack => FilePath -> IO KnowledgeBase
readKB path = do
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

main :: IO ()
main = do
    kbase <- readKB knowledgeBaseFile
    file <- LBS.readFile logFile            -- Read File
    let eachLine        = LT.lines $ LT.decodeUtf8 file
        knownErrors     = map (runAnalysis kbase) eachLine -- Parse file
        filteredErrors  = nub $ sortKnownIssue $ filterMaybe knownErrors
    mapM_ print filteredErrors