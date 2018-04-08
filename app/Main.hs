{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as LBS
import Data.Text (lines)
import Data.Text.Encoding (decodeUtf8)
import Prelude hiding (lines)
import Data.List (nub)

import Data.Attoparsec.Text

import Parsers
import Types (KnowledgeBase)
import KnowledgebaseParser.Parser (parseKnowLedgeBase)

import Data.Monoid ((<>))

logFile :: FilePath
logFile = "./logs/node.pub"

knowledgeBaseFile :: FilePath
knowledgeBaseFile = "./knowledgebase/knowledge.csv"

main :: IO ()
main = do
    kbase <- readKB
    file <- LBS.readFile logFile            -- Read File
    let eachLine = (lines $ decodeUtf8 file)
        knownErrors   = nub $ sortKnownIssue $ filterMaybe $ map (runAnalysis kbase)  eachLine -- Parse file
    mapM_ print knownErrors

readKB :: IO KnowledgeBase
readKB = do
    kfile <- LBS.readFile knowledgeBaseFile -- Load knowledebase
    let kb = parseOnly parseKnowLedgeBase (decodeUtf8 kfile)
    case kb of
        Left e    -> error $ "File not found" <> e-- Ugh need to do something better
        Right res -> return res

filterMaybe :: [Maybe a] -> [a]
filterMaybe [] = []
filterMaybe (x:xs) = case x of
                Nothing -> filterMaybe xs
                (Just a) -> a :filterMaybe xs