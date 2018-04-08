{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as LBS
import Data.Text (lines)
import Data.Text.Encoding (decodeUtf8)
import Prelude hiding (lines)
import Data.List (nub)
import qualified Data.Text.Lazy.IO as T

import Data.Attoparsec.Text

import Parsers
import KnowledgebaseParser.Parser (parseKnowLedgeBase)

logFile :: FilePath
logFile = "./logs/node.pub"

knowledgeBaseFile :: FilePath
knowledgeBaseFile = "./knowledgebase/knowledge.csv"

main :: IO ()
main = do
    file <- LBS.readFile logFile
    kfile <- LBS.readFile knowledgeBaseFile
    let kb       = parseOnly parseKnowLedgeBase (decodeUtf8 kfile)
    case kb of
        Right kbase -> mapM_ print kbase
        Left e -> putStrLn "Error!"
    let eachLine = (lines $ decodeUtf8 file)
        knownErrors   = nub $ sortKnownIssue $ filterMaybe $ map runAnalysis eachLine
    mapM_ print knownErrors

filterMaybe :: [Maybe a] -> [a]
filterMaybe [] = []
filterMaybe (x:xs) = case x of
                Nothing -> filterMaybe xs
                (Just a) -> a :filterMaybe xs