{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as LBS
import Data.Text.Lazy (lines)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Prelude hiding (lines)

import Parsers
import Data.List (nub)

logFile :: FilePath
logFile = "./logs/node.pub"

knowledgeBaseFile :: FilePath
knowledgeBaseFile = "./knowledgebase/knowledge.csv"

main :: IO ()
main = do
    file <- LBS.readFile logFile

    let eachLine = (lines $ decodeUtf8 file)
        knownErrors   = nub $ sortKnownIssue $ filterMaybe $ map runAnalysis eachLine
    mapM_ print knownErrors

filterMaybe :: [Maybe a] -> [a]
filterMaybe [] = []
filterMaybe (x:xs) = case x of
                Nothing -> filterMaybe xs
                (Just a) -> a :filterMaybe xs