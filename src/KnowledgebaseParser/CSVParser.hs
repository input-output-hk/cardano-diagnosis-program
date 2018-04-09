{-# LANGUAGE OverloadedStrings #-}

module KnowledgebaseParser.CSVParser (
 parseKnowLedgeBase
) where

import           Control.Applicative

import           Data.Attoparsec.Text.Lazy

import qualified Data.Text.Lazy            as LT

import           Types

import           Prelude                   hiding (takeWhile)

-- | Take any string that is inside quotes
insideQuotes :: Parser LT.Text
insideQuotes =
   LT.append <$> (LT.fromStrict <$> takeWhile (/= '"'))
            <*> (LT.concat <$> many (LT.cons <$> dquotes <*> insideQuotes))
   <?> "inside of double quotes"
   where
      dquotes =
         string "\"\"" >> return '"'
         <?> "paired double quotes"

-- | Parse quoted field
quotedField :: Parser LT.Text
quotedField =
   char '"' *> insideQuotes <* char '"'
   <?> "quoted field"

-- | Parse Errorcode
parseErrorCode :: Parser ErrorCode
parseErrorCode =
        (string "Error"             >> return Error)
    <|> (string "Unknown"           >> return Unknown)
    <|> (string "ConnectionRefused" >> return ConnectionRefused)
    <|> (string "NetworkError"      >> return NetworkError)
    <|> (string "PermissionError"   >> return PermissonError)
    <|> (string "FileNotFound"      >> return FileNotFound)
    <|> (string "BalanceError"      >> return BalanceError)
    <|> (string "TimeSync"          >> return TimeSync)
    <|> (string "DBError"           >> return DBError)
    <|> (string "ShortStorage"      >> return ShortStorage)

parseKnowledge :: Parser Knowledge
parseKnowledge = do
    e <- quotedField
    char ','
    char '"'
    c <- parseErrorCode
    char '"'
    char ','
    p <- quotedField
    char ','
    s <- quotedField
    return $ Knowledge e c p s

-- | Run parser an create knowledgebase
parseKnowLedgeBase :: Parser KnowledgeBase
parseKnowLedgeBase = many $ parseKnowledge <* endOfLine
