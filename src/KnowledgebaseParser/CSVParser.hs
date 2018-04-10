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
    <|> (string "StaleLockFile"     >> return StaleLockFile)
    <|> (string "BalanceError"      >> return BalanceError)
    <|> (string "TimeSync"          >> return TimeSync)
    <|> (string "DBError"           >> return DBError)
    <|> (string "UserNameError"     >> return UserNameError)
    <|> (string "ShortStorage"      >> return ShortStorage)

-- | Parse each csv records
-- Not really clean code..
parseKnowledge :: Parser Knowledge
parseKnowledge = do
    e <- quotedField
    _ <- char ','
    _ <- char '"'
    c <- parseErrorCode
    _ <- char '"'
    _ <- char ','
    p <- quotedField
    _ <- char ','
    s <- quotedField
    return $ Knowledge e c p s

-- | Parse CSV file and create knowledgebase
parseKnowLedgeBase :: Parser KnowledgeBase
parseKnowLedgeBase = many $ parseKnowledge <* endOfLine