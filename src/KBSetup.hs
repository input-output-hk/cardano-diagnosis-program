module KBSetup (
   knowledgeBaseFile
 , setupKB
) where

import           GHC.Stack                       (HasCallStack)
import           Data.Monoid                     ((<>))

import qualified Data.ByteString.Lazy            as LBS

import qualified Data.Text.Lazy.Encoding         as LT

import           Data.Attoparsec.Text.Lazy

import           KnowledgebaseParser.CSVParser   (parseKnowLedgeBase)
import           Types

-- | Path to the knowledge base
knowledgeBaseFile :: FilePath
knowledgeBaseFile = "./knowledgebase/knowledge.csv"

-- | Read knowledgebase csv file
setupKB :: HasCallStack => FilePath -> IO KnowledgeBase
setupKB path = do
    kfile <- LBS.readFile path
    let kb = parse parseKnowLedgeBase (LT.decodeUtf8 kfile)
    case eitherResult kb of
        Left e    -> error $ "File not found" <> e
        Right res -> return res