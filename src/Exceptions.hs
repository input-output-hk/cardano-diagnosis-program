
module Exceptions where

import           Control.Exception.Safe
import           Data.Semigroup         ((<>))

data ExtractorException
    = FileNotFound String      -- ^ Could not find the file
    | DirectoryNotFound String -- ^ Could not find the directory
    | UnknownOS String         -- ^ Unknown operating system

instance Show ExtractorException where
  show (FileNotFound str)      = "File not found: " <> str
  show (DirectoryNotFound str) = "Directory not found: " <> str
  show (UnknownOS str)         = "Unknown operating system: " <> str

instance Exception ExtractorException
