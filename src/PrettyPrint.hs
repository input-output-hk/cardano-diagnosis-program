{-# LANGUAGE RecordWildCards #-}

module PrettyPrint where

import Data.Text.Lazy (Text, unpack)
import Data.Monoid ((<>))

import Types

type Problem = Text
type Solution = Text
type ErrorText = Text

newtype PrintData = PrintData ([Problem], [Solution], [ErrorCode], [ErrorText])

formatData' :: [Knowledge] -> PrintData -> PrintData
formatData' [] fd                           = fd
formatData' (Knowledge{..}:xs) (PrintData (a,b,c,d)) = 
  formatData' xs (PrintData (kProblem:a, kSolution:b, kErrorCode:c ,kErrorText:d))

formatData :: [Knowledge] -> PrintData
formatData xs = formatData' xs (PrintData ([],[],[],[]))

instance Show PrintData where
  show (PrintData (ps, ss, ecodes, etexts)) =
    "**** Cardano classifier analysis ****\n\n"     <>
    "==== Problem ====\n"                           <> printListS ps <>
    "\n==== Possible Solution ====\n"               <> printListS ss <>
    "\n==== Error Category ====\n"                  <> printList ecodes <>
    "\n==== Error Texts ====\n"                     <> printListS etexts

-- Code smell!
printListS :: [Text] -> String
printListS = foldr (\p acc -> "- " <> unpack p <> "\n" <> acc) []

printList :: (Show a) => [a] -> String
printList = foldr (\p acc -> "- " <> show p <> "\n" <> acc) []