{-# LANGUAGE OverloadedStrings #-}

module Analysis where

import Data.Map (fromList)

import Types

-- Todo: use csv and parse them
b1 :: Behavior
b1 = Behavior "returned empty list" NetworkError

a1 :: Analysis
a1 = Analysis "Network error" ["Unknown"]

b2 :: Behavior
b2 = Behavior "useless for the following reason" TimeSync

a2 :: Analysis
a2 = Analysis "Time out of sync" ["Sync the local clock"]

b3 :: Behavior
b3 = Behavior "malformed" DBError

a3 :: Analysis
a3 = Analysis "Block data is corrupted" ["Delete content of DB-1.0"]

b4 :: Behavior
b4 = Behavior "error" Error

a4 :: Analysis
a4 = Analysis "Misc" ["Unknown"]

b5 :: Behavior
b5 = Behavior "No such file or directory" FileNotFound

a5 :: Analysis
a5 = Analysis "Some file is missing" ["Reinstall Daedalus", "Change username to latin-characters"]

b6 :: Behavior
b6 = Behavior "resource exhausted (No space left on device)" ShortStorage

a6 :: Analysis
a6 = Analysis "Not enough space on hard drive" ["Create more space"]

knowledgeBase :: KnowledgeBase
knowledgeBase = fromList [(b1, a1), (b2, a2), (b3, a3), (b4, a4), (b5, a5)
                        , (b6, a6)]