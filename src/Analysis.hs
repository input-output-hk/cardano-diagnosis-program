{-# LANGUAGE OverloadedStrings #-}

-- | Deprecated file, will delete soon
module Analysis where

import Types

-- Todo: use csv and parse them
k1 :: Knowledge
k1 = Knowledge "returned empty list" NetworkError "Network error" "Unknown"

k2 :: Knowledge
k2 = Knowledge "useless for the following reason" TimeSync "Time out of sync" "Sync the local clock"

k3 :: Knowledge
k3 = Knowledge "malformed" DBError "Block data is corrupted" "Delete content of DB-1.0"

k4 :: Knowledge
k4 = Knowledge "error" Error "Misc" "Unknown"

k5 :: Knowledge
k5 = Knowledge "No such file or directory" FileNotFound 
            "File missing or wrong username" "Reinstall Daedalus / Change username to latin-characters"

k6 :: Knowledge
k6 = Knowledge "resource exhausted (No space left on device)" ShortStorage "Not enough space on hard drive" "Create more space"

knowledgeBase :: KnowledgeBase
knowledgeBase = [k1,k2,k3,k4,k5,k6]