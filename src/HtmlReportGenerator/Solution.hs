{-# LANGUAGE OverloadedStrings #-}

module HtmlReportGenerator.Solution
       (
         renderSolution
       ) where

import qualified Data.Text.Lazy              as LT
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A

import           Types                       (ErrorCode (..))

-- | This is temporal workaround for rendering solutions
--this will be deprecated in the future version.
renderSolution :: ErrorCode -> LT.Text -> Html
renderSolution TimeSync      t = renderTimeSync t
renderSolution StaleLockFile t = renderStaleLockFile t
renderSolution DBError       t = renderDBError t
renderSolution _             t = renderSimpleSolution t

-- | Common rendering for simple solution
renderSimpleSolution :: LT.Text -> Html
renderSimpleSolution str = p ! A.class_ "card-text" $ toHtml str

-- | Render solution for DBError
renderDBError :: LT.Text -> Html
renderDBError str =
  p ! A.class_ "card-text" $ do
    p $ toHtml str
    p ! A.class_ "font-weight-bold" $ "Win"
    ol $ do
      li "Type %appdata% on the explorer’s address bar"
      li "Open Daedalus folder"
      li "Delete the content of DB-1.0"
    p ! A.class_ "font-weight-bold" $ "Mac"
    ol $ do
      li "On the Finder’s menu bar select 'Go'"
      li "Click 'Go to Finder…'"
      li "Type '~/Library/Application Support/Daedalus'"
      li "Delete the content of 'DB-1.0'"

-- | Render solution for Time sync
renderTimeSync :: LT.Text -> Html
renderTimeSync str =
  p ! A.class_ "card-text" $ do
    p $ toHtml str
    p ! A.class_ "font-weight-bold" $ "Win"
    ol $ do
      li "Press Win+S"
      li "Type in Settings"
      li "In Find a setting field type in Date"
      li "Go to Date and time settings"
      li "Press 'Additional date, time & regional settings'. New window will pop up"
      li "Press 'Set date and time'"
      li "Go to 'Internet time tab'"
      li "Press 'Change settings'"
      li "Press Update now Note: there should be a message like The clock was successfully synchronized"
    p ! A.class_ "font-weight-bold" $ "Mac"
    ol $ do
      li "Quit the application"
      li "Click the clock that is located on the top right of the screen"
      li "Select 'Open Date & Time Preferences'"
      li "Unlock the preferences"
      li "Uncheck the 'Set date and time automatically'"
      li "Restart the application"

-- | Render solution for stale lock file
renderStaleLockFile :: LT.Text -> Html
renderStaleLockFile str =
  p ! A.class_ "card-text" $ do
    p $ toHtml str
    p ! A.class_ "font-weight-bold" $ "Win"
    ol $ do
      li "Quit the application"
      li "Type %appdata% on the explorer’s search bar"
      li "Open Daedalus folder"
      li "Open 'Wallet-1.0' and delete file 'open.lock'"
      li "Start the application"
    p ! A.class_"font-weight-bold" $ "Mac"
    ol $ do
      li "On the Finder’s menu bar select 'Go'"
      li "Click 'Go to Finder…'"
      li "Type '~/Library/Application Support/Daedalus'"
      li "Open 'Wallet-1.0' and delete file 'open.lock'"
      li "Start the application"
