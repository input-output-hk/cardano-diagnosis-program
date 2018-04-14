{-# LANGUAGE OverloadedStrings #-}

module HtmlReportGenerator.Solution
       (
         renderSolution
       ) where

import qualified Data.Text.Lazy              as LT
import Data.Semigroup ((<>))
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

bold :: LT.Text -> Html
bold str = p ! A.class_ "font-weight-bold" $ toHtml str

-- | Render solution for DBError
renderDBError :: LT.Text -> Html
renderDBError str =
  p ! A.class_ "card-text" $ do
    p $ toHtml str
    bold "Win"
    ol $ do
      li $ "Type " <> mark "%appdata%" <> "on the explorer’s address bar"
      li $ "Open " <> mark "Daedalus" <> " folder"
      li $ "Delete the content of " <> mark "DB-1.0"
    bold "Mac"
    ol $ do
      li $ "On the Finder’s menu bar select " <> mark "Go"
      li $ "Click " <> mark "Go to Finder…"
      li $ "Type " <> mark "~/Library/Application Support/Daedalus"
      li $ "Delete the content of " <> mark "DB-1.0"

-- | Render solution for Time sync
renderTimeSync :: LT.Text -> Html
renderTimeSync str =
  p ! A.class_ "card-text" $ do
    p $ toHtml str
    bold "Win"
    ol $ do
      li $ "Press " <> mark "Win+S"
      li $ "Type in " <> mark "Settings"
      li $ "In " <> mark "Find a setting field" <> " type in " <> mark "Date"
      li $ "Go to " <> mark "Date and time settings"
      li $ "Press " <> mark "Additional date, time & regional settings" <> ". New window will pop up"
      li $ "Press " <> mark "Set date and time"
      li $ "Go to " <> mark "Internet time tab"
      li $ "Press " <> mark "Change settings'"
      li $ "Press " <> mark "Update now" <> "Note: there should be a message like The clock was successfully synchronized"
    bold "Mac"
    ol $ do
      li "Quit the application"
      li "Click the clock that is located on the top right of the screen"
      li $ "Select " <> mark "Open Date & Time Preferences'"
      li "Unlock the preferences"
      li $ "Uncheck the " <> mark "Set date and time automatically"
      li "Restart the application"

-- | Render solution for stale lock file
renderStaleLockFile :: LT.Text -> Html
renderStaleLockFile str =
  p ! A.class_ "card-text" $ do
    p $ toHtml str
    bold "Win"
    ol $ do
      li "Quit the application"
      li $ "Type " <> mark "%appdata%" <> "on the explorer’s search bar"
      li $ "Open " <> mark "Daedalus" <> " folder"
      li $ "Open " <> mark "Wallet-1.0" <> " and delete file " <> mark "open.lock"
      li "Start the application"
    bold "Mac"
    ol $ do
      li $ "On the Finder’s menu bar select " <> mark "Go"
      li $ "Click " <> mark "Go to Finder…"
      li $ "Type " <> mark "~/Library/Application Support/Daedalus"
      li $ "Open " <> mark "Wallet-1.0" <> "and delete file " <> mark "open.lock"
      li "Start the application"
