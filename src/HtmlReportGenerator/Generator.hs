{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module HtmlReportGenerator.Generator (
  generateReport2Html
) where

import Data.Monoid ((<>))

import Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A

import Prelude hiding (div, head)

import Types

cssLink :: AttributeValue
cssLink = "https://stackpath.bootstrapcdn.com/bootstrap/4.1.0/css/bootstrap.min.css"

renderAnalysisLists :: [Analysis] -> Html
renderAnalysisLists xs =
  div ! A.class_ "list-group" $
    mapM_ renderAnalysis xs

renderAnalysis :: Analysis -> Html
renderAnalysis Analysis{..} =
  div ! A.class_ "list-group-item list-group-item-action flex-column align-items-start" $ do
    div ! A.class_ "d-flex w-100 justify-content-between" $
      h5 ! A.class_ "mb-1 text-muted" $ toHtml aProblem
    p ! A.class_ "mb-1" $ toHtml aSolution
    small ! A.class_ "text-danger" $ toHtml aErrorCode <> ":"
    small ! A.class_ "text-muted" $ toHtml aErrorText

jumbotron :: Html
jumbotron =
  div ! A.class_ "jumbotron bg-info text-white" $
    div ! A.class_ "container" $ do
      h1 ! A.class_ "display-4" $ "Cardano Log Classifier"
      p ! A.class_ "lead" $ "We've successfully analyzed your log folder!"

generateReport2Html :: [Analysis] -> Html
generateReport2Html xs = docTypeHtml $ do
  head $ do
    title "Cardano Log Classifier"
    link ! A.href cssLink ! A.rel "stylesheet" ! A.type_ "text/css" ! A.title "CSS"
  body $ do
      jumbotron 
      main ! A.class_ "container" $
        renderAnalysisLists xs