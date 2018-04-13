{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module HtmlReportGenerator.Generator (
  generateReport2Html
) where

import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A

import           Prelude                     hiding (div, head, span)

import           Types
import qualified Data.Text.Lazy as LT

-- |Css link to the bootstrap stylesheet
cssLink :: AttributeValue
cssLink = "https://stackpath.bootstrapcdn.com/bootstrap/4.1.0/css/bootstrap.min.css"

-- |Render lists
renderAnalysisLists :: [(Knowledge, [LT.Text])] -> Html
renderAnalysisLists xs =
    div ! A.class_ "list-group" $
      mapM_ renderAnalysis xs

-- |Render list-group
renderAnalysis :: (Knowledge, [LT.Text]) -> Html
renderAnalysis (Knowledge{..}, xs) =
    div ! A.class_ "list-group-item list-group-item-action flex-column align-items-start" $ do
      div ! A.class_ "d-flex w-100 justify-content-between" $
        h5 ! A.class_ "mb-1 text-muted" $ toHtml kProblem
      p ! A.class_ "mb-1" $ toHtml kSolution
      span ! A.class_ "badge badge-warning" $ toHtml kErrorCode
      ul $
        mapM_ renderErrorText (take 10 xs)

-- |Render each error list item
renderErrorText :: LT.Text -> Html
renderErrorText str = li ! A.class_ "text-muted" $ toHtml str

-- |Render header
jumbotron :: Html
jumbotron =
    div ! A.class_ "jumbotron bg-info text-white" $
      div ! A.class_ "container" $ do
        h1 ! A.class_ "display-4" $ "Cardano Log Classifier"
        p ! A.class_ "lead" $ "We've successfully analyzed your log folder!"

-- |Takes lists of analysis and generate html as output
generateReport2Html :: [(Knowledge, [LT.Text])] -> Html
generateReport2Html xs = docTypeHtml $ do
    head $ do
      title "Cardano Log Classifier"
      link ! A.href cssLink ! A.rel "stylesheet" ! A.type_ "text/css" ! A.title "CSS"
    body $ do
        jumbotron
        main ! A.class_ "container" $
          renderAnalysisLists xs
