{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module HtmlReportGenerator.Generator
       (
         generateReport2Html
       ) where

import qualified Data.Text.Lazy              as LT
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A

import           Types                       (Knowledge(..))
import           HtmlReportGenerator.Solution (renderSolution)

import           Prelude                     hiding (div, head, span)

-- | Css link to the bootstrap stylesheet
cssLink :: AttributeValue
cssLink = "https://stackpath.bootstrapcdn.com/bootstrap/4.1.0/css/bootstrap.min.css"

-- | Render lists
renderAnalysisLists :: [(Knowledge, [LT.Text])] -> Html
renderAnalysisLists xs =
    div ! A.class_ "some" $
      mapM_ renderAnalysis xs

-- | Render list-group
renderAnalysis :: (Knowledge, [LT.Text]) -> Html
renderAnalysis (Knowledge{..}, xs) =
    div ! A.class_ "card flex-column border-dark mb-3" $ do
      div ! A.class_ "card-header" $ do
        span $ toHtml kProblem
        span ! A.class_ "badge badge-warning" $ toHtml kErrorCode
      div ! A.class_ "card-body" $ do
        renderSolution kErrorCode kSolution
        footer $
          ul ! A.class_ "list-unstyled" $
            mapM_ renderErrorText (take 10 xs)

-- | Render each error list item
renderErrorText :: LT.Text -> Html
renderErrorText str = li ! A.class_ "text-muted" $ toHtml str

-- | Render header
renderHeader :: Html
renderHeader =
    div ! A.class_ "jumbotron bg-info text-white" $
      div ! A.class_ "container" $ do
        h1 ! A.class_ "display-4" $ "Cardano Log Parser"
        p ! A.class_ "lead" $ "We've successfully analyzed your log folder!"

-- | Render help section
renderHelpSection :: Html
renderHelpSection = 
    div ! A.class_ "card mb-5" $ do
      div ! A.class_ "card-header" $ "Is your issue solved?"
      div ! A.class_ "card-body" $ do
        p ! A.class_ "card-text" $ "If you still have issues, please contact us from button below"
        a ! A.href "#" ! A.class_ "btn btn-primary" $ "Go"

-- | Takes lists of analysis and generate html as output
generateReport2Html :: [(Knowledge, [LT.Text])] -> Html
generateReport2Html xs = docTypeHtml $ do
    head $ do
      title "Cardano Log Parser"
      link ! A.href cssLink ! A.rel "stylesheet" ! A.type_ "text/css" ! A.title "CSS"
    body $ do
        renderHeader
        main ! A.class_ "container" $ do
          renderAnalysisLists xs
          renderHelpSection
