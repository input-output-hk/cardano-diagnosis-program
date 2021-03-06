{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module HtmlReportGenerator.Generator
       (
         generateReport2Html
       , generateErrorReport
       ) where

import qualified Data.Text.Lazy               as LT
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes  as A

import           Exceptions
import           HtmlReportGenerator.Solution (renderSolution)
import           Types                        (ErrorCode, Knowledge (..))

import           Prelude                      hiding (div, head, span)

-- Create ToMarkup instance for ExtractorException and ErrorCode
newtype HtmlException = HtmlException ExtractorException

instance ToMarkup HtmlException where
    toMarkup (HtmlException err) = toMarkup $ show err

newtype HtmlErrorCode = HtmlErrorCode ErrorCode

instance ToMarkup HtmlErrorCode where
    toMarkup (HtmlErrorCode err) = toMarkup $ show err

-- | Css link to the bootstrap stylesheet
cssLink :: AttributeValue
cssLink = "https://stackpath.bootstrapcdn.com/bootstrap/4.1.0/css/bootstrap.min.css"

headerTitle :: Html
headerTitle = h1 ! A.class_ "display-4" $ "Cardano Diagnosis Program"

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
        span ! A.class_ "badge badge-warning" $ toHtml (HtmlErrorCode kErrorCode)
      div ! A.class_ "card-body" $ do
        renderSolution kErrorCode kSolution
        footer $
          ul ! A.class_ "list-unstyled" $
            mapM_ renderErrorText (take 10 xs)

-- | Render each error list item
renderErrorText :: LT.Text -> Html
renderErrorText str = li ! A.class_ "text-muted" $ toHtml str

-- | Render header
renderHeader :: [(Knowledge, [LT.Text])] -> Html
renderHeader xs =
    if xs /= []
    then div ! A.class_ "jumbotron bg-info text-white" $
          div ! A.class_ "container" $ do
          headerTitle
          p ! A.class_ "lead" $ "We've successfully analyzed your log folder!"
    else div ! A.class_ "jumbotron bg-success text-white" $
          div ! A.class_ "container" $ do
          headerTitle
          p ! A.class_ "lead" $ "We've successfully analyzed your log folder!"
          p ! A.class_ "lead" $ "No issue found!"

-- | Render help section
renderHelpSection :: Html
renderHelpSection =
    div ! A.class_ "card mb-5" $ do
      div ! A.class_ "card-header" $ "Still having issues?"
      div ! A.class_ "card-body" $ do
        p ! A.class_ "card-text" $ "If you are still having issues, please contact us from button below"
        a ! A.href "https://daedaluswallet.io/faq/" ! A.class_ "btn btn-primary" $ "Go"

-- | Takes lists of analysis and generate html as output
generateReport2Html :: [(Knowledge, [LT.Text])] -> Html
generateReport2Html xs = docTypeHtml $ do
    head $ do
      title "Cardano Diagnosis Program"
      link ! A.href cssLink ! A.rel "stylesheet" ! A.type_ "text/css" ! A.title "CSS"
    body $ do
        renderHeader xs
        main ! A.class_ "container" $ do
          renderAnalysisLists xs
          renderHelpSection

-- | Generate error report (Used to indicate that analysis failed)
generateErrorReport :: ExtractorException -> Html
generateErrorReport e = do
  head $ do
    title "Cardano Diagnosis Program"
    link ! A.href cssLink ! A.rel "stylesheet" ! A.type_ "text/css" ! A.title "CSS"
  body $ do
    renderErrorHeader e
    main ! A.class_ "container" $
      renderHelpSection

-- | Render header for error message
renderErrorHeader :: ExtractorException -> Html
renderErrorHeader e =
  div ! A.class_ "jumbotron bg-warning" $
    div ! A.class_ "container" $ do
      headerTitle
      p ! A.class_ "lead" $ "Something went wrong while analyzing the log file: "
      p ! A.class_ "lead" $ toHtml (HtmlException e)
