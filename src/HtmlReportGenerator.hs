{-# LANGUAGE OverloadedStrings #-}

module HtmlReportGenerator (
    formatData
  , generateReport2Html
) where


import Control.Monad (forM_)

import Data.Text.Lazy (Text)
import Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A

import Prelude hiding (div, head)

import Types

cssLink :: AttributeValue
cssLink = "https://stackpath.bootstrapcdn.com/bootstrap/4.1.0/css/bootstrap.min.css"

renderList :: (ToMarkup a) => Text -> [a] -> Html
renderList headline xs =
  div ! A.class_ "my-3 p-3 bg-white rounded box-shadow" $ do
    h4 ! A.class_ "border-bottom border-gray pb-2 mb-0" $ toHtml headline
    ul ! A.class_ "list-group list-group-flush" $ forM_ xs (\x -> div (toHtml x) ! A.class_ "list-group-item" )

jumbotron :: Html
jumbotron =
  div ! A.class_ "jumbotron bg-info text-white" $
    div ! A.class_ "container" $ do
      h1 ! A.class_ "display-4" $ "Cardano Log Classifier"
      p ! A.class_ "lead" $ "We've successfully analyzed your log folder!"

generateReport2Html :: PrintData -> Html
generateReport2Html (PrintData (ps, ss, ecodes, etexts)) = docTypeHtml $ do
  head $ do
    title "Cardano Log Classifier"
    link ! A.href cssLink ! A.rel "stylesheet" ! A.type_ "text/css" ! A.title "CSS"
  body $ do
      jumbotron 
      main ! A.class_ "container" $ do
        renderList "Problem" ps
        renderList "Possible Solution" ss
        renderList "Error Code" ecodes
        renderList "Error Text" etexts