{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Melange.View.Index
  (
    IndexPage (..)
  ) where

import           Data.Text                   (Text, pack)
import           Data.Time                   (Day)
import           Melange.Model               (Board (..))
import           Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

newtype IndexPage = IndexPage (Maybe Board)

dayToText :: Day -> Text
dayToText = pack . show

boardToTitle :: Board -> Text
boardToTitle (Board Nothing day _) = dayToText day
boardToTitle (Board (Just title') day _) =
  mconcat [ title'
          , " ("
          , dayToText day
          , ")"
          ]

page :: Html -> Html
page bod = docTypeHtml $ do
  melangeTitle
  linkCSS
  body bod

melangeTitle :: Html
melangeTitle = H.title "Melange"

linkCSS :: Html
linkCSS = H.link ! A.rel "stylesheet" ! A.href "/static/melange.css" ! A.type_ "text/css"

instance ToMarkup IndexPage where
  toMarkup (IndexPage Nothing) = page $ p "There is nothing to display."

  toMarkup (IndexPage (Just bo@Board{..}))=
    page $ do
      H.div ! A.class_ "header" $
        h1 (toHtml . boardToTitle $ bo)
      H.div $
        mapM_ toMarkup items
