{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Melange.View.Index
  (
    BoardPage (..)
  ) where

import           Data.Text                   (Text, pack)
import           Data.Time                   (Day)
import           Melange.Model               (Board (..))
import qualified Melange.Model.Navigation    as N (BoardNavigation (..))
import           Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

data BoardPage = BoardPage (Maybe Board) N.BoardNavigation

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

linkToBoard :: AttributeValue -> Html -> Day -> Html
linkToBoard className content d = a ! A.class_ className ! A.href (textValue  . dayToText $ d) $ content

instance ToMarkup BoardPage where
  toMarkup (BoardPage Nothing _) = page $ p "There is nothing to display."

  toMarkup (BoardPage (Just bo@Board{..}) N.BoardNavigation{..})=
    page $ do
      H.div ! A.class_ "header secondClass" $ do
        maybe mempty (linkToBoard "previous" "<<<") previous
        h1 (toHtml . boardToTitle $ bo)
        maybe mempty (linkToBoard "next" ">>>") next
      H.div ! A.class_ "board-content" $
        mapM_ toMarkup items
