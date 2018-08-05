{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Melange.View.Index
  (
    IndexPage (..)
  ) where

import           Data.Text                   (Text, pack)
import           Data.Time                   (Day)
import           Melange.Model.External
import           Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

newtype IndexPage = IndexPage (Maybe Board)

dayToText :: Day -> Text
dayToText = pack . show

boardToTitle :: Board -> Text
boardToTitle (Board _ Nothing day _) = dayToText day
boardToTitle (Board _ (Just title') day _) =
  mconcat [ title'
          , " ("
          , dayToText day
          , ")"
          ]

instance ToMarkup IndexPage where
  toMarkup (IndexPage Nothing) = do
    H.head $
      H.title "Melange"
    body $
      p "There is nothing to display."
  toMarkup (IndexPage (Just bo@Board{..}))= do
    H.head $
      H.title "Melange"
    body $ do
      H.div ! A.class_ "header" $
        h1 (toHtml . boardToTitle $ bo)
      H.div $
        mapM_ toMarkup items
