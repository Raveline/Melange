{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Melange.Model.Board
  (
    Item(..)
  , Board (..)
  ) where

import           Data.Aeson
import           Data.Text                   (Text)
import           Data.Time                   (Day)
import           GHC.Generics
import           Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

data Item = Quote { quoteTitle  :: Maybe Text
                  , content     :: Text
                  , quoteSource :: Maybe Text
                  , quoteStyle  :: Maybe Text
                  }
          | Image { filepath    :: Text
                  , imageSource :: Maybe Text
                  , imageStyle  :: Maybe Text
                  }
  deriving (Show, Generic, Eq, ToJSON, FromJSON)

data Board = Board { boardTitle :: Maybe Text
                   , date       :: Day
                   , items      :: [Item] }
  deriving (Show, Generic, Eq, ToJSON, FromJSON)

itemDiv :: Maybe Text -> Text -> (Html -> Html)
itemDiv styl def =
  let classes = maybe def ( (def <> " ") <>) styl
  in H.div ! A.class_ (textValue classes)

instance ToMarkup Item where
  toMarkup Quote{..} =
    itemDiv quoteStyle "quote" $ do
       maybe mempty ( (h2 ! A.class_ "quote-title") . toHtml) quoteTitle
       H.div ! A.class_ "quote-content" $
         p (toHtml content)
       maybe mempty ( (p ! A.class_ "quote-source") . toHtml) quoteSource
  toMarkup Image{..} =
    itemDiv imageStyle "image" $ do
      H.img ! A.src ("/static/" <> textValue filepath)
      maybe mempty ( (p ! A.class_ "image-source") . toHtml) imageSource
