{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
module Melange.Model
  (
    Item (..)
  , Board (..)
  , BoardSummaryItem (..)
  , BoardSummary (..)
  ) where

import           Data.Aeson
import qualified Data.Text                   as T
import           Data.Time                   (Day)
import qualified Generics.SOP                as SOP
import           GHC.Generics
import           Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

data Item = Quote { quoteTitle  :: Maybe T.Text
                  , content     :: T.Text
                  , quoteSource :: Maybe T.Text }
          | Image { filepath    :: T.Text
                  , imageSource :: Maybe T.Text }
  deriving (Show, Generic, Eq, ToJSON, FromJSON)

data Board = Board { boardTitle :: Maybe T.Text
                   , date       :: Day
                   , items      :: [Item] }
  deriving (Show, Generic, Eq, ToJSON, FromJSON)

data BoardSummaryItem =
  BoardSummaryItem { date       :: Day
                   , boardTitle :: Maybe T.Text }
  deriving (Generic, Show, Eq, ToJSON)

instance SOP.Generic BoardSummaryItem
instance SOP.HasDatatypeInfo BoardSummaryItem

data BoardSummary =
  BoardSummary { boards        :: [BoardSummaryItem]
               , currentPage   :: Int
               , numberOfPages :: Int }
  deriving (Generic, Show, Eq, ToJSON)


instance ToMarkup Item where
  toMarkup Quote{..} =
    H.div ! A.class_ "quote" $ do
       maybe mempty ( (h2 ! A.class_ "quote-title") . toHtml) quoteTitle
       H.div ! A.class_ "quote-content" $
         p (toHtml content)
       maybe mempty ( (p ! A.class_ "quote-source") . toHtml) quoteSource
  toMarkup Image{..} =
    H.div ! A.class_ "image" $ do
      H.img ! A.src ("/static/" <> textValue filepath)
      maybe mempty ( (p ! A.class_ "image-source") . toHtml) imageSource
