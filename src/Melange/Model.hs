{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
module Melange.Model
  (
    Item (..)
  , Board (..)
  , ItemCreation (..)
  , BoardCreation (..)
  , boardToCreation
  ) where

import           Data.Aeson
import qualified Data.Text                   as T
import           Data.Time                   (Day)
import           Data.UUID                   (UUID)
import           GHC.Generics
import           Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

data Item = Quote { itemId      :: UUID
                  , quoteTitle  :: Maybe T.Text
                  , content     :: T.Text
                  , quoteSource :: Maybe T.Text }
          | Image { itemId      :: UUID
                  , filepath    :: T.Text
                  , imageSource :: Maybe T.Text }
  deriving (Show, Generic, Eq, ToJSON, FromJSON)

data Board = Board { boardId    :: UUID
                   , boardTitle :: Maybe T.Text
                   , date       :: Day
                   , items      :: [Item] }
           deriving (Show, Generic, Eq, ToJSON, FromJSON)

data ItemCreation =
  QuoteCreation { quoteTitle  :: Maybe T.Text
                , content     :: T.Text
                , quoteSource :: Maybe T.Text }
  | ImageCreation { filepath    :: T.Text
                  , imageSource :: Maybe T.Text }
           deriving (Generic, Show, Eq, FromJSON)

data BoardCreation =
  BoardCreation { boardTitle :: Maybe T.Text
                , date       :: Day
                , items      :: [ItemCreation] }
           deriving (Generic, Show, Eq, FromJSON)

itemToCreation :: Item -> ItemCreation
itemToCreation Quote{..} = QuoteCreation{..}
itemToCreation Image{..} = ImageCreation{..}

boardToCreation :: Board -> BoardCreation
boardToCreation (Board _ boardTitle date is) =
  let items = itemToCreation <$> is
  in BoardCreation{..}

instance ToMarkup Item where
  toMarkup Quote{..} =
    H.div ! A.class_ "quote" $ do
       maybe mempty ( (h2 ! A.class_ "quote-title") . toHtml) quoteTitle
       H.div ! A.class_ "quote-content" $
         p (toHtml content)
       maybe mempty ( (p ! A.class_ "quote-source") . toHtml) quoteSource
  toMarkup Image{..} = undefined
