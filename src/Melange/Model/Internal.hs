{-# LANGUAGE DeriveGeneric #-}
module Melange.Model.Internal
  (
    Item (..)
  , Quote (..)
  , Image (..)
  , Board (..)
  , itemId
  ) where

import qualified Data.Text    as T
import           Data.Time    (Day)
import           Data.UUID    (UUID)
import qualified Generics.SOP as SOP
import           GHC.Generics

-- TODO: Stop duplicating everything
-- and make sure we drop this model.
-- UUID for quotes and images could be added
-- at insertion.

data Item = ItemQuote UUID Quote | ItemImage UUID Image
  deriving (Show, Eq)

itemId :: Item -> UUID
itemId (ItemQuote u _) = u
itemId (ItemImage u _) = u

data Quote = Quote { quoteId     :: UUID
                   , quoteTitle  :: Maybe T.Text
                   , content     :: T.Text
                   , quoteSource :: Maybe T.Text }
  deriving (Show, Generic, Eq)

instance SOP.Generic Quote

data Image = Image { imageId     :: UUID
                   , filepath    :: T.Text
                   , imageSource :: Maybe T.Text }
  deriving (Show, Generic, Eq)

instance SOP.Generic Image

data Board = Board { boardId    :: UUID
                   , boardTitle :: Maybe T.Text
                   , date       :: Day
                   , items      :: [Item] }
           deriving (Show, Generic, Eq)
