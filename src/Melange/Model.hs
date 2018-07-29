{-# LANGUAGE DeriveGeneric #-}
module Melange.Model
  (
    Item (..)
  , Quote (..)
  , Image (..)
  , Board (..)
  ) where

import qualified Data.Text    as T
import           Data.Time
import           Data.UUID
import qualified Generics.SOP as SOP
import           GHC.Generics

data Item = ItemQuote UUID Quote | ItemImage UUID Image

data Quote = Quote { quoteId     :: UUID
                   , quoteTitle  :: Maybe T.Text
                   , content     :: T.Text
                   , quoteSource :: Maybe T.Text }
  deriving (Show, Generic)

instance SOP.Generic Quote

data Image = Image { imageId     :: UUID
                   , filepath    :: T.Text
                   , imageSource :: Maybe T.Text }
  deriving (Show, Generic)

instance SOP.Generic Image

data Board = Board { boardTitle :: Maybe T.Text
                   , date       :: UTCTime
                   , items      :: [Item] }
