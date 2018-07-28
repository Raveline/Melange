module Melange.Model
  (
    Item (..)
  , Quote (..)
  , Image (..)
  , Board (..)
  ) where

import qualified Data.Text as T
import           Data.Time

data Item = ItemQuote Quote | ItemImage Image

data Quote = Quote { quoteTitle  :: Maybe T.Text
                   , content     :: T.Text
                   , quoteSource :: Maybe T.Text }

data Image = Image { filepath    :: T.Text
                   , imageSource :: Maybe T.Text }

data Board = Board { boardTitle :: Maybe T.Text
                   , date       :: UTCTime
                   , items      :: [Item] }
