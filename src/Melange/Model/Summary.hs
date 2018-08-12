{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
module Melange.Model.Summary
  (
    BoardSummaryItem (..)
  , BoardSummary (..)
  ) where

import           Data.Aeson
import           Data.Text    (Text)
import           Data.Time    (Day)
import qualified Generics.SOP as SOP
import           GHC.Generics

data BoardSummaryItem =
  BoardSummaryItem { date       :: Day
                   , boardTitle :: Maybe Text }
  deriving (Generic, Show, Eq, ToJSON)

instance SOP.Generic BoardSummaryItem
instance SOP.HasDatatypeInfo BoardSummaryItem

data BoardSummary =
  BoardSummary { boards        :: [BoardSummaryItem]
               , currentPage   :: Int
               , numberOfPages :: Int }
  deriving (Generic, Show, Eq, ToJSON)

