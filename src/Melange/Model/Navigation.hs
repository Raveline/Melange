{-# LANGUAGE DeriveGeneric  #-}
module Melange.Model.Navigation
  (
    BoardNavigation(..)
  , emptyNavigation
  ) where

import           Data.Time    (Day)
import qualified Generics.SOP as SOP
import           GHC.Generics

data BoardNavigation =
  BoardNavigation { previous :: Maybe Day
                  , next     :: Maybe Day }
  deriving (Generic)

emptyNavigation :: BoardNavigation
emptyNavigation = BoardNavigation Nothing Nothing

instance SOP.Generic BoardNavigation
instance SOP.HasDatatypeInfo BoardNavigation
