{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
module Melange.Model.External
  (
    Item (..)
  , Board (..)
  ) where

import           Data.Aeson
import qualified Data.Text              as T
import           Data.Time              (Day)
import           Data.UUID              (UUID)
import qualified Generics.SOP           as SOP
import           GHC.Generics
import qualified Melange.Model.Internal as I

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

