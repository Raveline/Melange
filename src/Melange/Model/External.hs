{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Melange.Model.External
  (
    Item (..)
  , Board (..)
  , ItemCreation (..)
  , BoardCreation (..)
  ) where

import           Data.Aeson
import qualified Data.Text    as T
import           Data.Time    (Day)
import           Data.UUID    (UUID)
import           GHC.Generics

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
