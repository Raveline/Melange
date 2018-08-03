{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE FlexibleContexts #-}
module Melange.DB.Insertions
  (
    newItem
  , newBoard
  ) where

import           Control.Monad.Trans.Control
import           Control.Monad          (void)
import           Data.Foldable          (traverse_)
import           Data.Int
import qualified GHC.Generics           as GHC
import           Melange.DB.Schema      (Schema)
import           Melange.Model.Internal (Board (..), Image (..), Item (..),
                                         Quote (..), itemId)
import           Squeal.PostgreSQL

insertQuote :: Manipulation Schema '[ 'NotNull 'PGuuid, 'Null 'PGtext, 'NotNull 'PGtext, 'Null 'PGtext ] '[]
insertQuote = insertRow #quotes
    ( Set (param @1) `As` #quote_id
    :* Set (param @2) `As` #quote_title
    :* Set (param @3) `As` #content
    :* Set (param @4) `As` #quote_source
    :* Nil )
    OnConflictDoNothing (Returning Nil)

insertQuoteItem :: Manipulation Schema '[ 'NotNull 'PGuuid, 'Null 'PGuuid] '[]
insertQuoteItem = insertRow #items
    ( Set (param @1) `As` #item_id
    :* Set (param @2) `As` #quote_id
    :* Set null_ `As` #image_id
    :* Nil )
    OnConflictDoNothing (Returning Nil)

insertImage :: Manipulation Schema '[ 'NotNull 'PGuuid, 'NotNull 'PGtext, 'Null 'PGtext ] '[]
insertImage = insertRow #images
    ( Set (param @1) `As` #image_id
    :* Set (param @2) `As` #filepath
    :* Set (param @3) `As` #image_source
    :* Nil )
    OnConflictDoNothing (Returning Nil)

insertImageItem :: Manipulation Schema '[ 'NotNull 'PGuuid, 'Null 'PGuuid] '[]
insertImageItem = insertRow #items
    ( Set (param @1) `As` #item_id
    :* Set null_ `As` #quote_id
    :* Set (param @2) `As` #image_id
    :* Nil )
    OnConflictDoNothing (Returning Nil)

insertBoard :: Manipulation Schema '[ 'NotNull 'PGuuid, 'Null 'PGtext, 'NotNull 'PGdate ] '[]
insertBoard = insertRow #boards
    ( Set (param @1) `As` #board_id
    :* Set (param @2) `As` #title
    :* Set (param @3) `As` #date
    :* Nil )
    OnConflictDoNothing (Returning Nil)

insertBoardItem :: Manipulation Schema '[ 'NotNull 'PGuuid, 'NotNull 'PGuuid, 'NotNull 'PGint2] '[]
insertBoardItem = insertRow #board_items
    ( Set (param @1) `As` #board_id
    :* Set (param @2) `As` #item_id
    :* Set (param @3) `As` #order
    :* Nil )
    OnConflictDoNothing (Returning Nil)

deleteBoardItems :: Manipulation Schema '[ 'NotNull 'PGuuid ] '[]
deleteBoardItems = deleteFrom #board_items (#board_id .== param @1) (Returning Nil)

newItem :: (MonadBaseControl IO m, MonadPQ Schema m) => Item -> m ()
newItem (ItemQuote uuid q) =
  void $ manipulateParams insertQuote q
        >> manipulateParams insertQuoteItem (uuid, Just $ quoteId q)
newItem (ItemImage uuid i) =
  void $ manipulateParams insertImage i
        >> manipulateParams insertImageItem (uuid, Just $ imageId i)

newBoard :: (MonadBaseControl IO m, MonadPQ Schema m) => Board -> m ()
newBoard b@Board{..} =
  void $ traverse_ newItem items
      >> manipulateParams insertBoard (boardId, boardTitle, date)
      >> manipulateParams deleteBoardItems (Only boardId)
      >> associateBoardAndItems b

associateBoardAndItems :: (MonadBaseControl IO m, MonadPQ Schema m) => Board -> m ()
associateBoardAndItems Board{..} =
  let itemsUUID = itemId <$> items
      counting = [1..] :: [Int16]
      params = zip3 (repeat boardId) itemsUUID counting
  in void $ traversePrepared insertBoardItem params
