{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Melange.DB.Insertions
  (
    newItem
  , newBoard
  ) where

import           Control.Monad               (void)
import           Control.Monad.Trans.Control
import           Control.Monad.Base
import           Data.Int
import           Data.UUID                   (UUID)
import           Data.UUID.V4                (nextRandom)
import           Melange.DB.Schema           (Schema)
import           Melange.Model.External      (BoardCreation (..),
                                              ItemCreation (..))
import           Squeal.PostgreSQL           hiding (date)

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

newItem :: (MonadBaseControl IO m, MonadPQ Schema m) => ItemCreation -> m UUID
newItem (QuoteCreation t c s) = do
    quoteUUID <- liftBase nextRandom
    itemUUID <- liftBase nextRandom
    _ <- manipulateParams insertQuote (quoteUUID, t, c, s)
    _ <- manipulateParams insertQuoteItem (itemUUID, Just quoteUUID)
    pure itemUUID
newItem (ImageCreation f s) = do
    imageUUID <- liftBase nextRandom
    itemUUID <- liftBase nextRandom
    _ <- manipulateParams insertImage (imageUUID, f, s)
    _ <- manipulateParams insertImageItem (itemUUID, Just imageUUID)
    pure imageUUID

newBoard :: (MonadBaseControl IO m, MonadPQ Schema m) => BoardCreation -> m UUID
newBoard BoardCreation{..} = do
    boardId <- liftBase nextRandom
    uuids <- traverse newItem items
    _ <- manipulateParams insertBoard (boardId, boardTitle, date)
    _ <- manipulateParams deleteBoardItems (Only boardId)
    associateBoardAndItems boardId uuids
    pure boardId

associateBoardAndItems :: (MonadBaseControl IO m, MonadPQ Schema m) => UUID -> [UUID] -> m ()
associateBoardAndItems boardId itemIds =
  let counting = [1..] :: [Int16]
      params = zip3 (repeat boardId) itemIds counting
  in void $ traversePrepared insertBoardItem params
