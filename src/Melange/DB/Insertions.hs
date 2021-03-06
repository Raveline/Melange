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
  , updateBoard
  ) where

import           Control.Exception.Base      hiding (catch, throwIO)
import           Control.Exception.Safe
import           Control.Monad               (void)
import           Control.Monad.Base
import           Control.Monad.Trans.Control
import           Data.Int
import           Data.Time
import           Data.UUID                   (UUID)
import           Data.UUID.V4                (nextRandom)
import           Melange.DB.Deletions        (removeBoardAtDay)
import           Melange.DB.Schema           (Schema)
import           Melange.DB.Types            (QueryException (..))
import           Melange.Model               (Board (..), Item (..))
import           Squeal.PostgreSQL           hiding (date)

insertQuote :: Manipulation Schema '[ 'NotNull 'PGuuid, 'Null 'PGtext, 'NotNull 'PGtext, 'Null 'PGtext, 'Null 'PGtext ] '[]
insertQuote = insertRow #quotes
    ( Set (param @1) `As` #quote_id
    :* Set (param @2) `As` #quote_title
    :* Set (param @3) `As` #content
    :* Set (param @4) `As` #quote_source
    :* Set (param @5) `As` #quote_style
    :* Nil )
    OnConflictDoNothing (Returning Nil)

insertQuoteItem :: Manipulation Schema '[ 'NotNull 'PGuuid, 'Null 'PGuuid] '[]
insertQuoteItem = insertRow #items
    ( Set (param @1) `As` #item_id
    :* Set (param @2) `As` #quote_id
    :* Set null_ `As` #image_id
    :* Nil )
    OnConflictDoNothing (Returning Nil)

insertImage :: Manipulation Schema '[ 'NotNull 'PGuuid, 'NotNull 'PGtext, 'Null 'PGtext, 'Null 'PGtext ] '[]
insertImage = insertRow #images
    ( Set (param @1) `As` #image_id
    :* Set (param @2) `As` #filepath
    :* Set (param @3) `As` #image_source
    :* Set (param @4) `As` #image_style
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
    OnConflictDoRaise (Returning Nil)

insertBoardItem :: Manipulation Schema '[ 'NotNull 'PGuuid, 'NotNull 'PGuuid, 'NotNull 'PGint2] '[]
insertBoardItem = insertRow #board_items
    ( Set (param @1) `As` #board_id
    :* Set (param @2) `As` #item_id
    :* Set (param @3) `As` #order
    :* Nil )
    OnConflictDoRaise (Returning Nil)

deleteBoardItems :: Manipulation Schema '[ 'NotNull 'PGuuid ] '[]
deleteBoardItems = deleteFrom #board_items (#board_id .== param @1) (Returning Nil)

newItem :: (MonadBaseControl IO m, MonadPQ Schema m) => Item -> m UUID
newItem (Quote t c so st) = do
    quoteUUID <- liftBase nextRandom
    itemUUID <- liftBase nextRandom
    _ <- manipulateParams insertQuote (quoteUUID, t, c, so, st)
    _ <- manipulateParams insertQuoteItem (itemUUID, Just quoteUUID)
    pure itemUUID
newItem (Image f so st) = do
    imageUUID <- liftBase nextRandom
    itemUUID <- liftBase nextRandom
    _ <- manipulateParams insertImage (imageUUID, f, so, st)
    _ <- manipulateParams insertImageItem (itemUUID, Just imageUUID)
    pure itemUUID

handler :: (MonadPQ Schema m, MonadBaseControl IO m) => ErrorCall -> m a
handler e = liftBase $ print e >> throwIO AlreadyExists

catchLift :: (Exception e, MonadPQ Schema m, MonadBaseControl IO m) => m a -> (e -> m a) -> m a
catchLift action onError =
  control $ \runInIO ->
    runInIO action `catch` (runInIO . onError)

updateBoard :: (MonadBaseControl IO m, MonadPQ Schema m) => Day -> Board -> m UUID
updateBoard day updatedBoard = do
  removeBoardAtDay UpdateNonExistingEntity day
  newBoard updatedBoard

newBoard :: (MonadBaseControl IO m, MonadPQ Schema m) => Board -> m UUID
newBoard Board {..} = (do
    boardId <- liftBase nextRandom
    transactionally_ $ do
      void $ manipulateParams insertBoard (boardId, boardTitle, date)
      uuids <- traverse newItem items
      _ <- manipulateParams deleteBoardItems (Only boardId)
      associateBoardAndItems boardId uuids
    pure boardId) `catchLift` handler

associateBoardAndItems :: (MonadBaseControl IO m, MonadPQ Schema m) => UUID -> [UUID] -> m ()
associateBoardAndItems boardId itemIds =
  let counting = [1..] :: [Int16]
      params = zip3 (repeat boardId) itemIds counting
  in void $ traversePrepared insertBoardItem params
