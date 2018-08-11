{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module Melange.DB.Deletions
  (
    removeAllBoardInfo
  , removeBoardAtDay
  ) where

import           Control.Exception.Safe
import           Control.Monad               (void)
import           Control.Monad.Base
import           Control.Monad.Trans.Control
import           Data.Maybe                  (catMaybes)
import           Data.Time
import           Data.UUID                   (UUID)
import qualified Generics.SOP                as SOP
import           GHC.Generics                hiding (from)
import           Melange.DB.Schema
import           Melange.DB.Selections
import           Melange.DB.Types            (QueryException)
import           Melange.Model               (Board (..))
import           Squeal.PostgreSQL           hiding (date)

deleteBoard :: Manipulation Schema '[ 'NotNull 'PGdate ] '[]
deleteBoard = deleteFrom #boards
  (#date .== param @1) (Returning Nil )

deleteQuote :: Manipulation Schema '[ 'NotNull 'PGuuid ] '[]
deleteQuote = deleteFrom_ #quotes ( #quote_id .== param @1 )

deleteImage :: Manipulation Schema '[ 'NotNull 'PGuuid ] '[]
deleteImage =
  deleteFrom_ #images ( #image_id .== param @1 )

removeBoardAtDay :: (MonadBaseControl IO m, MonadPQ Schema m) => QueryException -> Day -> m ()
removeBoardAtDay e day = do
  oldBoard <- getBoardByDay day
  maybe (liftBase $ throwIO e) removeAllBoardInfo oldBoard

removeBoard :: (MonadBaseControl IO m, MonadPQ Schema m) => Day -> m ()
removeBoard d = void $ manipulateParams deleteBoard (Only d)

removeItem :: (MonadBaseControl IO m, MonadPQ Schema m) => ([UUID], [UUID]) -> m ()
removeItem (quotes, images) = do
  traversePrepared_ deleteQuote (Only <$> quotes)
  traversePrepared_ deleteImage (Only <$> images)

type BoardGraphDeletionResult =
  [
    "imageId" ::: 'Null 'PGuuid
  , "quoteId" ::: 'Null 'PGuuid
  ]

data BoardGraphDeletionRow =
  BoardGraphDeletionRow { imageId :: Maybe UUID
                        , quoteId :: Maybe UUID }
  deriving (Generic)

instance SOP.Generic BoardGraphDeletionRow
instance SOP.HasDatatypeInfo BoardGraphDeletionRow

unzipDeletionRows :: [BoardGraphDeletionRow] -> ([UUID], [UUID])
unzipDeletionRows bdrs =
  let images = catMaybes $ imageId <$> bdrs
      quotes = catMaybes $ quoteId <$> bdrs
  in (quotes, images)

fetchGraph :: Query Schema '[ 'NotNull 'PGdate] BoardGraphDeletionResult
fetchGraph = select
  ( #items ! #image_id `As` #imageId
  :* #items ! #quote_id `As` #quoteId
  :* Nil )
  (from (table #items
       & innerJoin (table #board_items) (#board_items ! #item_id .== #items ! #item_id)
       & innerJoin (table #boards) (#boards ! #board_id .== #board_items ! #board_id ))
  & where_ (#boards ! #date .== param @1))

removeAllBoardInfo :: (MonadBaseControl IO m, MonadPQ Schema m) => Board -> m ()
removeAllBoardInfo Board{..} = do
  results <- runQueryParams fetchGraph (Only date)
  rows <- getRows results
  let graph = unzipDeletionRows rows
  removeBoard date
  removeItem graph
