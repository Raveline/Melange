{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module Melange.DB.Deletions
  (
    removeBoard
  ) where

import           Control.Monad               (void)
import           Control.Monad.Trans.Control
import           Data.Time
import           Data.UUID                   (UUID)
import           Melange.DB.Schema
import           Squeal.PostgreSQL

deleteBoard :: Manipulation Schema '[ 'NotNull 'PGdate ] '[ "fromOnly" ::: 'NotNull 'PGuuid ]
deleteBoard = deleteFrom #boards
  (#date .== param @1) (Returning ( #board_id `as` #fromOnly))

deleteBoardItem :: Manipulation Schema '[ 'NotNull 'PGuuid ] '[]
deleteBoardItem = deleteFrom #board_items
  (#board_id .== param @1) (Returning Nil)

removeBoard :: (MonadBaseControl IO m, MonadPQ Schema m) => Day -> m ()
removeBoard d = do
  res <- manipulateParams deleteBoard (Only d)
  (rows :: [Only UUID]) <- getRows res
  void $ traversePrepared deleteBoardItem rows
