{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
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
import           Data.Foldable               (traverse_)
import           Data.Time
import           Melange.DB.Schema
import           Melange.DB.Selections
import           Melange.DB.Types            (QueryException)
import           Melange.Model               (Board (..), Item (..))
import           Squeal.PostgreSQL           hiding (date)
import           Squeal.PostgreSQL.Render

deleteBoard :: Manipulation Schema '[ 'NotNull 'PGdate ] '[]
deleteBoard = deleteFrom #boards
  (#date .== param @1) (Returning Nil )

deleteQuote :: Manipulation Schema '[ 'NotNull 'PGuuid ] '[]
deleteQuote = deleteFrom_ #quotes
  ( notNull #quote_id `in_` fetchQuoteId)

deleteImage :: Manipulation Schema '[ 'NotNull 'PGuuid ] '[]
deleteImage =
  deleteFrom_ #images
    ( notNull #image_id `in_` fetchImageId)

in_
  :: Expression schema from grp params ty
  -> Query schema params '[alias ::: ty]
  -> Condition schema from grp params
in_ x q = UnsafeExpression $
  renderExpression x <+> "IN" <+> parenthesized (renderQuery q)

fetchImageId :: Query Schema '[ 'NotNull 'PGuuid ] '[ "image_id" ::: 'Null 'PGuuid ]
fetchImageId = select (#items ! #image_id)
  (from (table #items)
  & where_ (#item_id .== param @1))

fetchQuoteId :: Query Schema '[ 'NotNull 'PGuuid ] '[ "quote_id" ::: 'Null 'PGuuid ]
fetchQuoteId = select (#items ! #quote_id)
  (from (table #items)
  & where_ (#item_id .== param @1))

removeBoardAtDay :: (MonadBaseControl IO m, MonadPQ Schema m) => QueryException -> Day -> m ()
removeBoardAtDay e day = do
  oldBoard <- getBoardByDay day
  maybe (liftBase $ throwIO e) removeAllBoardInfo oldBoard

removeBoard :: (MonadBaseControl IO m, MonadPQ Schema m) => Day -> m ()
removeBoard d = void $ manipulateParams deleteBoard (Only d)

removeItem :: (MonadBaseControl IO m, MonadPQ Schema m) => Item -> m ()
removeItem Quote{..} = void $ manipulateParams deleteQuote $ Only itemId
removeItem Image{..} = void $ manipulateParams deleteImage $ Only itemId

removeAllBoardInfo :: (MonadBaseControl IO m, MonadPQ Schema m) => Board -> m ()
removeAllBoardInfo Board{..} = do
  removeBoard date
  traverse_ removeItem items
