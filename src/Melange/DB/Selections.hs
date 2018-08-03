{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Melange.DB.Selections
  (
    getBoardByDay
  ) where

import           Control.Monad.Base
import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.Trans.Control
import           Data.Maybe                  (catMaybes)
import           Data.Text                   (Text)
import           Data.Time                   (Day)
import           Data.UUID                   (UUID)
import           Debug.Trace
import qualified Generics.SOP                as SOP
import           GHC.Generics                hiding (from)
import           Melange.DB.Schema
import           Melange.Model.Internal      (Board (..), Image (..), Item (..),
                                              Quote (..), itemId)
import           Squeal.PostgreSQL
import           Squeal.PostgreSQL.Pool

type BoardQueryResult =
  [
    "boardId" ::: 'NotNull 'PGuuid
  , "title"    ::: 'Null 'PGtext
  , "date"     ::: 'NotNull 'PGdate
  , "itemId" ::: 'NotNull 'PGuuid
  , "quoteId" ::: 'Null 'PGuuid
  , "quoteTitle" ::: 'Null 'PGtext
  , "content"   ::: 'Null 'PGtext
  , "quoteSource" ::: 'Null 'PGtext
  , "imageId" ::: 'Null 'PGuuid
  , "filepath"   :::  'Null 'PGtext
  , "imageSource" ::: 'Null 'PGtext
  ]

data BoardQueryRow = BoardQueryRow
  { boardId     :: UUID
  , title       :: Maybe Text
  , date        :: Day
  , itemId      :: UUID
  , quoteId     :: Maybe UUID
  , quoteTitle  :: Maybe Text
  , content     :: Maybe Text
  , quoteSource :: Maybe Text
  , imageId     :: Maybe UUID
  , filepath    :: Maybe Text
  , imageSource :: Maybe Text }
  deriving (Generic, Show)

instance SOP.Generic BoardQueryRow
instance SOP.HasDatatypeInfo BoardQueryRow

splitter :: BoardQueryRow -> (Board, (Maybe Item, Maybe Item))
splitter bqr@BoardQueryRow{..} =
  let board = Board boardId title date []
      quote =
        ItemQuote <$> pure itemId
                  <*> (Quote <$> quoteId <*> pure quoteTitle <*> content <*> pure quoteSource)
      image =
        ItemImage <$> pure itemId
                  <*> (Image <$> imageId <*> filepath <*> pure imageSource)
  in traceShow bqr $ (board, (quote, image))

joiner :: [(Board, (Maybe Item, Maybe Item))] -> Maybe Board
joiner [] = Nothing
joiner rows@((b,_):_) =
  let extractItems (i, q) = catMaybes [i, q]
      allItems = concatMap (extractItems . snd) rows
  in pure $ b { items = allItems }

type BoardQuery params = Query Schema params BoardQueryResult

selectBoardByDay :: BoardQuery '[ 'NotNull 'PGdate ]
selectBoardByDay = select
  (  #b ! #board_id `As` #boardId
     :* #b ! #title
     :* #b ! #date
     :* #it ! #item_id `As` #itemId
     :* #q ! #quote_id `As` #quoteId
     :* #q ! #quote_title `As` #quoteTitle
     :* #q ! #content
     :* #q ! #quote_source `As` #quoteSource
     :* #im ! #image_id `As` #imageId
     :* #im ! #filepath
     :* #im ! #image_source `As` #imageSource
     :* Nil)
  (from (table (#boards `As` #b)
          & innerJoin (table (#board_items `As` #bi)) (#b ! #board_id .== #bi ! #board_id)
          & innerJoin (table (#items `As` #it)) (#it ! #item_id .== #bi ! #item_id)
          & leftOuterJoin (table (#quotes `As` #q)) (fromNull false (#it ! #quote_id .== notNull (#q ! #quote_id)))
          & leftOuterJoin (table (#images `As` #im)) (fromNull false (#it ! #image_id .== notNull (#im ! #image_id))))
    & where_ (#b ! #date .== param @1))

getBoardByDay :: (MonadBaseControl IO m, MonadPQ Schema m) => Day -> m (Maybe Board)
getBoardByDay day = do
  res <- runQueryParams selectBoardByDay (Only day)
  joiner . fmap splitter <$> getRows res
