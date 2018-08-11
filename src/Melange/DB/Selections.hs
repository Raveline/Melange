{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE TypeOperators       #-}
module Melange.DB.Selections
  (
    getBoardByDay
  , getLatestBoard
  , getSummaryAtPage
  ) where

import           Control.Monad.Trans.Control
import           Data.Int                    (Int64)
import           Data.Maybe                  (catMaybes)
import           Data.Text                   (Text)
import           Data.Time                   (Day)
import           Data.UUID                   (UUID)
import           Data.Word                   (Word64)
import qualified Generics.SOP                as SOP
import           GHC.Generics                hiding (from)
import           Melange.DB.Schema
import           Melange.Model               (Board (..), BoardSummary (..),
                                              Item (..))
import           Squeal.PostgreSQL           hiding (date)

type BoardQueryResult =
  [
    "boardId" ::: 'NotNull 'PGuuid
  , "title"    ::: 'Null 'PGtext
  , "date"     ::: 'NotNull 'PGdate
  , "itemId" ::: 'NotNull 'PGuuid
  , "quoteTitle" ::: 'Null 'PGtext
  , "content"   ::: 'Null 'PGtext
  , "quoteSource" ::: 'Null 'PGtext
  , "filepath"   :::  'Null 'PGtext
  , "imageSource" ::: 'Null 'PGtext
  ]

type SummaryPageResult =
  [
    "date" ::: 'NotNull 'PGdate
  , "boardTitle" ::: 'Null 'PGtext
  ]

data BoardQueryRow = BoardQueryRow
  { boardId     :: UUID
  , title       :: Maybe Text
  , date        :: Day
  , itemId      :: UUID
  , quoteTitle  :: Maybe Text
  , content     :: Maybe Text
  , quoteSource :: Maybe Text
  , filepath    :: Maybe Text
  , imageSource :: Maybe Text }
  deriving (Generic, Show)

instance SOP.Generic BoardQueryRow
instance SOP.HasDatatypeInfo BoardQueryRow

splitter :: BoardQueryRow -> (Board, (Maybe Item, Maybe Item))
splitter BoardQueryRow{..} =
  let board = Board title date []
      quote =
        Quote <$> pure quoteTitle <*> content <*> pure quoteSource
      image =
        Image <$> filepath <*> pure imageSource
  in (board, (quote, image))

joiner :: [(Board, (Maybe Item, Maybe Item))] -> Maybe Board
joiner [] = Nothing
joiner rows@((b,_):_) =
  let extractItems (i, q) = catMaybes [i, q]
      allItems = concatMap (extractItems . snd) rows
  in pure $ b { items = allItems }

type BoardQuery params = Query Schema params BoardQueryResult

type  BoardSelection  =
    '[
     "b" :::
      '["board_id" ::: 'NotNull 'PGuuid,
        "title" ::: 'Null 'PGtext,
        "date" ::: 'NotNull 'PGdate]
    , "bi" :::
      '["board_id" ::: 'NotNull 'PGuuid
       , "item_id" ::: 'NotNull 'PGuuid
       , "order" ::: 'NotNull 'PGint2]
    , "it" :::
      '["item_id" ::: 'NotNull 'PGuuid
       , "quote_id" ::: 'Null 'PGuuid
       , "image_id" ::: 'Null 'PGuuid]
    , "q" :::
      '[ "quote_id" ::: 'Null 'PGuuid
       , "quote_title" ::: 'Null 'PGtext
       , "content" ::: 'Null 'PGtext
       , "quote_source" ::: 'Null 'PGtext]
    , "im" :::
      '["image_id" ::: 'Null 'PGuuid
       , "filepath" ::: 'Null 'PGtext
       , "image_source" ::: 'Null 'PGtext]
    ]

boardTables :: FromClause Schema (param :: [NullityType]) BoardSelection
boardTables =
  table (#boards `As` #b)
    & innerJoin (table (#board_items `As` #bi)) (#b ! #board_id .== #bi ! #board_id)
    & innerJoin (table (#items `As` #it)) (#it ! #item_id .== #bi ! #item_id)
    & leftOuterJoin (table (#quotes `As` #q)) (fromNull false (#it ! #quote_id .== notNull (#q ! #quote_id)))
    & leftOuterJoin (table (#images `As` #im)) (fromNull false (#it ! #image_id .== notNull (#im ! #image_id)))

boardFields :: NP (Aliased (Expression Schema (Join BoardSelection a) 'Ungrouped param)) BoardQueryResult
boardFields =
     #b ! #board_id `As` #boardId
     :* #b ! #title
     :* #b ! #date
     :* #it ! #item_id `As` #itemId
     :* #q ! #quote_title `As` #quoteTitle
     :* #q ! #content
     :* #q ! #quote_source `As` #quoteSource
     :* #im ! #filepath
     :* #im ! #image_source `As` #imageSource
     :* Nil

selectBoardByDay :: BoardQuery '[ 'NotNull 'PGdate ]
selectBoardByDay = select
  boardFields
  (from boardTables
    & where_ (#b ! #date .== param @1)
    & orderBy [(#bi ! #order) & Asc])

latestBoardId :: Query Schema '[] '["maxDate" ::: 'NotNull 'PGdate ]
latestBoardId =
  select
    ( max_ (#boards ! #date) `As` #maxDate
     :* Nil
    )
    (from (table #boards) & groupBy Nil)

selectLatestBoard :: BoardQuery '[]
selectLatestBoard = select
  boardFields
  (from (boardTables
    & innerJoin (subquery (latestBoardId `As` #lbi)) (#b ! #date .== #lbi ! #maxDate))
    & orderBy [(#bi ! #order) & Asc])

getBoardByDay :: (MonadBaseControl IO m, MonadPQ Schema m) => Day -> m (Maybe Board)
getBoardByDay day = do
  res <- runQueryParams selectBoardByDay (Only day)
  joiner . fmap splitter <$> getRows res

getLatestBoard :: (MonadBaseControl IO m, MonadPQ Schema m) => m (Maybe Board)
getLatestBoard = do
  res <- runQueryParams selectLatestBoard ()
  joiner . fmap splitter <$> getRows res

selectSummaryPage :: Word64 -> Query Schema '[] SummaryPageResult
selectSummaryPage off =
  select ( #boards ! #date
         :* #boards ! #title `As` #boardTitle
         :* Nil )
  (from (table #boards)
   & orderBy [(#boards ! #date) & Desc]
   & limit (fromIntegral summaryPageSize) & offset off)

countBoardsQuery :: Query Schema '[] '[ "fromOnly" :=> 'NotNull 'PGint8 ]
countBoardsQuery =
  select
    (countStar `As` #fromOnly :* Nil)
    (from (table #boards) & groupBy Nil)

summaryPageSize :: Int
summaryPageSize = 10

pageCount :: Int -> Int
pageCount n =
  let ratio = (fromIntegral n :: Double) / (fromIntegral summaryPageSize :: Double)
  in ceiling ratio

countBoards :: (MonadBaseControl IO m, MonadPQ Schema m) => m Int64
countBoards = fromOnly <$> (runQuery countBoardsQuery >>= getRow 0)

getSummaryAtPage :: (MonadBaseControl IO m, MonadPQ Schema m) => Int -> m BoardSummary
getSummaryAtPage currentPage =
  let offset' = fromIntegral $ currentPage * summaryPageSize
      getItems = runQueryParams (selectSummaryPage offset') () >>= getRows
      getCount = fromIntegral <$> countBoards
      numberOfPages = pageCount <$> getCount
  in BoardSummary <$> getItems <*> pure currentPage <*> numberOfPages
