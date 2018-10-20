{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
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
  , getBoardNavigationAt
  , getBoardPage
  , nextAndPrevious
  ) where

import           Control.Monad.Trans.Control
import           Data.Int                    (Int64)
import           Data.Maybe                  (catMaybes, fromMaybe)
import           Data.Text                   (Text)
import           Data.Time                   (Day)
import           Data.UUID                   (UUID)
import           Data.Word                   (Word64)
import qualified Generics.SOP                as SOP
import           GHC.Generics                hiding (from)
import           Melange.DB.Schema
import           Melange.Model               (Board (..), Item (..))
import qualified Melange.Model.Navigation    as N (BoardNavigation (..),
                                                   emptyNavigation)
import qualified Melange.Model.Summary       as S (BoardSummary (..))
import           Melange.View.Index          (BoardPage (..))
import           Safe                        (headMay)
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
  , "quoteStyle" ::: 'Null 'PGtext
  , "filepath"   :::  'Null 'PGtext
  , "imageSource" ::: 'Null 'PGtext
  , "imageStyle" ::: 'Null 'PGtext
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
  , quoteStyle  :: Maybe Text
  , filepath    :: Maybe Text
  , imageSource :: Maybe Text
  , imageStyle  :: Maybe Text
  }
  deriving (Generic, Show)

instance SOP.Generic BoardQueryRow
instance SOP.HasDatatypeInfo BoardQueryRow

splitter :: BoardQueryRow -> (Board, (Maybe Item, Maybe Item))
splitter BoardQueryRow{..} =
  let board = Board title date []
      quote =
        Quote <$> pure quoteTitle <*> content <*> pure quoteSource <*> pure quoteStyle
      image =
        Image <$> filepath <*> pure imageSource <*> pure imageStyle
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
       , "quote_source" ::: 'Null 'PGtext
       , "quote_style" ::: 'Null 'PGtext]
    , "im" :::
      '["image_id" ::: 'Null 'PGuuid
       , "filepath" ::: 'Null 'PGtext
       , "image_source" ::: 'Null 'PGtext
       , "image_style" ::: 'Null 'PGtext]
    ]

boardTables :: FromClause Schema (param :: [NullityType]) BoardSelection
boardTables =
  table (#boards `As` #b)
    & innerJoin (table (#board_items `As` #bi)) (#b ! #board_id .== #bi ! #board_id)
    & innerJoin (table (#items `As` #it)) (#it ! #item_id .== #bi ! #item_id)
    & leftOuterJoin (table (#quotes `As` #q)) (#it ! #quote_id .== notNull (#q ! #quote_id))
    & leftOuterJoin (table (#images `As` #im)) (#it ! #image_id .== notNull (#im ! #image_id))

boardFields :: NP (Aliased (Expression Schema (Join BoardSelection a) 'Ungrouped param)) BoardQueryResult
boardFields =
     #b ! #board_id `As` #boardId
     :* #b ! #title
     :* #b ! #date
     :* #it ! #item_id `As` #itemId
     :* #q ! #quote_title `As` #quoteTitle
     :* #q ! #content
     :* #q ! #quote_source `As` #quoteSource
     :* #q ! #quote_style `As` #quoteStyle
     :* #im ! #filepath
     :* #im ! #image_source `As` #imageSource
     :* #im ! #image_style `As` #imageStyle
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
  res <- runQuery selectLatestBoard
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

getSummaryAtPage :: (MonadBaseControl IO m, MonadPQ Schema m) => Int -> m S.BoardSummary
getSummaryAtPage currentPage =
  let offset' = fromIntegral $ currentPage * summaryPageSize
      getItems = runQueryParams (selectSummaryPage offset') () >>= getRows
      getCount = fromIntegral <$> countBoards
      numberOfPages = pageCount <$> getCount
  in S.BoardSummary <$> getItems <*> pure currentPage <*> numberOfPages

type NextAndPrevious =
  [
    "previous" ::: 'Null 'PGdate
  , "next" ::: 'Null 'PGdate
  ]

type ChronosReturnType =
  ("ref" ::: 'NotNull 'PGdate) ': NextAndPrevious

getBoardNavigationAt :: (MonadBaseControl IO m, MonadPQ Schema m) => Board -> m N.BoardNavigation
getBoardNavigationAt (Board _ d _) =
  let fromNoNav = fromMaybe N.emptyNavigation
  in fromNoNav . headMay <$> (runQueryParams nextAndPrevious (Only d) >>= getRows)

nextAndPrevious :: Query Schema '[ 'NotNull 'PGdate ] NextAndPrevious
nextAndPrevious =
  select
    ( #chronos ! #previous
    :* #chronos ! #next
    :* Nil )
    (from (subquery (chronos `As` #chronos)) & where_ (#chronos ! #ref .== param @1))

chronos :: Query Schema '[ 'NotNull 'PGdate ] ChronosReturnType
chronos =
 select
   ( #boards ! #date `As` #ref
   :* previousWF `As` #previous
   :* nextWF `As` #next
   :* Nil)
   (from (table #boards))

nextWF :: Expression schema relations grouping params ty
nextWF = UnsafeExpression " lag(date) over (order by date asc rows between current row and unbounded following)"

previousWF :: Expression schema relations grouping params ty
previousWF = UnsafeExpression " lead(date) over (order by date asc rows between current row and unbounded following)"

getBoardPage :: (MonadBaseControl IO m, MonadPQ Schema m) =>
  m (Maybe Board) -> m BoardPage
getBoardPage query = do
  foundBoard <- query
  maybe (pure $ BoardPage Nothing N.emptyNavigation)
    (fmap (BoardPage foundBoard) . getBoardNavigationAt) foundBoard
