{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Spec where

import           Control.Monad               (void)
import           Control.Monad.Trans.Control
import           Data.Aeson
import           Data.ByteString.Char8       (ByteString)
import qualified Data.ByteString.Lazy        as LBS
import           Data.Either                 (isRight)
import           Data.Int                    (Int64)
import           Data.Maybe                  (fromMaybe)
import           Data.String.QQ
import           Data.Text                   (Text)
import           Data.Time
import           Melange.DB.Deletions
import           Melange.DB.Insertions
import           Melange.DB.Schema
import           Melange.DB.Selections
import           Melange.DB.Types
import           Melange.Model
import           Squeal.PostgreSQL           hiding (date)
import           Squeal.PostgreSQL.Migration
import           Test.Hspec

fixtureQuote1 :: Item
fixtureQuote1 =
  Quote
    Nothing
    "Happy families are all alike; every unhappy family is unhappy in its own way"
    Nothing

fixtureQuote2 :: Item
fixtureQuote2 =
  Quote
    (Just "Oscar being Oscar")
    "To lose one parent may be regarded as a misfortune; to lose both looks like carelessness."
    Nothing

fixtureQuote3 :: Item
fixtureQuote3 =
  Quote
    Nothing
    "The beginnings of all things are small."
    (Just "Cicero")

fixtureQuote4 :: Item
fixtureQuote4 =
  Quote
    Nothing
    "The only joy in life is to begin"
    (Just "Cesar Pavese")

fixtureImage1 :: Item
fixtureImage1 =
  Image
    "Some filepath" (Just "Some source")

fixtureBoard :: Board
fixtureBoard =
  Board
    (Just "On family")
    (fromGregorian 1998 12 12)
    [fixtureQuote1, fixtureQuote2]

fixtureBoard2 :: Board
fixtureBoard2 =
  Board
    (Just "On beginnings")
    (fromGregorian 2018 8 1)
    [fixtureQuote3, fixtureQuote4]

fixtureBoard3 :: Board
fixtureBoard3 =
  Board
    (Just "On images")
    (fromGregorian 2000 1 1)
    [fixtureImage1]

shouldCorrespondTo :: (HasCallStack) => Maybe Board -> Board -> Expectation
shouldCorrespondTo Nothing _ = expectationFailure "Query had no result"
shouldCorrespondTo (Just b1) b2 = b1 `shouldBe` b2

connectionString :: ByteString
connectionString = "host=localhost port=5432 dbname=melangetest user=melange password=melange"

type CountQuery = Query Schema '[] '["fromOnly" ::: 'NotNull 'PGint8]

countT :: Has tab schema ('Table table) =>
  Aliased Alias (alias ::: tab) -> Query schema params '[ "fromOnly" :=> 'NotNull 'PGint8 ]
countT t =
  select
    (countStar `As` #fromOnly :* Nil)
    (from (table t) & groupBy Nil)

queryNumber :: (MonadBaseControl IO m, MonadPQ Schema m) => CountQuery -> m Int
queryNumber q =
  let asInt64 :: (MonadBaseControl IO m, MonadPQ Schema m) => m Int64
      asInt64 = fromOnly <$> (runQuery q >>= getRow 0)
  in fromIntegral <$> asInt64

queryNumberOfQuotes :: (MonadBaseControl IO m, MonadPQ Schema m) => m Int
queryNumberOfQuotes =
  queryNumber $ countT #quotes

queryNumberOfItems :: (MonadBaseControl IO m, MonadPQ Schema m) => m Int
queryNumberOfItems =
  queryNumber $ countT #items

queryNumberOfImages :: (MonadBaseControl IO m, MonadPQ Schema m) => m Int
queryNumberOfImages =
  queryNumber $ countT #images

queryNumberOfBoardItems :: (MonadBaseControl IO m, MonadPQ Schema m) => m Int
queryNumberOfBoardItems =
  queryNumber $ countT #board_items

exampleJsonSource :: LBS.ByteString
exampleJsonSource =
  [s|
    {"boardTitle":"About music"
    ,"date":"2018-08-05"
    ,"items":[{ "tag": "Quote"
              , "quoteTitle":null
              , "content":"Without music, life would be a mistake."
              , "quoteSource":"Friedrich Nietzsche"}]}
  |]

setupDB :: IO ()
setupDB = void . withConnection connectionString $
  manipulate (UnsafeManipulation "SET client_min_messages = error;")
  & pqThen $ migrateUp $ single setup

dropDB :: IO ()
dropDB = void . withConnection connectionString $
  manipulate (UnsafeManipulation "SET client_min_messages = error;")
  & pqThen $ migrateDown $ single setup

boardDate :: Board -> Day
boardDate Board{..} = date

main :: IO ()
main = hspec $ before_ setupDB $ after_ dropDB $ do
  describe "When it comes to boards" $ do
    it "There is no data loss when persisting them and retrieving them" $ do
      pickedBoard <- withConnection connectionString $ do
        void $ newBoard fixtureBoard
        getBoardByDay (boardDate fixtureBoard)
      pickedBoard `shouldCorrespondTo` fixtureBoard

    it "One can retrieve the latest one" $ do
      latest <- withConnection connectionString $ do
        void $ newBoard fixtureBoard >> newBoard fixtureBoard2
        getLatestBoard
      latest `shouldCorrespondTo` fixtureBoard2

    it "One cannot insert two boards with the same date" $
      withConnection connectionString (newBoard fixtureBoard >> newBoard fixtureBoard)
        `shouldThrow` (== AlreadyExists)

    it "One can delete a board and it deletes every related items" $ do
      let boardDay = boardDate fixtureBoard
      (fetchedBoard, entityCount) <- withConnection connectionString $ do
          newBoard fixtureBoard >> removeBoardAtDay DeleteNonExistingEntity boardDay
          b <- getBoardByDay boardDay
          biN <- queryNumberOfBoardItems
          bQu <- queryNumberOfQuotes
          bIm <- queryNumberOfImages
          bIt <- queryNumberOfItems
          pure (b, sum [biN, bQu, bIm, bIt])
      fetchedBoard `shouldBe` Nothing
      entityCount `shouldBe` 0

    it "One can update a board" $ do
      updatedBoard <- withConnection connectionString $ do
        let boardDay = boardDate fixtureBoard
        void $ newBoard fixtureBoard
        newBoardFromdDB <- getBoardByDay boardDay
        let updatableBoard = fromMaybe (error "No board at this date") newBoardFromdDB
            withUpdate :: Board
            withUpdate = updatableBoard { boardTitle = Just "Updated title" }
        _ <- updateBoard boardDay withUpdate
        getBoardByDay boardDay
      let getBoardTitle :: Board -> Maybe Text
          getBoardTitle Board{..} = boardTitle
      (updatedBoard >>= getBoardTitle) `shouldBe` Just "Updated title"

    it "Boards can contain images too" $ do
     queriedBoard <- withConnection connectionString $ do
       void $ newBoard fixtureBoard3
       getLatestBoard
     queriedBoard `shouldCorrespondTo` fixtureBoard3

  describe "Creation items" $
    it "Can be read from JSON" $
      let decoded = eitherDecode exampleJsonSource :: Either String Board
      in decoded `shouldSatisfy` isRight

