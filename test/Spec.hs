{-# LANGUAGE OverloadedStrings #-}
module Spec where

import           Control.Monad               (void)
import           Data.ByteString.Char8       (ByteString)
import           Data.Text                   (Text)
import           Data.Time
import           Data.UUID.V4                (nextRandom)
import           Melange.DB.Insertions
import           Melange.DB.Schema
import           Melange.DB.Selections
import           Melange.Model.Internal
import           Squeal.PostgreSQL           hiding (date)
import           Squeal.PostgreSQL.Migration
import           Test.Hspec

buildQuote :: Maybe Text -> Text -> Maybe Text -> IO Quote
buildQuote t c s = do
  pk <- nextRandom
  pure $ Quote pk t c s

fixtureQuote1 :: IO Quote
fixtureQuote1 =
  buildQuote
    Nothing
    "Happy families are all alike; every unhappy family is unhappy in its own way"
    Nothing

fixtureQuote2 :: IO Quote
fixtureQuote2 =
  buildQuote
    (Just "Oscar being Oscar")
    "To lose one parent may be regarded as a misfortune; to lose both looks like carelessness."
    Nothing

fixtureQuote3 :: IO Quote
fixtureQuote3 =
  buildQuote
    Nothing
    "The beginnings of all things are small."
    (Just "Cicero")

fixtureQuote4 :: IO Quote
fixtureQuote4 =
  buildQuote
    Nothing
    "The only joy in life is to begin"
    (Just "Cesar Pavese")

quoteToItem :: Quote -> IO Item
quoteToItem q = do
  uuid' <- nextRandom
  pure $ ItemQuote uuid' q

fixtureBoard :: IO Board
fixtureBoard = do
  uuid' <- nextRandom
  items' <- sequence [ fixtureQuote1 >>= quoteToItem
                     , fixtureQuote2 >>= quoteToItem ]
  pure $ Board uuid' (Just "On family") (fromGregorian 1998 12 12) items'

fixtureBoard2 :: IO Board
fixtureBoard2 = do
  uuid' <- nextRandom
  items' <- sequence [ fixtureQuote3 >>= quoteToItem
                     , fixtureQuote4 >>= quoteToItem ]
  pure $ Board uuid' (Just "On beginnings") (fromGregorian 2018 8 1) items'

connectionString :: ByteString
connectionString = "host=localhost port=5432 dbname=melangetest user=melange password=melange"

setupDB :: IO ()
setupDB = void . withConnection connectionString $ migrateUp $ single setup

dropDB :: IO ()
dropDB = void . withConnection connectionString $ migrateDown $ single setup

main :: IO ()
main = hspec $ before_ setupDB $ after_ dropDB $
  describe "When it comes to boards" $ do
    it "There is no data loss when persisting them and retrieving them" $ do
      board <- fixtureBoard2
      pickedBoard <- withConnection connectionString $ do
        newBoard board
        getBoardByDay (date board)
      pickedBoard `shouldBe` pure board

    it "One can retrieve the latest one" $ do
      board1 <- fixtureBoard
      board2 <- fixtureBoard2
      latest <- withConnection connectionString $ do
        newBoard board1
        newBoard board2
        getLatestBoard
      latest `shouldBe` pure board2
