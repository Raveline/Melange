{-# LANGUAGE OverloadedStrings #-}
module Spec where

import           Control.Monad               (void)
import           Data.ByteString.Char8       (ByteString)
import           Data.Time
import           Data.UUID.V4                (nextRandom)
import           Melange.DB.Schema
import           Melange.DB.Insertions
import           Melange.Model
import           Squeal.PostgreSQL
import           Squeal.PostgreSQL.Migration
import           Test.Hspec

fixtureQuote1 :: IO Quote
fixtureQuote1 = do
  uuid <- nextRandom
  pure $ Quote
          uuid
          Nothing
          "Happy families are all alike; every unhappy family is unhappy in its own way"
          Nothing

fixtureQuote2 :: IO Quote
fixtureQuote2 = do
  uuid <- nextRandom
  pure $ Quote
          uuid
          (Just "Oscar being Oscar")
          "To lose one parent may be regarded as a misfortune; to lose both looks like carelessness."
          Nothing

quoteToItem :: Quote -> IO Item
quoteToItem q = do
  uuid <- nextRandom
  pure $ ItemQuote uuid q

fixtureBoard :: IO Board
fixtureBoard = do
  uuid <- nextRandom
  items' <- sequence [ fixtureQuote1 >>= quoteToItem
                     , fixtureQuote2 >>= quoteToItem ]
  pure $ Board uuid (Just "A title") (fromGregorian 1998 12 12) items'

connectionString :: ByteString
connectionString = "host=localhost port=5432 dbname=melangetest user=melange password=melange"

main :: IO ()
main = hspec $
  describe "Make sure one can insert a board" $
    it "Simply insert a board" $ do
      board <- fixtureBoard
      void . withConnection connectionString $
        migrateUp (single setup)
      void . withConnection connectionString $
        newBoard board
      void . withConnection connectionString $
        migrateDown $ single setup

