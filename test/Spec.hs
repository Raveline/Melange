{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
module Spec where

import           Control.Monad               (void)
import           Data.Aeson
import           Data.ByteString.Char8       (ByteString)
import qualified Data.ByteString.Lazy        as LBS
import           Data.Either                 (isRight)
import           Data.String.QQ
import           Data.Time
import           Melange.DB.Insertions
import           Melange.DB.Schema
import           Melange.DB.Selections
import           Melange.Model.External      hiding (Board (..), Image,
                                              Item (..), Quote)
import           Melange.Model.Internal      hiding (date)
import           Squeal.PostgreSQL           hiding (date)
import           Squeal.PostgreSQL.Migration
import           Test.Hspec

fixtureQuote1 :: ItemCreation
fixtureQuote1 =
  QuoteCreation
    Nothing
    "Happy families are all alike; every unhappy family is unhappy in its own way"
    Nothing

fixtureQuote2 :: ItemCreation
fixtureQuote2 =
  QuoteCreation
    (Just "Oscar being Oscar")
    "To lose one parent may be regarded as a misfortune; to lose both looks like carelessness."
    Nothing

fixtureQuote3 :: ItemCreation
fixtureQuote3 =
  QuoteCreation
    Nothing
    "The beginnings of all things are small."
    (Just "Cicero")

fixtureQuote4 :: ItemCreation
fixtureQuote4 =
  QuoteCreation
    Nothing
    "The only joy in life is to begin"
    (Just "Cesar Pavese")

fixtureBoard :: BoardCreation
fixtureBoard =
  BoardCreation
    (Just "On family")
    (fromGregorian 1998 12 12)
    [fixtureQuote1, fixtureQuote2]

fixtureBoard2 :: BoardCreation
fixtureBoard2 =
  BoardCreation
    (Just "On beginnings")
    (fromGregorian 2018 8 1)
    [fixtureQuote3, fixtureQuote4]

shouldCorrespondTo :: (HasCallStack) => Maybe Board -> BoardCreation -> Expectation
shouldCorrespondTo Nothing _ = expectationFailure "Query had no result"
shouldCorrespondTo (Just (Board _ t d its)) bc =
  let convertItem (ItemQuote _ Quote{..}) = QuoteCreation{..}
      convertItem (ItemImage _ Image{..}) = ImageCreation{..}
      convertItems = convertItem <$> its
      asBoardCreation = BoardCreation t d convertItems
  in asBoardCreation `shouldBe` bc

connectionString :: ByteString
connectionString = "host=localhost port=5432 dbname=melangetest user=melange password=melange"

exampleJsonSource :: LBS.ByteString
exampleJsonSource =
  [s|
    {"boardTitle":"About music"
    ,"date":"2018-08-05"
    ,"items":[{ "tag": "QuoteCreation"
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

main :: IO ()
main = hspec $ before_ setupDB $ after_ dropDB $ do
  describe "When it comes to boards" $ do
    it "There is no data loss when persisting them and retrieving them" $ do
      pickedBoard <- withConnection connectionString $ do
        void $ newBoard fixtureBoard
        getBoardByDay ((date :: BoardCreation -> Day) fixtureBoard)
      pickedBoard `shouldCorrespondTo` fixtureBoard

    it "One can retrieve the latest one" $ do
      latest <- withConnection connectionString $ do
        void $ newBoard fixtureBoard >> newBoard fixtureBoard2
        getLatestBoard
      latest `shouldCorrespondTo` fixtureBoard2

    it "One cannot insert two boards with the same date" $ do
      (withConnection connectionString $ newBoard fixtureBoard >> newBoard fixtureBoard)
        `shouldThrow` (== AlreadyExists)

  describe "Creation items" $
    it "Can be read from JSON" $
      let decoded = eitherDecode exampleJsonSource :: Either String BoardCreation
      in decoded `shouldSatisfy` isRight
