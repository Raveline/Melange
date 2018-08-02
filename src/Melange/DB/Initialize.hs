{-# LANGUAGE OverloadedStrings #-}
module Melange.DB.Initialize
  (
    initDB
  , computeConnectionString
  ) where

import           Control.Monad               (void)
import           Control.Monad.Writer
import           Data.ByteString.Char8       (ByteString, pack)
import           Data.List                   (intercalate)
import           Data.Maybe                  (fromMaybe)
import           Melange.DB.Schema
import           Squeal.PostgreSQL
import           Squeal.PostgreSQL.Migration
import           System.Environment          (lookupEnv)

fetchEnv :: String -> IO String
fetchEnv env =
  let onError = error $ "Missing environment variable " <> env
  in fromMaybe onError <$> lookupEnv env

fetcher :: String -> WriterT [String] IO ()
fetcher env = do
  value <- liftIO $ fetchEnv env
  tell [env <> "=" <> value]

computeConnectionString :: IO ByteString
computeConnectionString =
  let computation =
        fetcher "host"
        >> fetcher "port"
        >> fetcher "dbname"
        >> fetcher "user"
        >> fetcher "password"
      formatter = pack . unwords
  in formatter <$> execWriterT computation

initDB :: IO ()
initDB = do
  connectionString <- computeConnectionString
  void $ withConnection connectionString $
    manipulate (UnsafeManipulation "SET client_min_messages TO WARNING;")
    & pqThen (migrateUp $ single setup)
  putStrLn "Finished"
