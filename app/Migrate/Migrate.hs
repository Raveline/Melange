module Migrate where

import Melange.DB.Initialize

main :: IO ()
main = putStrLn "Migrating schema..."
  >> initDB
  >> putStrLn "Finished"
