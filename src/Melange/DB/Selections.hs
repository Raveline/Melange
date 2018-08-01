{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Melange.DB.Selections
  (

  ) where

import           Melange.DB.Schema
import           Melange.Model     (Board (..), Image (..), Item (..),
                                    Quote (..), itemId)
import           Squeal.PostgreSQL

type BoardQueryResult =
  [
    "board_id" ::: 'NotNull 'PGuuid
  , "title"    ::: 'Null 'PGtext
  , "date"     ::: 'NotNull 'PGdate
  , "item_id" ::: 'NotNull 'PGuuid
  , "quote_id" ::: 'Null 'PGuuid
  , "quote_title" ::: 'Null 'PGtext
  , "content"   ::: 'Null 'PGtext
  , "quote_source" ::: 'Null 'PGtext
  , "image_id" ::: 'Null 'PGuuid
  , "filepath"   :::  'Null 'PGtext
  , "image_source" ::: 'Null 'PGtext
  ]

type BoardQuery params = Query Schema params BoardQueryResult

getBoardById :: BoardQuery '[ 'NotNull 'PGuuid ]
getBoardById = undefined
