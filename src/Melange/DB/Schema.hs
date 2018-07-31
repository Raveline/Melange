{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Melange.DB.Schema
  (
    Schema
  , BoardCols
  , QuoteCols
  , ImageCols
  , ItemCols
  , BoardItemCols
  , setup
  , teardown
  , initial
  ) where

import           Control.Monad               (void)
import           Control.Monad.Base          (liftBase)
import           Data.Int                    (Int32)
import           Data.Text                   (Text)
import           Data.UUID
import qualified Generics.SOP                as SOP
import qualified GHC.Generics                as GHC
import           Squeal.PostgreSQL
import           Squeal.PostgreSQL.Migration
import           Squeal.PostgreSQL.Render

type BoardCols =
      '[ "board_id" ::: 'NoDef :=> 'NotNull 'PGuuid
       , "title"    ::: 'NoDef :=> 'Null 'PGtext
       , "date"     ::: 'NoDef :=> 'NotNull 'PGdate
       ]

type QuoteCols =
      '[ "quote_id" ::: 'NoDef :=> 'NotNull 'PGuuid
       , "quote_title" ::: 'NoDef :=> 'Null 'PGtext
       , "content"   ::: 'NoDef :=> 'NotNull 'PGtext
       , "quote_source" ::: 'NoDef :=> 'Null 'PGtext
       ]

type ImageCols =
      '[ "image_id" ::: 'NoDef :=> 'NotNull 'PGuuid
       , "filepath"   :::   'NoDef :=> 'NotNull 'PGtext
       , "image_source" ::: 'NoDef :=> 'Null 'PGtext
       ]

type ItemCols =
      '[ "item_id" ::: 'NoDef :=> 'NotNull 'PGuuid
       , "quote_id" ::: 'NoDef :=> 'Null 'PGuuid
       , "image_id" ::: 'NoDef :=> 'Null 'PGuuid
       ]

type BoardItemCols =
      '[ "board_id" ::: 'NoDef :=> 'NotNull 'PGuuid
       , "item_id" ::: 'NoDef  :=> 'NotNull 'PGuuid
       , "order" ::: 'NoDef :=> 'NotNull 'PGint2
       ]

type Schema =
  '[ "quotes" ::: 'Table (
      '[ "pk_quote" ::: 'PrimaryKey '["quote_id"] ] :=> QuoteCols)
   , "images" ::: 'Table (
      '[ "pk_image" ::: 'PrimaryKey '["image_id"] ] :=> ImageCols)
   , "items" ::: 'Table (
       '[ "pk_item" ::: 'PrimaryKey '["item_id"]
        , "fk_quote_id" ::: 'ForeignKey '["quote_id"] "quotes" '["quote_id"]
        , "fk_image_id" ::: 'ForeignKey '["image_id"] "images" '["image_id"]
        ] :=> ItemCols)
   , "boards" ::: 'Table (
      '[ "pk_board" ::: 'PrimaryKey '["board_id"] ] :=> BoardCols)
   , "board_items" ::: 'Table (
       '[ "pk_board_item" ::: 'PrimaryKey '["board_id", "item_id"]
        , "fk_board_id" ::: 'ForeignKey '["board_id"] "boards" '["board_id"]
        , "fk_quote_id" ::: 'ForeignKey '["item_id"] "items" '["item_id"]
        ] :=> BoardItemCols
       )
   ]

initial :: Definition '[] Schema
initial =
    createTable #quotes
      (    (uuid & notNullable) `As` #quote_id
        :* (text & nullable) `As` #quote_title
        :* (text & notNullable) `As` #content
        :* (text & nullable) `As` #quote_source
        :* Nil
      )
      ( primaryKey #quote_id `As` #pk_quote :* Nil )
    >>> createTable #images
      (    (uuid & notNullable) `As` #image_id
        :* (text & notNullable) `As` #filepath
        :* (text & nullable) `As` #image_source
        :* Nil
      )
      ( primaryKey #image_id `As` #pk_image :* Nil )
    >>> createTable #items
      (    (uuid & notNullable) `As` #item_id
        :* (uuid & nullable) `As` #quote_id
        :* (uuid & nullable) `As` #image_id
        :* Nil
      )
      ( primaryKey #item_id `As` #pk_item
        :* foreignKey #quote_id #quotes #quote_id
             OnDeleteCascade OnUpdateCascade `As` #fk_quote_id
        :* foreignKey #image_id #images #image_id
             OnDeleteCascade OnUpdateCascade `As` #fk_image_id
        :* Nil )
    >>> createTable #boards
      (    (uuid & notNullable) `As` #board_id
        :* (text & nullable) `As` #title
        :* (date & notNullable) `As` #date
        :* Nil
      )
      ( primaryKey #board_id `As` #pk_board :* Nil )
    >>> createTable #board_items
      (    (uuid & notNullable) `As` #board_id
        :* (uuid & notNullable) `As` #item_id
        :* (int2 & notNullable) `As` #order
        :* Nil
      )
      ( primaryKey (#board_id :* #item_id :* Nil) `As` #pk_board_item
        :* foreignKey #board_id #boards #board_id
             OnDeleteCascade OnUpdateCascade `As` #fk_board_id
        :* foreignKey #item_id #items #item_id
             OnDeleteCascade OnUpdateCascade `As` #fk_quote_id
        :* Nil )

setup :: Migration IO '[] Schema
setup =
  Migration { name = "initial"
            , up = void $ define initial
            , down = void $ define teardown }

teardown :: Definition Schema '[]
teardown =
  dropTable #board_items
  >>> dropTable #items
  >>> dropTable #boards
  >>> dropTable #quotes
  >>> dropTable #images
