{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeInType        #-}
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
  , migration1
  ) where

import           Control.Monad               (void)
import           Squeal.PostgreSQL
import           Squeal.PostgreSQL.Migration

type BoardTable =
  "boards" ::: 'Table (
      '[ "pk_board" ::: 'PrimaryKey '["board_id"]
       , "unique_dates" ::: 'Unique '["date"]
       ] :=> BoardCols)

type BoardCols =
      '[ "board_id" ::: 'NoDef :=> 'NotNull 'PGuuid
       , "title"    ::: 'NoDef :=> 'Null 'PGtext
       , "date"     ::: 'NoDef :=> 'NotNull 'PGdate
       ]

type QuoteTable =
  "quotes" ::: 'Table (
    '[ "pk_quote" ::: 'PrimaryKey '["quote_id"]
     ] :=> QuoteCols)

type QuoteTableMig1 =
  "quotes" ::: 'Table (
    '[ "pk_quote" ::: 'PrimaryKey '["quote_id"]
     ] :=> QuoteColsMig1)

type QuoteCols =
      '[ "quote_id" ::: 'NoDef :=> 'NotNull 'PGuuid
       , "quote_title" ::: 'NoDef :=> 'Null 'PGtext
       , "content"   ::: 'NoDef :=> 'NotNull 'PGtext
       , "quote_source" ::: 'NoDef :=> 'Null 'PGtext
       ]

type QuoteColsMig1 =
  '[ "quote_id" ::: 'NoDef :=> 'NotNull 'PGuuid
   , "quote_title" ::: 'NoDef :=> 'Null 'PGtext
   , "content"   ::: 'NoDef :=> 'NotNull 'PGtext
   , "quote_source" ::: 'NoDef :=> 'Null 'PGtext
   , "quote_style" ::: 'NoDef :=> 'Null 'PGtext ]

type ImageTable =
  "images" ::: 'Table (
      '[ "pk_image" ::: 'PrimaryKey '["image_id"]
       ] :=> ImageCols)

type ImageCols =
  '[ "image_id" ::: 'NoDef :=> 'NotNull 'PGuuid
   , "filepath"   :::   'NoDef :=> 'NotNull 'PGtext
   , "image_source" ::: 'NoDef :=> 'Null 'PGtext
   ]

type ImageColsMig1 =
  '[ "image_id" ::: 'NoDef :=> 'NotNull 'PGuuid
   , "filepath"   :::   'NoDef :=> 'NotNull 'PGtext
   , "image_source" ::: 'NoDef :=> 'Null 'PGtext
   , "image_style" ::: 'NoDef :=> 'Null 'PGtext ]

type ImageTableMig1 =
  "images" ::: 'Table (
    '[ "pk_image" ::: 'PrimaryKey '["image_id"]
     ] :=> ImageColsMig1)

type ItemsTable = "items" ::: 'Table (
      '[ "pk_item" ::: 'PrimaryKey '["item_id"]
       , "fk_quote_id" ::: 'ForeignKey '["quote_id"] "quotes" '["quote_id"]
       , "fk_image_id" ::: 'ForeignKey '["image_id"] "images" '["image_id"]
       ] :=> ItemCols)

type ItemCols =
      '[ "item_id" ::: 'NoDef :=> 'NotNull 'PGuuid
       , "quote_id" ::: 'NoDef :=> 'Null 'PGuuid
       , "image_id" ::: 'NoDef :=> 'Null 'PGuuid
       ]

type BoardItemTable = "board_items" ::: 'Table (
       '[ "pk_board_item" ::: 'PrimaryKey '["board_id", "item_id"]
        , "fk_board_id" ::: 'ForeignKey '["board_id"] "boards" '["board_id"]
        , "fk_item_id" ::: 'ForeignKey '["item_id"] "items" '["item_id"]
        ] :=> BoardItemCols
       )

type BoardItemCols =
      '[ "board_id" ::: 'NoDef :=> 'NotNull 'PGuuid
       , "item_id" ::: 'NoDef  :=> 'NotNull 'PGuuid
       , "order" ::: 'NoDef :=> 'NotNull 'PGint2
       ]

type BaseSchema =
    '[ QuoteTable
     , ImageTable
     , ItemsTable
     , BoardTable
     , BoardItemTable
     ]

type Migration1 =
    '[ QuoteTableMig1
     , ImageTableMig1
     , ItemsTable
     , BoardTable
     , BoardItemTable
     ]

setup :: Migration IO '[] BaseSchema
setup =
  Migration { name = "initial"
            , up = void $ define initial
            , down = void $ define teardown }

initial :: Definition '[] BaseSchema
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
      ( primaryKey #board_id `As` #pk_board
        :* unique #date `As` #unique_dates
        :* Nil
      )
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
             OnDeleteCascade OnUpdateCascade `As` #fk_item_id
        :* Nil )

teardown :: Definition BaseSchema '[]
teardown =
  dropTable #board_items
  >>> dropTable #items
  >>> dropTable #boards
  >>> dropTable #quotes
  >>> dropTable #images

migration1 :: Migration IO BaseSchema Migration1
migration1 =
  Migration { name = "Add styles"
            , up = void $ define up1
            , down = void $ define down1 }

up1 :: Definition BaseSchema Migration1
up1 = alterTable #quotes (addColumn #quote_style (text & nullable))
     >>> alterTable #images (addColumn #image_style (text & nullable))

down1 :: Definition Migration1 BaseSchema
down1 = alterTable #quotes (dropColumn #quote_style)
        >>> alterTable #images (dropColumn #image_style)

type Schema = Migration1
