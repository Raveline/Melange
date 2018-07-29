{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Melange.DB.Insertions
  (
    newItem
  ) where

import           Control.Monad            (void)
import qualified GHC.Generics             as GHC
import           Melange.DB.Schema
import           Melange.Model
import           Squeal.PostgreSQL
import           Squeal.PostgreSQL.Render

insertQuote :: Manipulation Schema '[ 'NotNull 'PGuuid, 'Null 'PGtext, 'NotNull 'PGtext, 'Null 'PGtext ] '[]
insertQuote = insertRow #quotes
    ( Set (param @1) `As` #quote_id
    :* Set (param @2) `As` #quote_title
    :* Set (param @3) `As` #content
    :* Set (param @4) `As` #quote_source
    :* Nil )
    OnConflictDoNothing (Returning Nil)

insertQuoteItem :: Manipulation Schema '[ 'NotNull 'PGuuid, 'Null 'PGuuid] '[]
insertQuoteItem = insertRow #items
    ( Set (param @1) `As` #item_id
    :* Set (param @2) `As` #quote_id
    :* Set null_ `As` #image_id
    :* Nil )
    OnConflictDoNothing (Returning Nil)

insertImage :: Manipulation Schema '[ 'NotNull 'PGuuid, 'NotNull 'PGtext, 'Null 'PGtext ] '[]
insertImage = insertRow #images
    ( Set (param @1) `As` #image_id
    :* Set (param @2) `As` #filepath
    :* Set (param @3) `As` #image_source
    :* Nil )
    OnConflictDoNothing (Returning Nil)

insertImageItem :: Manipulation Schema '[ 'NotNull 'PGuuid, 'Null 'PGuuid] '[]
insertImageItem = insertRow #items
    ( Set (param @1) `As` #item_id
    :* Set null_ `As` #quote_id
    :* Set (param @2) `As` #image_id
    :* Nil )
    OnConflictDoNothing (Returning Nil)

newItem :: Item -> PQ Schema Schema IO ()
newItem (ItemQuote uuid q) =
  void $ manipulateParams insertQuote q
        >> manipulateParams insertQuoteItem (uuid, Just $ quoteId q)
newItem (ItemImage uuid i) =
  void $ manipulateParams insertImage i
        >> manipulateParams insertImageItem (uuid, Just $ imageId i)
