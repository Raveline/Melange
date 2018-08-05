{-# LANGUAGE OverloadedStrings #-}
module Melange.View.Index
  (
    IndexPage (..)
  ) where

import           Melange.Model.External
import           Text.Blaze.Html5            as H

newtype IndexPage = IndexPage (Maybe Board)

instance ToMarkup IndexPage where
  toMarkup (IndexPage Nothing) = do
    H.head $
      H.title "Melange"
    body $
      p "There is nothing to display"
  toMarkup (IndexPage (Just _))= do
    H.head $
      H.title "Melange"
    body $
      p "There should be a pretty board here"
