{-# LANGUAGE OverloadedStrings #-}
module Melange.View.Index
  (
    IndexPage (..)
  ) where

import           Control.Monad               (forM_)
import           Melange.Model.External
import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A

newtype IndexPage = IndexPage (Maybe Board)

instance ToMarkup IndexPage where
  toMarkup (IndexPage Nothing) = do
    H.head $
      H.title "Melange"
    body $
      p "There is nothing to display"
  toMarkup (IndexPage (Just b))= do
    H.head $
      H.title "Melange"
    body $
      p "There should be a pretty board here"
