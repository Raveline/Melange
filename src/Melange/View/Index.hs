{-# LANGUAGE OverloadedStrings #-}
module Melange.View.Index
  (
  ) where

import Control.Monad (forM_)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

index = docTypeHtml $ do
  H.head $
    H.title "Stuff"
  body $
    p "Stuff"
