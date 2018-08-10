{-# LANGUAGE OverloadedStrings #-}
module Melange.Image
  (
    findExtension
  ) where

import           Codec.Picture
import           Data.ByteString
import qualified Data.ByteString.Lazy as LBS

findExtension :: ByteString -> Either LBS.ByteString String
findExtension bs = case decodeImage bs of
  Right (ImageRGB8 _)   -> Right "png"
  Right (ImageYCbCr8 _) -> Right "jpg"
  _                     -> Left "Format not handled"
