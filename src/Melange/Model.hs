{-# LANGUAGE RecordWildCards #-}
module Melange.Model
  (
    internalBoardToExternal
  , externalBoardToInternal
  ) where

import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Data.UUID.V4           (nextRandom)
import qualified Melange.Model.External as E
import qualified Melange.Model.Internal as I

internalBoardToExternal :: I.Board -> E.Board
internalBoardToExternal I.Board{..} =
  let items' = internalToExternal <$> items
  in E.Board boardId boardTitle date items'

externalBoardToInternal :: (MonadIO m) => E.Board -> m I.Board
externalBoardToInternal E.Board{..} =
  let items' = traverse externalToInternal items
  in liftIO $ I.Board <$> pure boardId
                      <*> pure boardTitle
                      <*> pure date
                      <*> items'

internalToExternal :: I.Item -> E.Item
internalToExternal (I.ItemQuote itemId I.Quote {..}) = E.Quote{..}
internalToExternal (I.ItemImage itemId I.Image {..}) = E.Image{..}

externalToInternal :: E.Item -> IO I.Item
externalToInternal E.Quote{..} = do
  quoteId <- nextRandom
  pure $ I.ItemQuote itemId I.Quote{..}
externalToInternal E.Image{..} = do
  imageId <- nextRandom
  pure $ I.ItemImage itemId I.Image{..}
