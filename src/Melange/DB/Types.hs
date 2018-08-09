module Melange.DB.Types
  (
    QueryException(..)
  ) where

import           Control.Exception.Safe

data QueryException =
    AlreadyExists
  | UpdateNonExistingEntity
  | DeleteNonExistingEntity
  deriving (Show, Typeable, Eq)

instance Exception QueryException
