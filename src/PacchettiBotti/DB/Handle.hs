module PacchettiBotti.DB.Handle where

import Prelude (id)
import Data.Generics.Product (HasType(..))

import qualified Database.Persist.Sqlite as Persist

newtype Handle = Handle { handlePool :: Persist.ConnectionPool }

instance {-# OVERLAPPING #-} HasType Handle Handle
  where
    typed = id
