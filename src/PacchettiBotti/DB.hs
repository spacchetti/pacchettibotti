module PacchettiBotti.DB
  ( module PacchettiBotti.DB.Schema
  , mkDB
  , transact
  , Handle
  , Query
  , HasDB(..)
  ) where

import Spago.Prelude hiding (Handle)

import qualified Database.Persist.Sqlite         as Persist

import PacchettiBotti.DB.Schema


class HasDB env where
  dbL :: Lens' env Handle

newtype Handle = Handle { handlePool :: Persist.ConnectionPool }

instance HasDB Handle where
  dbL = id


newSqlitePool :: HasLogFunc env => RIO env Persist.ConnectionPool
newSqlitePool = Persist.createSqlitePool "db.sqlite" 10

mkDB :: HasLogFunc env => RIO env Handle
mkDB = do
  connectionPool <- newSqlitePool
  let dbHandle = Handle connectionPool
  runRIO dbHandle $ transact $ Persist.runMigration migrateAll
  pure dbHandle

-- | Our type synonym for environment that allows accessing 'SqlBackend'.
type Query env = ReaderT Persist.SqlBackend (RIO env)

-- | Run a query in a transaction.
transact :: HasDB env => Query env a -> RIO env a
transact m = do
  Handle{..} <- view dbL
  Persist.runSqlPool m handlePool
