module PacchettiBotti.Prelude
  ( module PacchettiBotti.Env
  , module Spago.Prelude
  , module PacchettiBotti.Prelude
  , module Spago.Types
  , Tag(..)
  , CommitHash(..)
  , RepoMetadataV1(..)
  , ReposMetadataV1
  , encodePretty
  , the
  , HasType
  ) where

import PacchettiBotti.Env
import Spago.Prelude hiding (logInfo, logWarn, logError, logDebug)
import Spago.GlobalCache (Tag(..), CommitHash(..), RepoMetadataV1(..), ReposMetadataV1)
import Spago.Types
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Generics.Product (the, HasType(..))

import qualified Control.Concurrent as Concurrent
import qualified RIO

_1m, _5m, _60m, _24h :: Int
_1m = 60 * 1000000
_5m = 5 * _1m
_60m = 60 * _1m
_24h = 24 * _60m

sleep :: MonadIO m => Int -> m ()
sleep = liftIO . Concurrent.threadDelay

addLogContext :: HasType LogContext env => LogContext -> RIO env a -> RIO env a
addLogContext ctx = mapRIO (over (the @LogContext) (<> LogContext "/" <> ctx))

setLogContext :: HasType LogContext env => LogContext -> RIO env a -> RIO env a
setLogContext ctx = mapRIO (set (the @LogContext) ctx)

liftLog :: (MonadIO m, MonadReader env m, HasLog env) => RIO LogFunc a -> m a
liftLog action = do
  logFunc <- view (the @LogFunc)
  runRIO logFunc action

logWithContext
  :: (HasLog env)
  => (Utf8Builder -> RIO LogFunc ())
  -> Utf8Builder
  -> RIO env ()
logWithContext logImpl msg = do
  (LogContext logContext) <- view (the @LogContext)
  logFunc <- view (the @LogFunc)
  runRIO logFunc (logImpl $ logContext  <> " - " <> msg)

logDebug, logInfo, logWarn, logError
  :: (HasLog env)
  => Utf8Builder
  -> RIO env ()
logDebug = logWithContext RIO.logDebug
logInfo = logWithContext RIO.logInfo
logWarn = logWithContext RIO.logWarn
logError = logWithContext RIO.logError