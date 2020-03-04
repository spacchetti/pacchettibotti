module PacchettiBotti.Prelude
  ( module PacchettiBotti.Env
  , module Spago.Prelude
  , module PacchettiBotti.Prelude
  ) where

import PacchettiBotti.Env
import Spago.Prelude hiding (Env, HasEnv(..))

import qualified Control.Concurrent            as Concurrent


_1m, _5m, _60m, _24h :: Int
_1m = 60 * 1000000
_5m = 5 * _1m
_60m = 60 * _1m
_24h = 24 * _60m

sleep :: MonadIO m => Int -> m ()
sleep = liftIO . Concurrent.threadDelay
