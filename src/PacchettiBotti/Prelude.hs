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
  ) where

import PacchettiBotti.Env
import Spago.Prelude
import Spago.GlobalCache (Tag(..), CommitHash(..), RepoMetadataV1(..), ReposMetadataV1)
import Spago.Types
import           Data.Aeson.Encode.Pretty       ( encodePretty )

import qualified Control.Concurrent            as Concurrent


_1m, _5m, _60m, _24h :: Int
_1m = 60 * 1000000
_5m = 5 * _1m
_60m = 60 * _1m
_24h = 24 * _60m

sleep :: MonadIO m => Int -> m ()
sleep = liftIO . Concurrent.threadDelay
