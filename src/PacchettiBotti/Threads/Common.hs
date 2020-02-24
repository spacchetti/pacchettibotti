module PacchettiBotti.Threads.Common where

import           Spago.Prelude

import qualified Control.Concurrent            as Concurrent
import qualified Control.Concurrent.STM.TChan  as Chan
import qualified Data.Map.Strict               as Map

import qualified PacchettiBotti.GitHub         as GitHub

import           PacchettiBotti.Threads


-- | Call GitHub to check for new releases of a repository
--   When there's a new one and we don't have it in our state we send a message on the bus
checkLatestRelease :: HasLogFunc env => GitHub.Auth -> GitHub.Address -> Message -> RIO env ()
checkLatestRelease token address RefreshState = GitHub.getLatestRelease token address >>= \case
  Left err -> logWarn $ "Could not check the latest release for " <> displayShow address <> ". Error: " <> displayShow err
  Right GitHub.Release {..} -> do
    State{..} <- liftIO $ Concurrent.readMVar state
    case Map.lookup address latestReleases of
      -- We don't do anything if we have a release saved and it's the current one
      Just currentRelease | currentRelease == releaseTagName -> pure ()
      _ -> do
        logInfo $ "Found a new release for " <> displayShow address <> ": " <> display releaseTagName
        atomically $ Chan.writeTChan bus $ NewRepoRelease address releaseTagName
checkLatestRelease _ _ _ = pure ()

-- | Everything that goes on the bus is persisted in the State,
--   so threads can access some decently-up-to-date info
--   (with no guarantee of atomicity though, this is only for convenience)
persistState :: Message -> RIO env ()
persistState = \case
  RefreshState -> pure ()
  NewVerification _ -> pure () -- TODO: maybe save this?
  NewRepoRelease address release -> liftIO $ Concurrent.modifyMVar_ state
    $ \State{..} -> let newReleases = Map.insert address release latestReleases
                    in pure State{ latestReleases = newReleases , ..}
  NewMetadata newMetadata -> liftIO $ Concurrent.modifyMVar_ state
    $ \State{..} -> pure State{ metadata = newMetadata, ..}
  NewPackageSet newPackageSet -> liftIO $ Concurrent.modifyMVar_ state
    $ \State{..} -> pure State{ packageSet = newPackageSet, ..}
