module PacchettiBotti.Threads.Common where

import           PacchettiBotti.Prelude

import qualified Data.Map.Strict               as Map

import qualified PacchettiBotti.GitHub         as GitHub


-- | Call GitHub to check for new releases of a repository
--   When there's a new one and we don't have it in our state we send a message on the bus
checkLatestRelease :: HasEnv env => GitHub.Address -> RIO env ()
checkLatestRelease address = GitHub.getLatestRelease address >>= \case
  Left err -> logWarn $ "Could not check the latest release for " <> displayShow address <> ". Error: " <> displayShow err
  Right GitHub.Release {..} -> do
    State{..} <- readState
    case Map.lookup address latestReleases of
      -- We don't do anything if we have a release saved and it's the current one
      Just currentRelease | currentRelease == releaseTagName -> pure ()
      _ -> do
        logInfo $ "Found latest release for " <> displayShow address <> ": " <> display releaseTagName
        writeBus $ NewRepoRelease address releaseTagName
