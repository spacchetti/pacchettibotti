module PacchettiBotti.Threads.Generic where

import           PacchettiBotti.Prelude

import qualified PacchettiBotti.GitHub         as GitHub
import qualified PacchettiBotti.DB             as DB
import qualified PacchettiBotti.Threads.Spago  as Spago
import qualified PacchettiBotti.Threads.PackageSets
                                               as PackageSets


-- | Call GitHub to check for new releases of a repository
--   When there's a new one and we don't have it in our state we send a message on the bus
checkLatestRelease :: HasEnv env => GitHub.Address -> RIO env ()
checkLatestRelease address = GitHub.getTags address >>= \case
  Left err -> logWarn $ "Could not check the latest release for " <> displayShow address <> ". Error: " <> displayShow err
  Right tags@(latest@DB.Release{..}:_) -> do
    DB.transact (DB.getLatestRelease address) >>= \case
      -- We don't do anything if we have a release saved and it's the current one
      Just latestCachedTag | releaseTag == latestCachedTag -> pure ()
      _ -> do
        logInfo $ "Found latest release for " <> displayShow address <> ": " <> displayShow latest
        DB.transact $ DB.insertReleases tags
        case address of
          a | a == PackageSets.packageSetsRepo -> writeBus NewPackageSetsRelease
          a | a == Spago.purescriptRepo        -> writeBus NewPureScriptRelease
          a | a == Spago.docsSearchRepo        -> writeBus NewDocsSearchRelease
          a -> logError $ "Got another release, don't know what to do. Repo: " <> displayShow a
  Right [] -> logError $ "Did not find any releases for " <> displayShow address