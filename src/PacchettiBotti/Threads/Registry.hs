module PacchettiBotti.Threads.Registry where

import           PacchettiBotti.Prelude

import qualified Data.Aeson                    as Json
import qualified Data.Map.Strict               as Map
import qualified PacchettiBotti.GitHub         as GitHub
import qualified PacchettiBotti.Static         as Static
import qualified Control.Retry                 as Retry
import qualified PacchettiBotti.DB             as DB


bowerPackages :: Map PackageName DB.Address
bowerPackages = snd $ Map.mapEither DB.parseAddress bowerPackagesMap
  where
    bowerPackagesMap :: Map PackageName Text
    bowerPackagesMap = fromRight mempty $ Json.eitherDecodeStrict Static.bowerPackagesJson

refreshBowerPackages :: HasEnv env => RIO env ()
refreshBowerPackages = do
  logInfo $ "Fetching release info for " <> display (length bowerPackages) <> " packages"

  -- Call GitHub for all these packages, get releases for them, write them to DB
  void $ withTaskGroup' 5 $ \taskGroup -> do
    asyncs <- for (Map.toList bowerPackages) (async' taskGroup . fetchRepoMetadata)
    for asyncs wait'

  logInfo "Fetched all releases for all packages, saved them to DB"
  writeBus NewBowerRefresh

  where
    fetchRepoMetadata
      :: (HasGitHub env, HasDB env)
      => (PackageName, DB.Address) -> RIO env ()
    fetchRepoMetadata (packageName, address) =
      Retry.recoverAll (Retry.fullJitterBackoff 500000 <> Retry.limitRetries 20) $ \Retry.RetryStatus{..} -> do
        logDebug $ "Retry " <> display rsIterNumber <> ": fetching releases for " <> displayShow address

        !eitherTags <- GitHub.getTags address

        case eitherTags of
          Left _ -> die [ "Retry " <> display rsIterNumber <> ": failed to fetch releases" ]
          Right tags -> do
            logInfo $ "YAY tags " <> displayShow address
            DB.transact $ do
              let package = DB.Package packageName address Nothing
              void $ DB.insertPackage package
              DB.insertReleases tags
