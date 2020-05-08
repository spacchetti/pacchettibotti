module PacchettiBotti.Threads.Registry where

import           PacchettiBotti.Prelude

import qualified Data.Map.Strict               as Map
import qualified PacchettiBotti.GitHub         as GitHub
import qualified Control.Retry                 as Retry
import qualified Spago.Async                   as Async
import qualified PacchettiBotti.DB             as DB
import qualified PacchettiBotti.Registry.Bower as Bower


refreshBowerPackages :: HasEnv env => RIO env ()
refreshBowerPackages = do
  logInfo $ "Fetching release info for " <> display (length Bower.bowerPackages) <> " packages"

  -- Call GitHub for all these packages, get releases for them, write them to DB
  void $ Async.withTaskGroup 5 $ \taskGroup -> do
    asyncs <- for (Map.toList Bower.bowerPackages) (Async.async taskGroup . fetchRepoMetadata)
    for asyncs Async.wait

  logInfo "Fetched all releases for all packages, saved them to DB"
  writeBus NewBowerRefresh

  where
    fetchRepoMetadata
      :: (HasEnv env)
      => (PackageName, DB.Address) -> RIO env ()
    fetchRepoMetadata (packageName, address) =
      Retry.recoverAll (Retry.fullJitterBackoff 500000 <> Retry.limitRetries 20) $ \Retry.RetryStatus{..} -> do
        let dAddress = displayShow address
        logDebug $ "Retry " <> display rsIterNumber <> ": fetching releases for " <> dAddress
        DB.transact $ do
          let package = DB.Package packageName address Nothing
          void $ DB.insertPackage package

        !eitherTags <- GitHub.fetchAndSaveTags address

        case eitherTags of
          Left _ -> die [ "Retry " <> display rsIterNumber <> ": failed to fetch releases for " <> dAddress ]
          Right _tags -> logInfo $ "Yay, got tags for " <> dAddress
