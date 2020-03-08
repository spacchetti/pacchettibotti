module PacchettiBotti.Threads.PackageSetsMetadata where

import           PacchettiBotti.Prelude

import qualified Control.Retry                 as Retry
import qualified Data.Map.Strict               as Map
import qualified Spago.Dhall                   as Dhall
import qualified Dhall.Map
import qualified Spago.Config
import qualified Data.ByteString.Lazy          as BSL
import qualified GHC.IO

import qualified PacchettiBotti.GitHub         as GitHub
import qualified PacchettiBotti.Run            as Run
import qualified PacchettiBotti.DB             as DB


metadataRepo :: GitHub.Address
metadataRepo = GitHub.Address "spacchetti" "package-sets-metadata"

-- | Take the latest package set from package-sets master, get a list of all the
--   packages in there, and thenn query their commits and tags. Once done, send
--   the package on the bus.
fetcher :: HasEnv env => RIO env ()
fetcher = do
  logInfo "Downloading and parsing package set.."
  packageSet <- fetchPackageSet
  DB.transact $ DB.replacePackageSet packageSet
  writeBus NewPackageSet
  let packages = Map.toList packageSet
  logInfo $ "Fetching metadata for " <> display (length packages) <> " packages"

  -- Call GitHub for all these packages, get metadata for them, save to DB
  void $ withTaskGroup' 5 $ \taskGroup -> do
    asyncs <- for packages (async' taskGroup . fetchRepoMetadata)
    for asyncs wait'

  logInfo "Fetched all metadata, saved to DB."
  writeBus NewMetadata

  where
    fetchRepoMetadata :: HasEnv env => (PackageName, Package) -> RIO env ()
    fetchRepoMetadata (_, pkg@Package{ location = Local{..}, ..}) = die [ "Tried to fetch a local package: " <> displayShow pkg ]
    fetchRepoMetadata (packageName, Package{ location = Remote{ repo = Repo repoUrl, ..}, ..}) =
      Retry.recoverAll (Retry.fullJitterBackoff 50000 <> Retry.limitRetries 10) $ \Retry.RetryStatus{..} -> do
        let address@DB.Address{..} = case DB.parseAddress repoUrl of
              Right a -> a
              Left err -> error $ show err
        let dAddress = displayShow address
        logDebug $ "Retry " <> display rsIterNumber <> ": fetching tags and commits for " <> dAddress
        DB.transact $ do
          let package = DB.Package packageName address Nothing
          void $ DB.insertPackage package

        !eitherTags <- GitHub.getTags address
        !eitherCommits <- GitHub.getCommits address

        case (eitherTags, eitherCommits) of
          (Left _, _) -> die [ "Retry " <> display rsIterNumber <> ": failed to fetch tags for " <> dAddress ]
          (_, Left _) -> die [ "Retry " <> display rsIterNumber <> ": failed to fetch commits for " <> dAddress ]
          (Right _tags, Right _commits) -> logInfo $ "Got tags and commits for " <> dAddress


    -- | Tries to read in a PackageSet from GitHub, master branch
    --   (so we always get the most up to date and we don't have to wait for a release)
    fetchPackageSet
      :: (MonadIO m, MonadThrow m, MonadReader env m, HasLogFunc env)
      => m (Map PackageName Package)
    fetchPackageSet = do
      expr <- liftIO $ Dhall.inputExpr "https://raw.githubusercontent.com/purescript/package-sets/master/src/packages.dhall"
      case expr of
        Dhall.RecordLit pkgs -> Map.mapKeys PackageName . Dhall.Map.toMap
          <$> traverse Spago.Config.parsePackage pkgs
        something -> throwM $ Dhall.PackagesIsNotRecord something


-- | Whenever there's a new metadata set, push it to the repo
updater :: (HasDB env, HasLogFunc env) => RIO env ()
updater = do
  -- Get metadata
  metadata <- DB.transact DB.getPackageSetMetadata
  -- Write the metadata to file
  let writeMetadata :: HasLogFunc env => GHC.IO.FilePath -> RIO env ()
      writeMetadata tempfolder = do
        path <- makeAbsolute (tempfolder </> "metadataV1new.json")
        logInfo $ "Writing metadata to file: " <> displayShow path
        liftIO $ BSL.writeFile path $ encodePretty metadata
        logInfo "Done."

  let commitMessage = "Update GitHub index file"
  Run.runAndPushMaster metadataRepo commitMessage
    writeMetadata
    [ "mv -f metadataV1new.json metadataV1.json"
    , "git add metadataV1.json"
    ]
