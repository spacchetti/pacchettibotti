module PacchettiBotti.Threads.PackageSetsMetadata where

import           PacchettiBotti.Prelude

import qualified Control.Retry                 as Retry
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as Text
import qualified Spago.Dhall                   as Dhall
import qualified Dhall.Map
import qualified Spago.Config
import qualified Data.ByteString.Lazy          as BSL
import qualified GHC.IO

import           Spago.Types
import           Spago.GlobalCache              ( RepoMetadataV1(..), ReposMetadataV1 )
import           Data.Aeson.Encode.Pretty       ( encodePretty )

import qualified PacchettiBotti.GitHub         as GitHub
import qualified PacchettiBotti.Run            as Run


metadataRepo :: GitHub.Address
metadataRepo = GitHub.Address "spacchetti" "package-sets-metadata"

-- | Take the latest package set from package-sets master, get a list of all the
--   packages in there, and thenn query their commits and tags. Once done, send
--   the package on the bus.
fetcher :: HasEnv env => RIO env ()
fetcher = do
  logInfo "Downloading and parsing package set.."
  packageSet <- fetchPackageSet
  writeBus $ NewPackageSet packageSet
  let packages = Map.toList packageSet
  logInfo $ "Fetching metadata for " <> display (length packages) <> " packages"

  -- Call GitHub for all these packages and get metadata for them
  !metadata <- withTaskGroup' 10 $ \taskGroup -> do
    asyncs <- for packages (async' taskGroup . fetchRepoMetadata)
    for asyncs wait'

  logInfo "Fetched all metadata."
  writeBus $ NewMetadata $ foldMap (uncurry Map.singleton) metadata

  where
    fetchRepoMetadata :: HasGitHub env => (PackageName, Package) -> RIO env (PackageName, RepoMetadataV1)
    fetchRepoMetadata (_, pkg@Package{ location = Local{..}, ..}) = die [ "Tried to fetch a local package: " <> displayShow pkg ]
    fetchRepoMetadata (packageName, Package{ location = Remote{ repo = Repo repoUrl, ..}, ..}) =
      Retry.recoverAll (Retry.fullJitterBackoff 50000 <> Retry.limitRetries 25) $ \Retry.RetryStatus{..} -> do
        let !(owner:repo:_rest)
              = Text.split (=='/') $ Text.replace "https://github.com/" ""
                (if Text.isSuffixOf ".git" repoUrl
                 then Text.dropEnd 4 repoUrl
                 else repoUrl)
            address = GitHub.Address (GitHub.mkName Proxy owner) (GitHub.mkName Proxy repo)

        logDebug $ "Retry " <> display rsIterNumber <> ": fetching tags and commits for " <> displayShow address

        !eitherTags <- GitHub.getTags address
        !eitherCommits <- GitHub.getCommits address

        case (eitherTags, eitherCommits) of
          (Left _, _) -> die [ "Retry " <> display rsIterNumber <> ": failed to fetch tags" ]
          (_, Left _) -> die [ "Retry " <> display rsIterNumber <> ": failed to fetch commits" ]
          (Right (latest, tags), Right commits) ->
            pure (packageName, RepoMetadataV1{..})


    -- | Tries to read in a PackageSet from GitHub, master branch
    --   (so we always get the most up to date and we don't have to wait for a release)
    fetchPackageSet :: (MonadIO m, MonadThrow m, MonadReader env m, HasLogFunc env) => m PackageSetMap
    fetchPackageSet = do
      expr <- liftIO $ Dhall.inputExpr "https://raw.githubusercontent.com/purescript/package-sets/master/src/packages.dhall"
      case expr of
        Dhall.RecordLit pkgs -> Map.mapKeys PackageName . Dhall.Map.toMap
          <$> traverse Spago.Config.parsePackage pkgs
        something -> throwM $ Dhall.PackagesIsNotRecord something


-- | Whenever there's a new metadata set, push it to the repo
updater :: HasLogFunc env => ReposMetadataV1 -> RIO env ()
updater metadata = do
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
