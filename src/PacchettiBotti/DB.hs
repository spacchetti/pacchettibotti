module PacchettiBotti.DB
  ( module PacchettiBotti.DB.Schema
  , mkDB
  , transact
  , DB.Handle
  , Query
  , getReleases
  , getLatestRelease
  , getBannedReleases
  , getReleasesForPackage
  , getCommits
  , getPackageSet
  , getPackageSetMetadata
  , getAllPackages
  , insertPackage
  , insertReleases
  , insertCommits
  , insertFetchInfo
  , replacePackageSet
  , shouldFetchHappen
  , Release(..)
  ) where

import PacchettiBotti.Prelude hiding (Handle, Package, PackageName)

import qualified Database.Persist.Sqlite       as Persist
import qualified Data.Map.Strict               as Map
import qualified Data.List                     as List
import qualified Spago.Types                   as Spago
import qualified GitHub
import qualified Data.Set as Set

import           Database.Persist.Sqlite        ( (=.)
                                                , (==.)
                                                , (>=.)
                                                )
import PacchettiBotti.DB.Schema
import qualified PacchettiBotti.DB.Handle as DB
import Data.Time (UTCTime)
import qualified Data.Time as Time
import           System.FileLock                ( withFileLock
                                                , SharedExclusive(..)
                                                )
import           Spago.GlobalCache              ( ReposMetadataV1
                                                , Tag(..)
                                                , RepoMetadataV1(..)
                                                )


-- | Our type synonym for environment that allows accessing 'SqlBackend'.
type Query a = forall env . ReaderT Persist.SqlBackend (RIO env) a

mkDB :: RIO LogFunc DB.Handle
mkDB = do
  connectionPool <- Persist.createSqlitePool "db.sqlite" 5
  let dbHandle = DB.Handle connectionPool
  runRIO dbHandle $ transact $ Persist.runMigration migrateAll
  pure dbHandle


-- | Run a query in a transaction.
--
--  NOTE: Currently, we take a write lock on every action. This is
--  a bit heavyweight, but it avoids the SQLITE_BUSY errors, e.g.
--  as seen on <https://github.com/commercialhaskell/stack/issues/4471>
--  We can investigate more elegant solutions in the future,
--  such as separate read and write actions or introducing
--  smarter retry logic.
transact :: HasDB env => Query a -> RIO env a
transact m = do
  DB.Handle{..} <- view (the @DB.Handle)
  liftIO $ withFileLock "db.sqlite" Exclusive $ const $
    runRIO handlePool $ Persist.runSqlPool m handlePool


-- -- Queries

insertPackage :: Package -> Query (Maybe (Persist.Entity Package))
insertPackage = Persist.insertUniqueEntity


-- Here we reverse the list, so that older releases are pushed in first,
-- and they'll have a smaller id. This is so that we can sort Asc to get the latest
insertReleases :: [Release] -> Query ()
insertReleases = traverse_ Persist.insertUnique . List.reverse


insertCommits :: [Commit] -> Query ()
insertCommits = traverse_ Persist.insertUnique


insertFetchInfo :: Fetch -> Query ()
insertFetchInfo fetch@Fetch{..} = void $ Persist.upsert fetch [ FetchTime =. fetchTime ]


replacePackageSet :: Map Spago.PackageName Spago.Package -> Query ()
replacePackageSet newPackageSet = do
  -- First we reset all the package set versions that we have stored
  allPackages <- Persist.selectList [ ] [ ]
  for_ (List.filter (isJust . packageSetVersion . Persist.entityVal) allPackages)
    $ \Persist.Entity{..} -> Persist.update entityKey [ PackageSetVersion =. Nothing ]
  -- Then we go through all the packages in the set and update the version with the new one
  for_ packageVersions $ \package@Package{..} ->
    Persist.upsert package [ PackageSetVersion =. packageSetVersion ]
  where
    packageVersions :: [Package]
    packageVersions = catMaybes $ uncurry toPackage <$> Map.toList newPackageSet

    toPackage _ Spago.Package{ location = Spago.Local{} } = Nothing
    toPackage packageName Spago.Package{ location = Spago.Remote{ version, repo = Spago.Repo repo }}
      = case parseAddress repo of
          Left _ -> Nothing
          Right address -> Just $ Package packageName address (Just $ Tag version)


shouldFetchHappen :: FetchType -> UTCTime -> Query Bool
shouldFetchHappen fetchType now = do
  maybeLastFetch <- fmap Persist.entityVal
    <$> Persist.selectFirst
          [ FetchType ==. fetchType, FetchTime >=. threshold ]
          [ Persist.Desc FetchTime, Persist.LimitTo 1 ]
  pure $ isNothing maybeLastFetch
  where
    threshold = Time.addUTCTime (- (Time.nominalDay / 48)) now


getPackage :: Spago.PackageName -> Query (Maybe Package)
getPackage packageName = fmap Persist.entityVal
  <$> Persist.selectFirst [ PackageName ==. packageName ] [ ]


getAllPackages :: Query [Package]
getAllPackages = fmap Persist.entityVal <$> Persist.selectList [ ] [ ]


getReleases :: Address -> Query [Release]
getReleases address = fmap Persist.entityVal
  <$> Persist.selectList [ ReleaseAddress ==. address ] [ Persist.Desc ReleaseId ]


getReleasesForPackage :: Spago.PackageName -> Query [Release]
getReleasesForPackage packageName = do
  maybePackage <- getPackage packageName
  case maybePackage of
    Nothing -> pure []
    Just Package{ packageAddress } -> fmap Persist.entityVal
      <$> Persist.selectList [ ReleaseAddress ==. packageAddress ] [ Persist.Desc ReleaseId ]


getLatestRelease :: Address -> Query (Maybe Tag)
getLatestRelease address =
  fmap (releaseTag . Persist.entityVal)
    <$> Persist.selectFirst
          [ ReleaseAddress ==. address ]
          [ Persist.Desc ReleaseId, Persist.LimitTo 1 ]


getBannedReleases :: Query (Set (Spago.PackageName, Tag))
getBannedReleases = do
  bannedReleases <- fmap Persist.entityVal
    <$> Persist.selectList [ ReleaseBanned ==. True ] [ ]
  bannedPackages <- for bannedReleases $ \Release{..} -> do
    latestForAddress <- getLatestRelease releaseAddress
    case latestForAddress of
      -- We care about a banned tag only if it's the latest.
      -- Because if it's not, then there will be a new one that might fix the issue.
      Just latest | latest == releaseTag -> do
        maybePackage <- fmap Persist.entityVal <$> Persist.selectFirst [ PackageAddress ==. releaseAddress ] [ ]
        pure $ ((,releaseTag) . PacchettiBotti.DB.Schema.packageName) <$> maybePackage
      _ -> pure Nothing
  pure $ Set.fromList $ catMaybes bannedPackages


getCommits :: Address -> Query [Commit]
getCommits address = fmap Persist.entityVal
  <$> Persist.selectList [ CommitAddress ==. address ] [ ]


getPackageSetMetadata :: Query ReposMetadataV1
getPackageSetMetadata = do
  packageSetMap <- getPackageSet
  traverse mkRepoMetadata packageSetMap
  where
    toMetadataTag Release{..} = (releaseTag, releaseCommit)

    mkRepoMetadata :: Package -> Query RepoMetadataV1
    mkRepoMetadata Package{..} = do
      let (Address owner' _repo) = packageAddress
      let owner = GitHub.untagName owner'
      commits <- fmap commitCommit <$> getCommits packageAddress
      releases <- getReleases packageAddress
      latest <- getLatestRelease packageAddress
      let tags = Map.fromList $ toMetadataTag <$> releases
      pure RepoMetadataV1{..}


getPackageSet :: Query (Map Spago.PackageName Package)
getPackageSet = do
  allPackages <- fmap Persist.entityVal <$> Persist.selectList [ ] [ ]
  pure
    $ Map.fromList
    $ mkPair
    <$> List.filter (isJust . packageSetVersion) allPackages
  where
    mkPair package@Package{..} = (packageName, package)
