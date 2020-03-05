module PacchettiBotti.DB
  ( module PacchettiBotti.DB.Schema
  , mkDB
  , transact
  , Handle
  , Query
  , HasDB(..)
  , getLatestRelease
  , getBannedReleases
  , getPackageSet
  , getPackageSetMetadata
  , insertPackage
  , insertReleases
  , insertCommits
  , replacePackageSet
  , Release(..)
  ) where

import           Spago.Prelude           hiding ( Handle )

import qualified Database.Persist.Sqlite       as Persist
import qualified Data.Map.Strict               as Map
import qualified Data.List                     as List
import qualified Data.Ord
import qualified Spago.Types                   as Spago
import qualified GitHub
import qualified Data.SemVer                   as SemVer
import qualified Data.Set as Set

import           Database.Persist.Sqlite        ( (=.)
                                                , (==.)
                                                )
import           PacchettiBotti.DB.Schema
import           System.FileLock                ( withFileLock
                                                , SharedExclusive(..)
                                                )
import           Spago.GlobalCache              ( ReposMetadataV1
                                                , Tag(..)
                                                , RepoMetadataV1(..)
                                                )


class HasDB env where
  dbL :: Lens' env Handle

newtype Handle = Handle { handlePool :: Persist.ConnectionPool }

instance HasDB Handle where
  dbL = id

-- | Our type synonym for environment that allows accessing 'SqlBackend'.
type Query a = forall env . ReaderT Persist.SqlBackend (RIO env) a


newSqlitePool :: HasLogFunc env => RIO env Persist.ConnectionPool
newSqlitePool = Persist.createSqlitePool "db.sqlite" 5

mkDB :: HasLogFunc env => RIO env Handle
mkDB = do
  connectionPool <- newSqlitePool
  let dbHandle = Handle connectionPool
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
  Handle{..} <- view dbL
  liftIO $ withFileLock "db.sqlite" Exclusive $ const $
    runRIO handlePool $ Persist.runSqlPool m handlePool


-- -- Queries

insertPackage :: Package -> Query (Maybe (Persist.Entity Package))
insertPackage = Persist.insertUniqueEntity


insertReleases :: [Release] -> Query ()
insertReleases = traverse_ Persist.insertUnique


insertCommits :: [Commit] -> Query ()
insertCommits = traverse_ Persist.insertUnique


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


getReleases :: Address -> Query [Release]
getReleases address = fmap Persist.entityVal 
  <$> Persist.selectList [ ReleaseAddress ==. address ] [ ]

getLatestRelease :: Address -> Query (Maybe Tag)
getLatestRelease address = do
  releases <- getReleases address
  -- TODO: ideally we want to store these in the DB as SemVer already
  let parseTag (Tag tag) = SemVer.parseSemVer tag
  let renderSV = ("v" <>) . SemVer.renderSV
  let semverTags = (parseTag . releaseTag) <$> releases
  pure $ fmap (Tag . renderSV) $ headMay $ List.sortOn Data.Ord.Down $ rights semverTags


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
        pure $ ((,releaseTag) . packageName) <$> maybePackage
      _ -> pure Nothing
  pure $ Set.fromList $ catMaybes bannedPackages


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
      commits <- fmap (commitCommit . Persist.entityVal)
        <$> Persist.selectList [ CommitAddress ==. packageAddress ] [ ]
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
