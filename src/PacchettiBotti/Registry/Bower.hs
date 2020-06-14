{-# LANGUAGE OverloadedLists #-}
module PacchettiBotti.Registry.Bower where

import PacchettiBotti.Prelude

import qualified Data.Aeson as Json
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Conduit.Tar as Tar
import qualified Data.Conduit.Zlib as Compression (ungzip)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Language.JavaScript.Parser as JavaScript
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http (getGlobalManager)
import qualified Network.HTTP.Conduit as Http
import qualified RIO.Time as Time
import qualified Spago.Dhall as Dhall
import qualified System.FilePath.Glob as Glob
import qualified Text.Megaparsec as Parse
import qualified Turtle
import qualified UnliftIO.Directory as Directory
import qualified Web.Bower.PackageMeta as Bower

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (FromJSON(..), (.!=), (.:!))
import           Data.Conduit (runConduit, (.|))
import           Language.JavaScript.Dependencies (jsAstDependencies)
import           System.IO.Error (isDoesNotExistError)
import           Web.Bower.PackageMeta (PackageMeta (..))

import qualified PacchettiBotti.DB as DB
import qualified PacchettiBotti.GitHub as GitHub
import qualified PacchettiBotti.Run as Run
import qualified PacchettiBotti.Static as Static


type Expr = Dhall.DhallExpr Dhall.Import

type NativeDependencies = Map Text Text

newtype NpmDependencies = NpmDependencies { npmDependencies :: NativeDependencies }
newtype NpmDevDependencies = NpmDevDependencies { npmDevDependencies :: NativeDependencies }

data NpmPackageJson =
  NpmPackageJson
    { npmPackageJsonDependencies :: NativeDependencies
    , npmPackageJsonDevDependencies :: NativeDependencies
    }

instance FromJSON NpmPackageJson where
  parseJSON = Json.withObject "npm package.json" $ \package ->
    NpmPackageJson
      <$> package .:! "dependencies" .!= mempty
      <*> package .:! "devDependencies" .!= mempty

registryRepo :: GitHub.Address
registryRepo = GitHub.Address "purescript" "registry"

syncFromBower :: HasEnv env => RIO env ()
syncFromBower = do
  today <- liftIO $ Text.pack . Time.showGregorian . Time.utctDay <$> Time.getCurrentTime
  let commit = "Update manifests from Bower"
  let branchName = "pacchettibotti-updates-" <> today
  Run.runAndPushBranch branchName registryRepo commit writeMissingBowerManifests
    [ "git add packages"
    ]


writeMissingBowerManifests :: HasEnv env => String -> RIO env ()
writeMissingBowerManifests path = do
  let indexPath = "package.dhall"
  -- first of all we get a listing of all the bower packages
  -- let packages = Map.keys bowerPackages
  let packages :: [PackageName] = [ PackageName "aff" , PackageName "prelude" ]
  -- Then for every one of them..
  for_ packages $ \packageName@(PackageName package) -> do
    let packageDir = Text.pack path <> "/packages/" <> package
    -- first we check if we have the directory for it. If not, we make one
    whenM (not <$> testdir (pathFromText packageDir)) $ do
      logInfo $ "Directory did not exist for package " <> display package
      mktree $ pathFromText packageDir
    -- then for every package directory we list all the files, which are the versions
    -- Note that we exclude the version index from the listing!
    versions
      <- fmap (filter (== indexPath))
      $ Directory.listDirectory $ Text.unpack packageDir
    -- then we query the DB for all releases for that package
    releases <- DB.transact $ DB.getReleasesForPackage packageName
    -- and write the release index
    logInfo "Writing releases index.."
    let packageIndexPath = packageDir <> "/" <> Text.pack indexPath
    writeTextFile packageIndexPath (mkReleaseIndex releases)
    Dhall.format packageIndexPath

    -- are there any releases that we don't have the file for?
    let notInFiles DB.Release{..} = not $ Set.member releaseTag $ Set.fromList $ Tag . Text.pack <$> versions
    let missingFiles = List.filter notInFiles releases
    -- if yes, we:
    --  - download the bower file
    --  - convert it to our Package type
    --  - dump it on a file with the release name
    unless (List.null missingFiles) $ do
      logInfo $ "Found " <> display (List.length missingFiles) <> " releases that were on Bower but not registered"
      for_ missingFiles $ \DB.Release{..} -> do
        -- TODO push these on the bus at some point? We'll have to upload packages sooner or later
        let (DB.Address owner repo) = releaseAddress
        let (Tag tag) = releaseTag
        unless (Set.member (releaseAddress, releaseTag) toSkip) $ do
          let releaseTarballUrl = "https://github.com/"
                <> GitHub.untagName owner <> "/"
                <> GitHub.untagName repo <> "/"
                <> "archive/"
                <> tag <> ".tar.gz"
          let packageInfo = displayShow releaseAddress <> "@" <> display tag
          let versionPath = packageDir <> "/" <> tag <> ".dhall"
          result <- try . withSystemTempDirectory (Text.unpack $ GitHub.untagName repo <> "-" <> tag) $ \tmp -> do
            -- TODO: try to fetch the spago.dhall first and see if it conforms
            -- to the registry schema. Not right now, there's no package doing it
            logInfo $ "Fetching release tarball for " <> packageInfo
            req <- Http.parseRequest $ Text.unpack releaseTarballUrl
            res <- Http.responseBody <$> (Http.http req =<< liftIO Http.getGlobalManager)
            runConduit $ res .| Compression.ungzip .| Tar.untar (Tar.restoreFileInto tmp) .| Conduit.mapM_ liftIO
            packageMeta <- either error pure <=< liftIO $ Json.eitherDecodeFileStrict' "bower.json"
            logDebug "Checking self-contained Bower dependencies"
            unlessM (selfContainedDependencies packageMeta) $
              error "Bower dependencies not self-contained on PureScript packages!"
            logInfo $ "Parsing foreign modules for JavaScript dependencies"
            npmPackageJson <- either error pure <=< liftIO $ Json.eitherDecodeFileStrict "package.json" `catch` \e ->
              if isDoesNotExistError e then pure (Right $ NpmPackageJson mempty mempty) else throwIO e
            (npmDependencies, npmDevDependencies) <- nativeDependencies tmp npmPackageJson
            logInfo $ "Writing package definition for " <> packageInfo
            writeTextFile versionPath (toDhallSource packageMeta npmDependencies npmDevDependencies tag)
            Dhall.format versionPath

          case result of
            Left (err :: SomeException) -> do
              isFile <- testfile versionPath
              when isFile $
                Turtle.rm $ pathFromText versionPath
              logError $ "Failed to import " <> packageInfo
              logError $ displayShow err
            Right _ -> logInfo $ "Done with " <> packageInfo


-- | Are all the dependencies PureScript packages?
selfContainedDependencies :: HasDB env => PackageMeta -> RIO env Bool
selfContainedDependencies PackageMeta{..} = do
  packages <- Set.fromList . fmap DB.packageName <$> DB.transact DB.getAllPackages
  pure
    $ and
    $ (\d -> Set.member d packages)
    . (PackageName . stripPurescriptPrefix . Bower.runPackageName . fst)
    <$> (bowerDevDependencies <> bowerDependencies)


-- | Releases that should be skipped because they are somehow broken for now
toSkip :: Set (GitHub.Address, Tag)
toSkip = Set.fromList []


bowerPackages :: Map PackageName DB.Address
bowerPackages
  = snd $ Map.mapEither DB.parseAddress
  $ Map.mapKeys (\(PackageName p) -> PackageName $ stripPurescriptPrefix p) bowerPackagesMap
  where
    bowerPackagesMap :: Map PackageName Text
    bowerPackagesMap = fromRight mempty $ Json.eitherDecodeStrict Static.bowerPackagesJson


nativeDependencies :: MonadIO m => String -> NpmPackageJson -> m (NpmDependencies, NpmDevDependencies)
nativeDependencies root NpmPackageJson{..} = do
  usedDependencies <- globUsedDependencies "src/**/*.js"
  usedDevDependencies <- globUsedDependencies "test/**/*.js"
  let (unsavedNpmDependencies, npmDependencies) = partitionUsedDependencies npmPackageJsonDependencies usedDependencies
      (unsavedNpmDevDependencies, npmDevDependencies) = partitionUsedDependencies npmPackageJsonDevDependencies usedDevDependencies
  unless (null unsavedNpmDependencies && null (Set.difference unsavedNpmDevDependencies usedDependencies)) $ do
    error "Npm dependencies not saved in package.json!"
  pure $ (NpmDependencies{..}, NpmDevDependencies{..})
  where
    globUsedDependencies pattern = liftIO $ do
      sources <- Glob.globDir1 (Glob.compile pattern) root
      dependencies <- mconcat <$> traverse (fmap jsAstDependencies . JavaScript.parseFileUtf8) sources
      pure $ Set.difference dependencies builtinNodeJsModules
    partitionUsedDependencies dependencies = foldMap $ \k ->
      case Map.lookup k dependencies of
        Nothing -> (Set.singleton k, mempty)
        Just v -> (mempty, Map.singleton k v)
    builtinNodeJsModules =
      [ "assert"
      , "async_hooks"
      , "buffer"
      , "child_process"
      , "cluster"
      , "console"
      , "constants"
      , "crypto"
      , "dgram"
      , "dns"
      , "domain"
      , "events"
      , "fs"
      , "http"
      , "http2"
      , "https"
      , "inspector"
      , "module"
      , "net"
      , "os"
      , "path"
      , "perf_hooks"
      , "process"
      , "punycode"
      , "querystring"
      , "readline"
      , "repl"
      , "stream"
      , "string_decoder"
      , "sys"
      , "timers"
      , "tls"
      , "trace_events"
      , "tty"
      , "url"
      , "util"
      , "v8"
      , "vm"
      , "wasi"
      , "worker_threads"
      , "zlib"
      ]


toDhallSource :: PackageMeta -> NpmDependencies -> NpmDevDependencies -> Text -> Text
toDhallSource PackageMeta{..} NpmDependencies{..} NpmDevDependencies{..} version = Text.unlines
  [ "let Registry = ../../v1/Registry.dhall"
  , "in  Registry.Package::{"
  , ", name = " <> (tshow . stripPurescriptPrefix . Bower.runPackageName) bowerName
  , case bowerLicense of
      [license] -> ", license = Registry.License.`" <> license <> "`"
      _ -> ""
  , case bowerRepository of
      Nothing -> ""
      Just Bower.Repository{..}
        -> ", repository = Some (Registry.Repo." <> case parseRepo repositoryUrl of
          Nothing -> "Git { url = " <> tshow repositoryUrl <> ", version = " <> version <> " })"
          Just (owner, repo)
            -> "GitHub { owner = " <> tshow owner <> ", repo = " <> tshow repo <> ", version = " <> tshow version <> " })"
  , ", targets = toMap {"
    , ", src = Registry.Target::{"
      , "  sources = [ \"src/**/*.purs\" ] "
      , ", dependencies = " <> mkDependenciesMap (mkBowerDep <$> bowerDependencies)
      , ", nativeDependencies = " <> mkDependenciesMap (mkDep <$> Map.toList npmDependencies)
    , "}"
  , if List.null bowerDevDependencies && Map.null npmDevDependencies then "" else Text.unlines
    [ ", test = Registry.Target::{"
      , "  sources = [ \"src/**/*.purs\", \"test/**/*.purs\" ] "
      , ", dependencies = " <> mkDependenciesMap (mkBowerDep <$> bowerDependencies <> bowerDevDependencies)
      , ", nativeDependencies = " <> mkDependenciesMap (mkDep <$> Map.toList (npmDependencies <> npmDevDependencies))
      , "}"
    , "}"
    ]
  ]
  where
    mkBowerDep (packageName, versionRange) =
      mkDep
        ( stripPurescriptPrefix $ Bower.runPackageName packageName
        , Bower.runVersionRange versionRange
        )
    mkDep (packageName, versionRange)
      = "`" <> packageName
      <> "` = "
      <> tshow versionRange
    mkDependenciesMap dependencies =
      if List.null dependencies
      then "[] : Registry.Dependencies"
      else "toMap { " <> Text.intercalate ", " dependencies <> " }"


parseRepo :: Text -> Maybe (Text, Text)
parseRepo = Parse.parseMaybe parseRepoString
  where
    parseRepoString :: Parse.Parsec Void Text (Text, Text)
    parseRepoString = do
      void $ Parse.chunk "git://github.com/" <|> "git@github.com:"
      owner <- Parse.takeWhile1P (Just "owner") (/= '/')
      void $ Parse.chunk "/"
      repo <- Parse.takeRest
      pure
        ( owner
        , if Text.isSuffixOf ".git" repo
          then Text.dropEnd 4 repo
          else repo
        )


stripPurescriptPrefix :: Text -> Text
stripPurescriptPrefix name = fromMaybe name $ Text.stripPrefix "purescript-" name


mkReleaseIndex :: [DB.Release] -> Text
mkReleaseIndex releases = "{ " <> foldMap mkReleaseLine releases <> " }"
  where
    mkReleaseLine DB.Release{ releaseTag = Tag tag } = "\n, `" <> tag <> "` = ./" <> tag <> ".dhall"
