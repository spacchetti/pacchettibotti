{-# LANGUAGE OverloadedLists #-}
module PacchettiBotti.Registry.Bower where

import PacchettiBotti.Prelude


import qualified Data.Aeson                    as Json
import qualified PacchettiBotti.Static         as Static
import           Web.Bower.PackageMeta      (PackageMeta (..))
import qualified Web.Bower.PackageMeta      as Bower
import qualified Spago.Dhall as Dhall
import qualified Data.Text as Text
import qualified Text.Megaparsec               as Parse
import qualified Data.List as List
import qualified UnliftIO.Directory as Directory
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Network.HTTP.Simple as Http
import qualified Turtle

import qualified PacchettiBotti.DB as DB
import qualified PacchettiBotti.GitHub as GitHub


type Expr = Dhall.DhallExpr Dhall.Import


importFromBower :: IO ()
importFromBower = withEnv $ do
  -- first of all we get a listing of all the bower packages
  let packages = Map.keys bowerPackages
  for_ packages $ \packageName@(PackageName package) -> do
    -- TODO: clone the repo instead
    let packageDir = "../registry/packages/" <> package
    -- first we check if we have the directory. If not, we make one
    whenM (not <$> testdir (pathFromText packageDir)) $ do
      logInfo $ "Directory did not exist for package " <> display package
      mktree $ pathFromText packageDir
    -- then for every package there, we list all the files, which are the versions
    -- TODO: exclude the index from the listing
    versions <- Directory.listDirectory $ Text.unpack packageDir
    -- then we query the DB for all releases for that package
    releases <- DB.transact $ DB.getReleasesForPackage packageName
    -- TODO: write releases index
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
          let url = "https://raw.githubusercontent.com/"
                <> GitHub.untagName owner <> "/"
                <> GitHub.untagName repo <> "/"
                <> tag <> "/bower.json"
          -- TODO: download the package.json too?
          -- See https://github.com/purescript/registry/issues/20
          let packageInfo = displayShow releaseAddress <> "@" <> display tag
          let versionPath = packageDir <> "/" <> tag <> ".dhall"
          result <- try $ do
            logInfo $ "Fetching Bower info for " <> packageInfo
            req <- Http.parseRequest $ Text.unpack url
            packageMeta <- Http.getResponseBody <$> Http.httpJSON req
            logDebug "Checking self-contained dependencies"
            unlessM (selfContainedDependencies packageMeta) $ do
              error "Dependencies not self-contained on purescript packages!"
            logInfo $ "Writing package definition for " <> packageInfo
            writeTextFile versionPath (toDhallSource packageMeta tag)
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
  packages <- Set.fromList <$> fmap DB.packageName <$> DB.transact DB.getAllPackages
  pure
    $ and
    $ (\d -> Set.member d packages)
    <$> (PackageName . stripPurescriptPrefix . Bower.runPackageName . fst)
    <$> (bowerDevDependencies <> bowerDependencies)


-- | Releases that should be skipped because they are somehow broken for now
toSkip :: Set (GitHub.Address, Tag)
toSkip = Set.fromList []


bowerPackages :: Map PackageName DB.Address
bowerPackages
  = (flip Map.restrictKeys) [PackageName "aff", PackageName "prelude"]
  $ snd $ Map.mapEither DB.parseAddress
  $ Map.mapKeys (\(PackageName p) -> PackageName $ stripPurescriptPrefix p) bowerPackagesMap
  where
    bowerPackagesMap :: Map PackageName Text
    bowerPackagesMap = fromRight mempty $ Json.eitherDecodeStrict Static.bowerPackagesJson


toDhallSource :: PackageMeta -> Text -> Text
toDhallSource PackageMeta{..} version = Text.unlines
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
  , ", targets = toMap { "
  , if List.null bowerDependencies
    then ", src = Registry.Target::{ sources = [ \"src/**/*.purs\" ], dependencies = [] : Registry.Dependencies }"
    else ", src = Registry.Target::{ sources = [ \"src/**/*.purs\" ], dependencies = toMap { " <> Text.intercalate ", " (mkDep <$> bowerDependencies) <> " }}"
  , if List.null bowerDevDependencies
    then ""
    else ", test = Registry.Target::{ sources = [ \"src/**/*.purs\", \"test/**/*.purs\" ], dependencies = toMap { " <> Text.intercalate ", " (mkDep <$> bowerDevDependencies) <> " }}"
  , " }"
  , " }"
  ]
  where
    mkDep (packageName, versionRange)
      = "`" <> stripPurescriptPrefix (Bower.runPackageName packageName)
      <> "` = "
      <> tshow (Bower.runVersionRange versionRange)


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