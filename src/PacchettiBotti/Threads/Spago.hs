module PacchettiBotti.Threads.Spago where

import           PacchettiBotti.Prelude

import qualified Data.Text                     as Text

import qualified PacchettiBotti.GitHub         as GitHub
import qualified PacchettiBotti.Run            as Run
import qualified PacchettiBotti.DB             as DB
import qualified PacchettiBotti.Threads.PackageSets
                                               as PackageSets


docsSearchRepo :: GitHub.Address
docsSearchRepo = GitHub.Address "spacchetti" "purescript-docs-search"

spagoRepo :: GitHub.Address
spagoRepo = GitHub.Address "spacchetti" "spago"

purescriptRepo :: GitHub.Address
purescriptRepo = GitHub.Address "purescript" "purescript"

docsSearchPackageName :: PackageName
docsSearchPackageName = PackageName "__internal__.purescript-docs-search"

spagoPackageName :: PackageName
spagoPackageName = PackageName "__internal__.spago"

purescriptPackageName :: PackageName
purescriptPackageName = PackageName "__internal__.purescript"


-- | Whenever there's a new release of package-sets, update it in Spago's template
updatePackageSets :: HasEnv env => RIO env ()
updatePackageSets =
  DB.transact (DB.getLatestRelease PackageSets.packageSetsPackageName) >>= \case
    Nothing -> logError "Did not find a latest release for Spago?"
    Just (Tag newTag) -> do
      let prTitle = "Update to package-sets@" <> newTag
      let prBranchName = "spacchettibotti-" <> newTag
      Run.runAndOpenPR GitHub.SimplePR{ prBody = "", prAddress = spagoRepo, ..} (const $ pure ())
        [ "cd templates"
        , "spago upgrade-set"
        , "git add packages.dhall"
        ]


-- | Whenever there's a new release of purescript, update the release on our various CI files
updatePurescriptVersion :: HasEnv env => RIO env ()
updatePurescriptVersion =
  DB.transact (DB.getLatestRelease purescriptPackageName) >>= \case
    Nothing -> logError "Did not find a latest release for Purescript?"
    Just (Tag newTag) -> do
      let prTitle = "Update to purescript@" <> newTag
      let prBranchName = "spacchettibotti-purs-" <> newTag
      let bundleTag = Text.drop 1 newTag
      Run.runAndOpenPR GitHub.SimplePR{ prBody = "", prAddress = spagoRepo, ..} (const $ pure ())
        [ "sed -e 's/$tag =.*$/$tag = " <> surroundQuote newTag <> "/g' -i appveyor.yml"
        , "sed -e 's/    TAG=.*$/    TAG=" <> surroundQuote newTag <> "/g' -i .travis.yml"
        , "sed -e 's/Generated by purs bundle.*/Generated by purs bundle " <> bundleTag <> "/g' -i test/fixtures/bundle-app.js"
        , "sed -e 's/Generated by purs bundle.*/Generated by purs bundle " <> bundleTag <> "/g' -i test/fixtures/bundle-app-src-map.js"
        , "sed -e 's/Generated by purs bundle.*/Generated by purs bundle " <> bundleTag <> "/g' -i test/fixtures/bundle-module.js"
        , "sed -e 's/Generated by purs bundle.*/Generated by purs bundle " <> bundleTag <> "/g' -i test/fixtures/bundle-module-src-map.js"
        , "sed -e 's/Generated by purs version.*/Generated by purs version " <> bundleTag <> "/g' -i test/fixtures/bundle-app.js"
        , "sed -e 's/Generated by purs version.*/Generated by purs version " <> bundleTag <> "/g' -i test/fixtures/bundle-app-src-map.js"
        , "sed -e 's/Generated by purs version.*/Generated by purs version " <> bundleTag <> "/g' -i test/fixtures/bundle-module.js"
        , "sed -e 's/Generated by purs version.*/Generated by purs version " <> bundleTag <> "/g' -i test/fixtures/bundle-module-src-map.js"
        , "git add .travis.yml appveyor.yml test"
        ]


-- | Whenever there's a new release of the purescript-docs-search, update our version of it
updateDocsSearch :: HasEnv env => RIO env ()
updateDocsSearch =
  DB.transact (DB.getLatestRelease docsSearchPackageName) >>= \case
    Nothing -> logError "Did not find a latest release for purescript-docs-search?"
    Just (Tag newTag) -> do
      let prTitle = "Update to purescript-docs-search@" <> newTag
      let prBranchName = "spacchettibotti-docs-search-" <> newTag
      Run.runAndOpenPR GitHub.SimplePR{ prBody = "", prAddress = spagoRepo, ..} (const $ pure ())
        [ "sed -e 's/docsSearchVersion = .*/docsSearchVersion = " <> surroundQuote newTag <> "/g' -i src/Spago/Prelude.hs"
        , "git add src"
        ]
