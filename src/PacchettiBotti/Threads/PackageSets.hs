module PacchettiBotti.Threads.PackageSets where

import           PacchettiBotti.Prelude

import qualified Data.Text                     as Text
import qualified Control.Concurrent            as Concurrent
import qualified Data.Map.Merge.Strict         as Map
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import qualified Data.Time                     as Time
import qualified GHC.IO
import qualified Text.Megaparsec               as Parse
import qualified Spago.Dhall                   as Dhall
import qualified Dhall.Map

import qualified PacchettiBotti.GitHub         as GitHub
import qualified PacchettiBotti.Run            as Run
import qualified PacchettiBotti.DB             as DB


type Expr = Dhall.DhallExpr Dhall.Import

packageSetsRepo :: GitHub.Address
packageSetsRepo = GitHub.Address "purescript" "package-sets"

packageSetsPackageName :: PackageName
packageSetsPackageName = PackageName "__internal__.package-sets"


-- | Watch out for the result of a `spago verify-set` command, and comment appropriately
--   on the PR thread (if any)
commenter :: HasEnv env => Text -> VerificationResult -> RIO env ()
commenter commandRun result = do
  maybePR <- GitHub.getPullRequestForUser "spacchettibotti" packageSetsRepo

  case maybePR of
    Nothing -> do
      logWarn "Could not find an open PR, waiting 5 mins.."
      liftIO $ Concurrent.threadDelay (5 * 60 * 1000000)
      writeBus (NewVerification commandRun result)
    Just GitHub.PullRequest{..} -> do
      let commentBody = case result of
            (ExitSuccess, _, _) -> "Result of `" <> commandRun <> "` in a clean project: **success** 🎉"
            (_, _out, err) -> Text.unlines
              [ "Result of `" <> commandRun <> "` in a clean project: **failure** 😱"
              , ""
              , "<details><summary>Error output</summary><p>"
              , ""
              , "```"
              , err
              , "```"
              , ""
              , "</p></details>"
              ]
      GitHub.commentOnPR packageSetsRepo pullRequestNumber commentBody


data BotCommand
  = Ban PackageName
  | Unban PackageName
  deriving (Eq, Ord)


-- | This fairly sophisticated thing will try to upgrade packages in the set
--   as soon as there's a new release available for them.
--   It will do so by getting the releases, patching the Dhall files in there,
--   then opening a PR with the result of `spago verify-set`.
--   Once in there, it will parse the comments on the PR to find out which
--   packages we want to temporarily ban from the verification/upgrade process
updater :: HasEnv env => RIO env ()
updater = do
  -- Let's get the package set and its metadata from DB
  (packageSet, newMetadata) <- DB.transact $ do
    p <- DB.getPackageSet
    m <- DB.getPackageSetMetadata
    pure (p, m)
  -- This metadata is the most up-to-date snapshot of all the repos in package-sets
  -- master branch. Among other things it contains the latest releases of all the
  -- packages in there.
  -- So here we take all these releases, and diff this list with the current
  -- package set from the state - i.e. the one versioned in package-sets master.
  -- This gives us a list of packages that need to be updated.
  -- In doing this we consider packages that might have been "banned"
  -- (i.e. that we don't want to update)
  let intersectionMaybe f = Map.merge Map.dropMissing Map.dropMissing (Map.zipWithMaybeMatched f)
  let computePackagesToUpdate metadata banned
        = intersectionMaybe pickPackage metadata packageSet
        where
          pickPackage :: PackageName -> RepoMetadataV1 -> DB.Package -> Maybe (Tag, Text)
          -- | We throw away packages without a release
          pickPackage _ RepoMetadataV1{ latest = Nothing, ..} _ = Nothing
          -- | And the ones that are not in the set
          pickPackage _ _ DB.Package{ packageSetVersion = Nothing, ..} = Nothing
          -- | For the remaining ones: we pick the latest from metadata if it's
          --   different from the one we have in the set. Except if that version
          --   is banned, in that case we pick it from the package set
          pickPackage _ RepoMetadataV1{ latest = Just latest, owner } DB.Package{ packageSetVersion = Just version, ..}
            = case (latest == version, Set.member (packageName, latest) banned) of
                (True, _) -> Nothing
                (_, True) -> Just (version, owner)
                (_, _)    -> Just (latest, owner)

  banned <- DB.transact DB.getBannedReleases
  let removeBannedOverrides packageName (tag, _) = case Map.lookup packageName packageSet of
        Just DB.Package{ packageSetVersion = Just version } | tag == version -> False
        _ -> True
  let newVersionsWithBanned = computePackagesToUpdate newMetadata banned
  let newVersions = Map.filterWithKey removeBannedOverrides newVersionsWithBanned

  let patchVersions :: (HasLog env, HasBus env) => GHC.IO.FilePath -> RIO env ()
      patchVersions path = do
        let packages = Map.toList newVersionsWithBanned
        for_ packages $ \(packageName, (tag, owner)) -> do
          logInfo $ "Patching version for " <> displayShow packageName
          Run.withAST (Text.pack $ path </> "src" </> "groups" </> Text.unpack (Text.toLower owner) <> ".dhall")
            $ updateVersion packageName tag

        -- If we don't have anything to commit, then we actually skip verification
        (exitCode, out, _err) <- Run.runWithCwd path "git diff --exit-code"
        case exitCode of
          ExitSuccess -> logInfo "Tried refreshing package-sets' Dhall, but got nothing new to commit"
          _ -> do
            logInfo $ "Output: " <> display out
            logInfo "Verifying new set. This might take a LONG while.."
            let verifyCmd (PackageName package, _) = "spago -C verify " <> package
            let cmd = "cd src && " <> Text.intercalate " && " (map verifyCmd packages)
            result <- Run.runWithCwd path cmd
            logInfo "Verified packages, spamming the channel with the result.."
            writeBus $ NewVerification cmd result

  let commands =
        [ "make"
        , "git add packages.json"
        , "git add src/groups"
        ]

  logInfo $ "Found " <> display (length newVersions) <> " packages to update"

  unless (null newVersions) $ do
    logInfo $ displayShow newVersions
    -- If we have at least one package to update, let's see if we already have an
    -- open PR to package-sets. If we do we can just commit there
    maybePR <- GitHub.getPullRequestForUser "spacchettibotti" packageSetsRepo

    case maybePR of
      Nothing -> do
        today <- liftIO $ Text.pack . Time.showGregorian . Time.utctDay <$> Time.getCurrentTime
        let prBranchName = "spacchettibotti-updates-" <> today
            prTitle = "Updates " <> today
            prAddress = packageSetsRepo
            prBody = mkBody newVersions banned
        logInfo $ "No PR found, opening " <> displayShow prTitle
        Run.runAndOpenPR GitHub.SimplePR{..} patchVersions commands
      Just GitHub.PullRequest{ pullRequestHead = GitHub.PullRequestCommit{..}, ..} -> do
        logInfo $ "Found a PR open, reusing " <> displayShow pullRequestTitle
        -- A PR is there and there might be updates to the banned packages, so we
        -- try to update the banlist
        commentsForPR <- GitHub.getCommentsOnPR packageSetsRepo pullRequestNumber
        let newBanned = computeNewBanned commentsForPR banned packageSet
        let newVersionsWithBanned' = computePackagesToUpdate newMetadata newBanned
        let newVersions' = Map.filterWithKey removeBannedOverrides newVersionsWithBanned'

        let patchVersions' path = do
              patchVersions path
              GitHub.updatePullRequestBody packageSetsRepo pullRequestNumber $ mkBody newVersions' newBanned

        Run.runAndPushBranch pullRequestCommitRef packageSetsRepo pullRequestTitle patchVersions' commands
  where
    computeNewBanned comments banned packageSet
      = Set.fromList $ Map.toList
      $ foldl applyCommand (Map.fromList $ Set.toList banned)
      $ mapMaybe parseComment comments
      where
        applyCommand :: Map PackageName Tag -> BotCommand -> Map PackageName Tag
        applyCommand bannedMap (Ban package) | Just DB.Package{ packageSetVersion = Just version } <- Map.lookup package packageSet
          = Map.insert package version bannedMap
        applyCommand bannedMap (Unban package) = Map.delete package bannedMap
        applyCommand bannedMap _ = bannedMap

        parseComment GitHub.IssueComment{..} = Parse.parseMaybe parseCommand issueCommentBody

        parseCommand :: Parse.Parsec Void Text BotCommand
        parseCommand = do
          void $ Parse.chunk "@spacchettibotti "
          command <- (Parse.chunk "ban " >> pure Ban) <|> (Parse.chunk "unban " >> pure Unban)
          package <- PackageName <$> Parse.takeRest
          pure $ command package

    mkBody packages banned =
      let renderUpdate (PackageName packageName, (Tag tag, owner))
            = "- [`" <> packageName <> "` upgraded to `" <> tag <> "`](https://github.com/"
              <> owner <> "/purescript-" <> packageName <> "/releases/tag/" <> tag <> ")"
          renderBanned (PackageName packageName, Tag tag)
            = "- `" <> packageName <> "`@`" <> tag <> "`"
      in Text.unlines
         $ [ "Updated packages:" ]
         <> fmap renderUpdate (Map.toList packages)
         <> (if Set.null banned
             then []
             else [ "", "Banned packages:" ] <> fmap renderBanned (Set.toList banned))
         <> [ ""
            , "You can give commands to the bot by adding a comment where you tag it, e.g.:"
            , "- `@spacchettibotti ban react-basic`"
            , "- `@spacchettibotti unban simple-json`"
            ]

    updateVersion :: Monad m => PackageName -> Tag -> Expr -> m Expr
    updateVersion (PackageName packageName) (Tag tag) (Dhall.RecordLit kvs)
      | Just (Dhall.RecordLit pkgKVs) <- Dhall.Map.lookup packageName kvs
      , Just (Dhall.TextLit _) <- Dhall.Map.lookup "version" pkgKVs =
          let
            newPackageVersion = Dhall.toTextLit tag
            newPackage = Dhall.RecordLit $ Dhall.Map.insert "version" newPackageVersion pkgKVs
          in pure $ Dhall.RecordLit $ Dhall.Map.insert packageName newPackage kvs
    updateVersion _ _ other = pure other
