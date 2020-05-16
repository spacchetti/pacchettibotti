module PacchettiBotti.GitHub
  ( Address(..)
  , SimplePR(..)
  , fetchAndSaveTags
  , fetchAndSaveCommits
  , getPullRequestForUser
  , getCommentsOnPR
  , updatePullRequestBody
  , openPR
  , pullRequestExists
  , commentOnPR
  , GitHub.Auth(..)
  , GitHub.Release(..)
  , GitHub.PullRequest(..)
  , GitHub.PullRequestCommit(..)
  , GitHub.IssueComment(..)
  , GitHub.untagName
  , GitHub.mkName
  ) where

import PacchettiBotti.Prelude

import qualified Data.Vector                   as Vector
import qualified GitHub
import qualified Data.Time as Time

import qualified PacchettiBotti.DB             as DB

import           PacchettiBotti.DB              ( Address(..) )


data SimplePR = SimplePR
  { prBranchName :: !Text
  , prAddress    :: !Address
  , prTitle      :: !Text
  , prBody       :: !Text
  }


fetchAndSaveTags
  :: HasEnv env
  => Address
  -> RIO env (Either GitHub.Error [DB.Release])
fetchAndSaveTags address@(Address owner repo) = do
  let fetchType = DB.ReleasesFetch address
  now <- liftIO Time.getCurrentTime
  shouldFetch <- DB.transact $ DB.shouldFetchHappen fetchType now
  if shouldFetch
  then do
    logInfo $ "Getting tags for " <> displayShow address
    token <- view (the @GitHub.Auth)
    res <- liftIO $ GitHub.github token $ GitHub.tagsForR owner repo GitHub.FetchAll
    for (Vector.toList . fmap mkRelease <$> res) $ \tags -> do
      DB.transact $ do
        DB.insertReleases tags
        DB.insertFetchInfo (DB.Fetch fetchType now)
      pure tags
  else do
    logDebug $ "Using cache for tags of repo " <> displayShow address
    Right <$> DB.transact (DB.getReleases address)
  where
    mkRelease GitHub.Tag{..} = DB.Release{..}
      where
        releaseTag = Tag tagName
        releaseAddress = address
        releaseBanned = False
        releaseCommit = CommitHash $ GitHub.branchCommitSha tagCommit


fetchAndSaveCommits
  :: HasEnv env
  => Address
  -> RIO env (Either GitHub.Error [DB.Commit])
fetchAndSaveCommits address@(Address owner repo) = do
  let fetchType = DB.CommitsFetch address
  now <- liftIO Time.getCurrentTime
  shouldFetch <- DB.transact $ DB.shouldFetchHappen fetchType now
  if shouldFetch
  then do
    logInfo $ "Getting commits for " <> displayShow address
    token <- view (the @GitHub.Auth)
    res <- liftIO $ GitHub.github token $ GitHub.commitsForR owner repo GitHub.FetchAll
    for (Vector.toList . fmap mkCommit <$> res) $ \commits -> do
      DB.transact $ do
        DB.insertCommits commits
        DB.insertFetchInfo (DB.Fetch fetchType now)
      pure commits
  else do
    logDebug $ "Using cache for commits of repo " <> displayShow address
    Right <$> DB.transact (DB.getCommits address)
  where
    mkCommit GitHub.Commit{..} = DB.Commit{..}
      where
        commitCommit = CommitHash $ GitHub.untagName commitSha
        commitAddress = address


getPullRequestForUser
  :: HasGitHub env
  => GitHub.Name GitHub.User
  -> Address
  -> RIO env (Maybe GitHub.PullRequest)
getPullRequestForUser user Address{..} = do
  token <- view (the @GitHub.Auth)
  maybePRs <- liftIO $ fmap hush $ GitHub.github token
    $ GitHub.pullRequestsForR owner repo GitHub.stateOpen GitHub.FetchAll
  let findPRbyUser = Vector.find
        (\GitHub.SimplePullRequest{ simplePullRequestUser = GitHub.SimpleUser{..}}
          -> simpleUserLogin == user)
  let fetchFullPR GitHub.SimplePullRequest{..}
        = liftIO
        $ fmap hush
        $ GitHub.github token
        $ GitHub.pullRequestR owner repo simplePullRequestNumber
  case (findPRbyUser =<< maybePRs :: Maybe GitHub.SimplePullRequest) of
    Nothing -> pure Nothing
    Just pr -> fetchFullPR pr


getCommentsOnPR
  :: HasGitHub env
  => Address
  -> GitHub.IssueNumber
  -> RIO env [GitHub.IssueComment]
getCommentsOnPR Address{..} issueNumber = do
  token <- view (the @GitHub.Auth)
  eitherComments <- liftIO
    $ GitHub.github token
    $ GitHub.commentsR owner repo issueNumber GitHub.FetchAll
  pure $ case eitherComments of
    Left _ -> []
    Right comments -> Vector.toList comments


updatePullRequestBody
  :: HasGitHub env
  => Address
  -> GitHub.IssueNumber
  -> Text
  -> RIO env ()
updatePullRequestBody Address{..} pullRequestNumber newBody = do
  token <- view (the @GitHub.Auth)
  void
    $ liftIO
    $ GitHub.github token
    $ GitHub.updatePullRequestR owner repo pullRequestNumber
    $ GitHub.EditPullRequest Nothing (Just newBody) Nothing Nothing Nothing


openPR :: HasGitHub env => Address -> SimplePR -> RIO env ()
openPR Address{..} SimplePR{..} = do
  logInfo "Pushed a new commit, opening PR.."
  token <- view (the @GitHub.Auth)
  response <- liftIO $ GitHub.github token
    $ GitHub.createPullRequestR owner repo
    $ GitHub.CreatePullRequest prTitle prBody prBranchName "master"
  case response of
    Right _   -> logInfo "Created PR 🎉"
    Left err' -> logError $ "Error while creating PR: " <> displayShow err'


pullRequestExists :: HasGitHub env => Address -> SimplePR -> RIO env Bool
pullRequestExists Address{..} SimplePR{..} = do
  logInfo $ "Checking if we ever opened a PR " <> displayShow prTitle

  token <- view (the @GitHub.Auth)
  oldPRs <- liftIO
    $ GitHub.github token
    $ GitHub.pullRequestsForR owner repo
    (GitHub.optionsHead (GitHub.untagName owner <> ":" <> prBranchName) <> GitHub.stateAll)
    GitHub.FetchAll
  case oldPRs of
    Left err -> do
      logError $ "Error: " <> displayShow err
      pure True
    Right prs | not $ Vector.null prs -> do
      logWarn "PR was opened, skipping.."
      pure True
    Right _ -> do
      logInfo "No previous PRs found, opening one.."
      pure False


commentOnPR :: HasGitHub env => Address -> GitHub.IssueNumber -> Text -> RIO env ()
commentOnPR Address{..} pullRequestNumber commentBody = do
  token <- view (the @GitHub.Auth)
  (liftIO $ GitHub.github token $ GitHub.createCommentR owner repo pullRequestNumber commentBody) >>= \case
    Left err -> logError $ "Something went wrong while commenting. Error: " <> displayShow err
    Right _ -> logInfo "Commented on the open PR"
