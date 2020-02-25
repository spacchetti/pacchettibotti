module PacchettiBotti.GitHub
  ( Address(..)
  , SimplePR(..)
  , getLatestRelease
  , getTags
  , getCommits
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

import Spago.Prelude hiding (Env)

import qualified Data.Vector                   as Vector
import qualified Data.Map                      as Map
import qualified Data.Text                     as Text
import qualified GitHub

import           Spago.GlobalCache              ( CommitHash(..)
                                                , Tag(..)
                                                )
import PacchettiBotti.Env


data Address = Address
  { owner :: GitHub.Name GitHub.Owner
  , repo  :: GitHub.Name GitHub.Repo
  } deriving (Eq, Ord)

instance Show Address where
  show (Address owner repo) = Text.unpack
    $ "\"" <> GitHub.untagName owner <> "/" <> GitHub.untagName repo <> "\""

data SimplePR = SimplePR
  { prBranchName :: !Text
  , prAddress    :: !Address
  , prTitle      :: !Text
  , prBody       :: !Text
  }

getLatestRelease
  :: HasGitHub env
  => Address
  -> RIO env (Either GitHub.Error GitHub.Release)
getLatestRelease address@(Address owner repo) = do
  logInfo $ "Getting latest release for " <> displayShow address
  token <- view githubTokenL
  liftIO $ GitHub.github token $ GitHub.latestReleaseR owner repo


getTags
  :: HasGitHub env
  => Address
  -> RIO env (Either GitHub.Error (Maybe Tag, Map Tag CommitHash))
getTags address@(Address owner repo) = do
  logInfo $ "Getting tags for " <> displayShow address
  token <- view githubTokenL
  res <- liftIO $ GitHub.github token $ GitHub.tagsForR owner repo GitHub.FetchAll
  let f vec =
        ( Tag . GitHub.tagName <$> vec Vector.!? 0
        , Map.fromList
          $ Vector.toList
          $ fmap (\t ->
                    ( Tag $ GitHub.tagName t
                    , CommitHash $ GitHub.branchCommitSha $ GitHub.tagCommit t
                    ))
          vec
        )
  pure (fmap f res)


getCommits
  :: HasGitHub env
  => Address
  -> RIO env (Either GitHub.Error [CommitHash])
getCommits address@(Address owner repo) = do
  logInfo $ "Getting commits for " <> displayShow address
  token <- view githubTokenL
  res <- liftIO $ GitHub.github token $ GitHub.commitsForR owner repo GitHub.FetchAll
  pure $ fmap (Vector.toList . fmap (CommitHash . GitHub.untagName . GitHub.commitSha)) res


getPullRequestForUser
  :: HasGitHub env
  => GitHub.Name GitHub.User
  -> Address
  -> RIO env (Maybe GitHub.PullRequest)
getPullRequestForUser user Address{..} = do
  token <- view githubTokenL
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
  -- TODO: there must be a nice way to lift this instead of casing
  case (findPRbyUser =<< maybePRs :: Maybe GitHub.SimplePullRequest) of
    Nothing -> pure Nothing
    Just pr -> fetchFullPR pr


getCommentsOnPR
  :: HasGitHub env
  => Address
  -> GitHub.IssueNumber
  -> RIO env [GitHub.IssueComment]
getCommentsOnPR Address{..} issueNumber = do
  token <- view githubTokenL
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
  token <- view githubTokenL
  void
    $ liftIO
    $ GitHub.github token
    $ GitHub.updatePullRequestR owner repo pullRequestNumber
    $ GitHub.EditPullRequest Nothing (Just newBody) Nothing Nothing Nothing


openPR :: HasGitHub env => Address -> SimplePR -> RIO env ()
openPR Address{..} SimplePR{..} = do
  logInfo "Pushed a new commit, opening PR.."
  token <- view githubTokenL
  response <- liftIO $ GitHub.github token
    $ GitHub.createPullRequestR owner repo
    $ GitHub.CreatePullRequest prTitle prBody prBranchName "master"
  case response of
    Right _   -> logInfo "Created PR 🎉"
    Left err' -> logError $ "Error while creating PR: " <> displayShow err'


pullRequestExists :: HasGitHub env => Address -> SimplePR -> RIO env Bool
pullRequestExists Address{..} SimplePR{..} = do
  logInfo $ "Checking if we ever opened a PR " <> displayShow prTitle

  token <- view githubTokenL
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
  token <- view githubTokenL
  (liftIO $ GitHub.github token $ GitHub.createCommentR owner repo pullRequestNumber commentBody) >>= \case
    Left err -> logError $ "Something went wrong while commenting. Error: " <> displayShow err
    Right _ -> logInfo "Commented on the open PR"
