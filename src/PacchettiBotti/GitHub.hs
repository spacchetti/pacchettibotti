module PacchettiBotti.GitHub
  ( Address(..)
  , SimplePR(..)
  , GitHubEnv
  , getLatestRelease
  , getTags
  , getCommits
  , getPullRequestForUser
  , getCommentsOnPR
  , updatePullRequestBody
  , openPR
  , pullRequestExists
  , commentOnPR
  , GitHub.AuthMethod(..)
  , GitHub.Auth(..)
  , GitHub.Release(..)
  , GitHub.PullRequest(..)
  , GitHub.PullRequestCommit(..)
  , GitHub.IssueComment(..)
  , GitHub.untagName
  , GitHub.mkName
  ) where

import Spago.Prelude

import qualified Data.Vector                   as Vector
import qualified Data.Map                      as Map
import qualified Data.Text                     as Text
import qualified GitHub

import           Spago.GlobalCache              ( CommitHash(..)
                                                , Tag(..)
                                                )


type GitHubEnv env am = (HasLogFunc env, GitHub.AuthMethod am)

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
  :: GitHubEnv env am
  => am
  -> Address
  -> RIO env (Either GitHub.Error GitHub.Release)
getLatestRelease token address@(Address owner repo) = do
  logInfo $ "Getting latest release for " <> displayShow address
  liftIO $ GitHub.github token $ GitHub.latestReleaseR owner repo


getTags
  :: GitHubEnv env am
  => am
  -> Address
  -> RIO env (Either GitHub.Error (Maybe Tag, Map Tag CommitHash))
getTags token address@(Address owner repo) = do
  logInfo $ "Getting tags for " <> displayShow address
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
  :: GitHubEnv env am
  => am
  -> Address
  -> RIO env (Either GitHub.Error [CommitHash])
getCommits token address@(Address owner repo) = do
  logInfo $ "Getting commits for " <> displayShow address
  res <- liftIO $ GitHub.github token $ GitHub.commitsForR owner repo GitHub.FetchAll
  pure $ fmap (Vector.toList . fmap (CommitHash . GitHub.untagName . GitHub.commitSha)) res


getPullRequestForUser
  :: GitHubEnv env am
  => am
  -> GitHub.Name GitHub.User
  -> Address
  -> RIO env (Maybe GitHub.PullRequest)
getPullRequestForUser token user Address{..} = do
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
  :: GitHubEnv env am
  => am
  -> Address
  -> GitHub.IssueNumber
  -> RIO env [GitHub.IssueComment]
getCommentsOnPR token Address{..} issueNumber = do
  eitherComments <- liftIO
    $ GitHub.github token
    $ GitHub.commentsR owner repo issueNumber GitHub.FetchAll
  pure $ case eitherComments of
    Left _ -> []
    Right comments -> Vector.toList comments


updatePullRequestBody
  :: GitHubEnv env am
  => am
  -> Address
  -> GitHub.IssueNumber
  -> Text
  -> RIO env ()
updatePullRequestBody token Address{..} pullRequestNumber newBody =
  void
    $ liftIO
    $ GitHub.github token
    $ GitHub.updatePullRequestR owner repo pullRequestNumber
    $ GitHub.EditPullRequest Nothing (Just newBody) Nothing Nothing Nothing


openPR :: GitHubEnv env am => am -> Address -> SimplePR -> RIO env ()
openPR token Address{..} SimplePR{..} = do
  logInfo "Pushed a new commit, opening PR.."
  response <- liftIO $ GitHub.github token
    $ GitHub.createPullRequestR owner repo
    $ GitHub.CreatePullRequest prTitle prBody prBranchName "master"
  case response of
    Right _   -> logInfo "Created PR 🎉"
    Left err' -> logError $ "Error while creating PR: " <> displayShow err'


pullRequestExists :: GitHubEnv env am => am -> Address -> SimplePR -> RIO env Bool
pullRequestExists token Address{..} SimplePR{..} = do
  logInfo $ "Checking if we ever opened a PR " <> displayShow prTitle

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


commentOnPR :: GitHubEnv env am => am -> Address -> GitHub.IssueNumber -> Text -> RIO env ()
commentOnPR token Address{..} pullRequestNumber commentBody =
  (liftIO $ GitHub.github token $ GitHub.createCommentR owner repo pullRequestNumber commentBody) >>= \case
  Left err -> logError $ "Something went wrong while commenting. Error: " <> displayShow err
  Right _ -> logInfo "Commented on the open PR"
