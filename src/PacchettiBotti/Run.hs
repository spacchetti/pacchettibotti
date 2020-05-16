module PacchettiBotti.Run where

import           PacchettiBotti.Prelude

import qualified GHC.IO
import qualified Spago.Dhall                   as Dhall
import qualified Data.Text                     as Text
import qualified Data.Time                     as Time
import qualified System.Process                as Process
import qualified System.IO.Temp                as Temp
import qualified Dhall.Core

import qualified PacchettiBotti.GitHub         as GitHub


type Expr = Dhall.DhallExpr Dhall.Import


runAndPushBranch
  :: HasLog env
  => Text
  -> GitHub.Address
  -> Text
  -> (GHC.IO.FilePath -> RIO env ())
  -> [Text]
  -> RIO env ()
runAndPushBranch branchName address commit preAction commands
  = runInClonedRepo address branchName commit preAction commands (pure ())


runAndPushMaster
  :: HasLog env
  => GitHub.Address
  -> Text
  -> (GHC.IO.FilePath -> RIO env ())
  -> [Text]
  -> RIO env ()
runAndPushMaster = runAndPushBranch "master"


runAndOpenPR
  :: HasGitHub env
  => GitHub.SimplePR
  -> (GHC.IO.FilePath -> RIO env ())
  -> [Text]
  -> RIO env ()
runAndOpenPR pr@GitHub.SimplePR{ prAddress = address@GitHub.Address{..}, ..} preAction commands
  = unlessM (GitHub.pullRequestExists address pr) $ do
  let openPR = GitHub.openPR address pr
  runInClonedRepo address prBranchName prTitle preAction commands openPR


runInClonedRepo
  :: HasLog env
  => GitHub.Address
  -> Text
  -> Text
  -> (GHC.IO.FilePath -> RIO env ())
  -> [Text]
  -> RIO env ()
  -> RIO env ()
runInClonedRepo address@GitHub.Address{..} branchName commit preAction commands postAction =
  -- Clone the repo in a temp folder
  Temp.withTempDirectory "data" "__temp-repo" $ \path -> do
    let repoPath = Text.unpack $ GitHub.untagName repo
    let runInRepo cmds failure success = do
          (code, out, err) <- runWithCwd (path </> repoPath) $ Text.intercalate " && " cmds
          if code /= ExitSuccess
            then do
              failure
              logInfo $ display out
              logError $ display err
            else success

    (code, _out, _err) <- runWithCwd path $ "git clone git@github.com:" <> GitHub.untagName owner <> "/" <> GitHub.untagName repo <> ".git"
    if code /= ExitSuccess
      then do
        logError "Error while cloning repo"
        logError $ "Stdout: " <> display _out
        logError $ "Stderr: " <> display _err
      else do
        logInfo $ "Cloned " <> displayShow address
        -- Configure the repo: set the git identity to spacchettibotti and switch to the branch
        runInRepo
          [ "git config --local user.name 'Pacchettibotti'"
          , "git config --local user.email 'pacchettibotti@ferrai.io'"
          , "git checkout " <> branchName <> " || git checkout -b " <> branchName
          ]
          (logError "Failed to configure the repo")
          -- If the setup was fine, run the setup code before running the commands
          (preAction =<< makeAbsolute (path </> repoPath))
        -- Run the commands we wanted to run
        runInRepo
          commands
          (logError "Something was off while running commands..")
          -- Check if anything actually changed or got staged
          (runInRepo
            [ "git diff --staged --exit-code" ]
            (runInRepo
              [ "git commit -m '" <> commit <> "'"
              , "git push --set-upstream origin " <> branchName
              ]
              (logError "Failed to commit!")
              postAction)
            (logInfo "Nothing to commit, skipping.."))


runWithCwd :: HasLog env => GHC.IO.FilePath -> Text -> RIO env (ExitCode, Text, Text)
runWithCwd cwd cmd = do
  logDebug $ "Running in path " <> displayShow cwd <> ": `" <> display cmd <> "`"
  let processWithNewCwd = (Process.shell (Text.unpack cmd)) { Process.cwd = Just cwd }
  systemStrictWithErr processWithNewCwd empty


getLatestCommitTime :: HasLog env => GHC.IO.FilePath -> RIO env Time.UTCTime
getLatestCommitTime path = do
  (_code, out, _err) <- runWithCwd path "git show -s --format=%ci"
  Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S %z" $ Text.unpack out


withAST :: HasLog env => Text -> (Expr -> RIO env Expr) -> RIO env ()
withAST path transform = do
  rawConfig <- liftIO $ Dhall.readRawExpr path
  case rawConfig of
    Nothing -> logWarn $ "Could not find file " <> display path
    Just (header, expr) -> do
      newExpr <- transformMExpr transform expr
      logInfo $ "Done. Updating the \"" <> display path <> "\" file.."
      writeTextFile path $ Dhall.prettyWithHeader header newExpr <> "\n"
      liftIO $ Dhall.format path
  where
    transformMExpr
      :: Monad m
      => (Dhall.Expr s Dhall.Import -> m (Dhall.Expr s Dhall.Import))
      -> Dhall.Expr s Dhall.Import
      -> m (Dhall.Expr s Dhall.Import)
    transformMExpr rules =
      transformMOf
        Dhall.subExpressions
        rules
        . Dhall.Core.denote
