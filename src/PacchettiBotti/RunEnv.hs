module PacchettiBotti.RunEnv where

import PacchettiBotti.Prelude

import qualified PacchettiBotti.DB as DB
import qualified Control.Concurrent.STM.TChan as Chan
import qualified GHC.IO.Encoding
import qualified Data.Text as Text
import qualified GitHub
import qualified RIO
import qualified System.Environment as Env
import qualified Data.Text.Encoding as Encoding


-- | Run the action with a fully loaded Env
withEnv :: MonadUnliftIO m => RIO Env a -> m a
withEnv action = withBinaryFile "pacchettibotti.log" AppendMode $ \configHandle -> do
  logStderr <- (setLogUseLoc False . setLogMinLevel LevelInfo) <$> logOptionsHandle stderr True
  logFile <- setLogUseLoc False <$> logOptionsHandle configHandle True

  withLogFunc logStderr $ \logFuncConsole -> withLogFunc logFile $ \logFuncFile ->
    let envLogFunc = logFuncConsole <> logFuncFile
    in runRIO envLogFunc $ do
    -- We always want to run in UTF8 anyways
    liftIO $ GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8
    -- Stop `git` from asking for input, not gonna happen
    -- We just fail instead. Source:
    -- https://serverfault.com/questions/544156
    liftIO $ Env.setEnv "GIT_TERMINAL_PROMPT" "0"

    RIO.logInfo "Reading GitHub token.."
    envGithubToken <- liftIO $ GitHub.OAuth . Encoding.encodeUtf8 . Text.pack
      <$> Env.getEnv "PACCHETTIBOTTI_GITHUB_TOKEN"

    -- Prepare data folder that will contain the temp copies of the repos
    RIO.logInfo "Creating 'data' folder"
    mktree "data"

    envBus <- liftIO Chan.newBroadcastTChanIO

    RIO.logInfo "Migrating DB.."
    envDB <- DB.mkDB

    let envLogContext = LogContext "/"
    let env = Env{..}

    runRIO env action