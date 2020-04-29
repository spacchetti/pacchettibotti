module PacchettiBotti.Env
  ( module PacchettiBotti.Env
  , DB.HasDB
  ) where

import           Spago.Prelude           hiding ( Env, HasEnv )

import qualified Control.Concurrent.STM.TChan  as Chan
import qualified GitHub
import qualified PacchettiBotti.DB             as DB
import qualified GHC.IO.Encoding
import qualified Data.Text                     as Text
import qualified System.Environment            as Env
import qualified Data.Text.Encoding            as Encoding

data Env = Env
  { envLogFunc :: !LogFunc
  , envGithubToken :: !GitHub.Auth
  , envHealthchecksToken :: !String
  , envDB :: !DB.Handle
  -- | Main message bus. It is write-only so you should use `spawnThread` to read from it
  , envBus :: !(Chan.TChan Message)
  }


class HasGitHubToken env where
  githubTokenL :: Lens' env GitHub.Auth

class HasBus env where
  busL :: Lens' env (Chan.TChan Message)
  writeBus :: Message -> RIO env ()

class ( HasLogFunc env
      , HasGitHubToken env
      , DB.HasDB env
      , HasBus env
      ) => HasEnv env where
  envL :: Lens' env Env

instance HasLogFunc Env where
  logFuncL = lens envLogFunc (\x y -> x { envLogFunc = y })

instance HasGitHubToken Env where
  githubTokenL = lens envGithubToken (\x y -> x { envGithubToken = y })

instance DB.HasDB Env where
  dbL = lens envDB (\x y -> x { envDB = y })

instance HasBus Env where
  busL = lens envBus (\x y -> x { envBus = y })
  writeBus msg = do
    bus <- view busL
    atomically $ Chan.writeTChan bus msg

instance HasEnv Env where
  envL = id

type HasGitHub env = (HasLogFunc env, HasGitHubToken env)

type VerificationResult = (ExitCode, Text, Text)

data Message
  = HourlyUpdate
  | DailyUpdate
  | NewPureScriptRelease
  | NewDocsSearchRelease
  | NewPackageSetsRelease
  | NewPackageSet
  | NewMetadata
  | NewBowerRefresh
  | NewVerification !Text !VerificationResult
  deriving (Show)


-- | Run the action with a fully loaded Env
withEnv :: MonadUnliftIO m => RIO Env a -> m a
withEnv action = withBinaryFile "pacchettibotti.log" AppendMode $ \configHandle -> do
  logStderr <- setLogUseLoc False <$> logOptionsHandle stderr True
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

    logInfo "Reading GitHub token.."
    envGithubToken <- liftIO $ GitHub.OAuth . Encoding.encodeUtf8 . Text.pack
      <$> Env.getEnv "PACCHETTIBOTTI_GITHUB_TOKEN"

    logInfo "Reading healthchecks.io token"
    envHealthchecksToken <- liftIO $ Env.getEnv "PACCHETTIBOTTI_HEALTHCHECKSIO_TOKEN"

    -- Prepare data folder that will contain the temp copies of the repos
    logInfo "Creating 'data' folder"
    mktree "data"

    envBus <- liftIO Chan.newBroadcastTChanIO

    logInfo "Migrating DB.."
    envDB <- DB.mkDB

    let env = Env{..}

    runRIO env action