module PacchettiBotti.Env
  ( module PacchettiBotti.Env
  , DB.HasDB
  ) where

import           Spago.Prelude           hiding ( Env, HasEnv )

import qualified Control.Concurrent.STM.TChan  as Chan
import qualified GitHub
import qualified PacchettiBotti.DB             as DB


data Env = Env
  { envLogFunc :: !LogFunc
  , envGithubToken :: !GitHub.Auth
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
  | NewVerification !VerificationResult
  deriving (Show)