module PacchettiBotti.Env
  ( module PacchettiBotti.Env
  , DB.HasDB
  ) where

import           Spago.Prelude           hiding ( Env, HasEnv )

import qualified Control.Concurrent            as Concurrent
import qualified Control.Concurrent.STM.TChan  as Chan
import qualified GitHub
import qualified PacchettiBotti.DB             as DB

import           Spago.GlobalCache              ( Tag(..) )
import           Spago.Types
import           PacchettiBotti.Types


data Env = Env
  { envLogFunc :: !LogFunc
  , envGithubToken :: !GitHub.Auth
  , envDB :: !DB.Handle
  -- | Main message bus. It is write-only so you should use `spawnThread` to read from it
  , envBus :: !(Chan.TChan Message)
  -- | Concurrent-safe global state, so we can read data in here instead of
  --   having to pass it around.
  , envState :: !(Concurrent.MVar State)
  }


class HasGitHubToken env where
  githubTokenL :: Lens' env GitHub.Auth

class HasBus env where
  busL :: Lens' env (Chan.TChan Message)
  writeBus :: Message -> RIO env ()

class HasState env where
  stateL :: Lens' env (Concurrent.MVar State)
  readState :: RIO env State
  updateState :: (State -> IO State) -> RIO env ()

class ( HasLogFunc env
      , HasGitHubToken env
      , DB.HasDB env
      , HasBus env
      , HasState env
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

instance HasState Env where
  stateL = lens envState (\x y -> x { envState = y })
  readState = do
    state <- view stateL
    liftIO $ Concurrent.readMVar state
  updateState f = do
    state <- view stateL
    liftIO $ Concurrent.modifyMVar_ state f

instance HasEnv Env where
  envL = id

type HasGitHub env = (HasLogFunc env, HasGitHubToken env)

type VerificationResult = (ExitCode, Text, Text)

data Message
  = HourlyUpdate
  | DailyUpdate
  | NewRepoRelease !Address !Text
  | NewVerification !VerificationResult
  | NewPackageSet
  | NewMetadata
  | NewBowerRefresh
  deriving (Show)

data State = State
  { latestReleases :: !(Map Address Text)
  , banned         :: !(Set (PackageName, Tag))
  } deriving (Show)

emptyState :: State
emptyState = State{..}
  where
    latestReleases = mempty
    banned = mempty
