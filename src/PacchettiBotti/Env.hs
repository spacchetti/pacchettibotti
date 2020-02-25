module PacchettiBotti.Env where

import           Spago.Prelude           hiding ( Env )

import qualified Control.Concurrent            as Concurrent
import qualified Control.Concurrent.STM.TChan  as Chan
import qualified GitHub
import qualified PacchettiBotti.DB             as DB
import qualified Data.Text as Text

import           Spago.GlobalCache              ( ReposMetadataV1
                                                , Tag(..)
                                                )
import           Spago.Types


-- TODO: rename this to envLogFunc
data Env = Env
  { envLogFun :: !LogFunc
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

class HasDB env where
  dbL :: Lens' env DB.Handle

class HasBus env where
  busL :: Lens' env (Chan.TChan Message)
  writeBus :: Message -> RIO env ()

class HasState env where
  stateL :: Lens' env (Concurrent.MVar State)
  readState :: RIO env State
  updateState :: (State -> IO State) -> RIO env ()

class ( HasLogFunc env
      , HasGitHubToken env
      , HasDB env
      , HasBus env
      , HasState env
      ) => HasEnv env where
  envL :: Lens' env Env

instance HasLogFunc Env where
  logFuncL = lens envLogFun (\x y -> x { envLogFun = y })

instance HasGitHubToken Env where
  githubTokenL = lens envGithubToken (\x y -> x { envGithubToken = y })

instance HasDB Env where
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



type PackageSetMap = Map PackageName Package
type VerificationResult = (ExitCode, Text, Text)

data Message
  = RefreshState
  | NewRepoRelease !Address !Text
  | NewPackageSet !PackageSetMap
  | NewMetadata !ReposMetadataV1
  | NewVerification !VerificationResult
  deriving (Show)

data State = State
  { latestReleases :: !(Map Address Text)
  , packageSet     :: !PackageSetMap
  , metadata       :: !ReposMetadataV1
  , banned         :: !(Set (PackageName, Tag))
  } deriving (Show)

emptyState :: State
emptyState = State{..}
  where
    latestReleases = mempty
    packageSet = mempty
    metadata = mempty
    banned = mempty


data Address = Address
  { owner :: GitHub.Name GitHub.Owner
  , repo  :: GitHub.Name GitHub.Repo
  } deriving (Eq, Ord)

instance Show Address where
  show (Address owner repo) = Text.unpack
    $ "\"" <> GitHub.untagName owner <> "/" <> GitHub.untagName repo <> "\""
