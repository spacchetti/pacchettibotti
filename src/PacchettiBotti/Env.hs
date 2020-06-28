module PacchettiBotti.Env where

import           Spago.Prelude

import qualified Control.Concurrent.STM.TChan  as Chan
import qualified GitHub
import qualified PacchettiBotti.DB.Handle      as DB

import Data.Generics.Product (the, HasType)


newtype LogContext = LogContext { unLogContext :: Utf8Builder }
  deriving (Display, Generic, Semigroup, Monoid)

type HasLog env = (HasType LogFunc env, HasType LogContext env)
type HasGitHub env = (HasLog env, HasType GitHub.Auth env)
type HasDB = HasType DB.Handle
type HasBus = HasType Bus
type HasEnv env =
  ( HasLog env
  , HasLogFunc env
  , HasType GitHub.Auth env
  , HasType DB.Handle env
  , HasType Bus env
  , HasResourceMap env
  )

data Env = Env
  { envLogFunc :: !LogFunc
  , envLogContext :: !LogContext
  , envGithubToken :: !GitHub.Auth
  , envDB :: !DB.Handle
  -- | Main message bus. It is write-only so you should use `spawnThread` to read from it
  , envBus :: !Bus
  , envResourceMap :: ResourceMap
  } deriving (Generic)

instance HasLogFunc Env where
  logFuncL = lens envLogFunc (\x y -> x { envLogFunc = y })

instance HasResourceMap Env where
  resourceMapL = lens envResourceMap (\x y -> x { envResourceMap = y })

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

type Bus = Chan.TChan Message
type VerificationResult = (ExitCode, Text, Text)


writeBus :: HasType Bus env => Message -> RIO env ()
writeBus msg = do
  bus <- view (the @Bus)
  atomically $ Chan.writeTChan bus msg
