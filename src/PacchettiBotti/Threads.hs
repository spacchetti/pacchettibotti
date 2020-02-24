module PacchettiBotti.Threads where

import           Spago.Prelude

import qualified Control.Concurrent            as Concurrent
import qualified GHC.IO
import qualified Control.Concurrent.STM.TChan  as Chan

import           Spago.GlobalCache              ( ReposMetadataV1
                                                , Tag(..)
                                                )
import           Spago.Types

import qualified PacchettiBotti.GitHub         as GitHub


type PackageSetMap = Map PackageName Package

data Message
  = RefreshState
  | NewRepoRelease !GitHub.Address !Text
  | NewPackageSet !PackageSetMap
  | NewMetadata !ReposMetadataV1
  | NewVerification !(ExitCode, Text, Text)
  deriving (Show)

data State = State
  { latestReleases :: !(Map GitHub.Address Text)
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


-- | Concurrent-safe global state, so we can read data in here instead of
--   having to pass it around.
state :: Concurrent.MVar State
state = GHC.IO.unsafePerformIO $ Concurrent.newMVar emptyState


-- | Main message bus. It is write-only so you should use `spawnThread` to read from it
bus :: Chan.TChan Message
bus = GHC.IO.unsafePerformIO Chan.newBroadcastTChanIO
