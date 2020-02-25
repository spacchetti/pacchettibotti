module PacchettiBotti where

import           PacchettiBotti.Prelude

import qualified Control.Concurrent            as Concurrent
import qualified Control.Concurrent.STM.TChan  as Chan
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Encoding
import qualified GHC.IO.Encoding
import qualified System.Environment            as Env
import qualified Data.Map as Map

import qualified PacchettiBotti.DB             as DB
import qualified PacchettiBotti.GitHub         as GitHub
import qualified PacchettiBotti.Threads.Common as Common
import qualified PacchettiBotti.Threads.Spago  as Spago
import qualified PacchettiBotti.Threads.PackageSets
                                               as PackageSets
import qualified PacchettiBotti.Threads.PackageSetsMetadata
                                               as Metadata


main :: IO ()
main = withBinaryFile "pacchettibotti.log" AppendMode $ \configHandle -> do
  logStderr <- setLogUseLoc False <$> logOptionsHandle stderr True
  logFile <- setLogUseLoc False <$> logOptionsHandle configHandle True

  withLogFunc logStderr $ \logFuncConsole -> withLogFunc logFile $ \logFuncFile ->
    let logFunc = logFuncConsole <> logFuncFile
    in runRIO logFunc $ do
    -- We always want to run in UTF8 anyways
    liftIO $ GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8
    -- Stop `git` from asking for input, not gonna happen
    -- We just fail instead. Source:
    -- https://serverfault.com/questions/544156
    liftIO $ Env.setEnv "GIT_TERMINAL_PROMPT" "0"

    -- Read GitHub Auth Token
    logInfo "Reading GitHub token.."
    token <- liftIO $ GitHub.OAuth . Encoding.encodeUtf8 . Text.pack
      <$> Env.getEnv "SPACCHETTIBOTTI_TOKEN"

    -- Prepare data folder that will contain the temp copies of the repos
    logInfo "Creating 'data' folder"
    mktree "data"

    state <- liftIO $ Concurrent.newMVar emptyState
    bus <- liftIO Chan.newBroadcastTChanIO

    let env =
          let
            envLogFun = logFunc
            envGithubToken = token
            envBus = bus
            envState = state
            envDB = DB.Handle -- TODO
          in Env{..}

    runRIO env $ do
      -- To kickstart updates we just need to send a "heartbeat" on the bus once an hour
      -- Threads will be listening to this, do stuff, post things on the bus which we handle
      -- here in the main thread
      spawnThread "Heartbeat" heartbeat

      pullChan <- atomically $ Chan.dupTChan bus
      forever $ atomically (Chan.readTChan pullChan) >>= handleMessage


handleMessage :: HasEnv env => Message -> RIO env ()
handleMessage = \case
  RefreshState -> do
    spawnThread "releaseCheckPureScript"
      $ Common.checkLatestRelease Spago.purescriptRepo
    spawnThread "releaseCheckDocsSearch"
      $ Common.checkLatestRelease Spago.docsSearchRepo
    spawnThread "releaseCheckPackageSets"
      $ Common.checkLatestRelease PackageSets.packageSetsRepo
    spawnThread "metadataFetcher" Metadata.fetcher
  NewRepoRelease address release -> do
    updateState
      $ \State{..} -> let newReleases = Map.insert address release latestReleases
                      in pure State{ latestReleases = newReleases , ..}
    case address of
      a | a == PackageSets.packageSetsRepo -> spawnThread "spagoUpdatePackageSets"
          $ Spago.updatePackageSets release
      a | a == Spago.purescriptRepo -> spawnThread "spagoUpdatePurescript"
          $ Spago.updatePurescriptVersion release
        --     TODO: update purescript-metadata repo on purs release
      a | a == Spago.docsSearchRepo -> spawnThread "spagoUpdateDocsSearch"
          $ Spago.updateDocsSearch release
      _ -> logWarn $ "Got unexpected release update: " <> displayShow address
  NewVerification result ->
    spawnThread "packageSetsCommenter" $ PackageSets.commenter result
  NewMetadata newMetadata -> do
    updateState
      $ \State{..} -> pure State{ metadata = newMetadata, ..}
    spawnThread "packageSetsUpdater" $ PackageSets.updater newMetadata
    spawnThread "metadataUpdater" $ Metadata.updater newMetadata
  NewPackageSet newPackageSet ->
    updateState
      $ \State{..} -> pure State{ packageSet = newPackageSet, ..}


spawnThread :: HasEnv env => Text -> RIO Env () -> RIO env ()
spawnThread name thread = do
  env <- view envL
  logInfo $ "Spawning thread " <> displayShow name
  void
    $ liftIO $ Concurrent.forkIO
    $ catch (runRIO env thread)
    $ \(err :: SomeException) -> runRIO env $ do
      logError $ "Thread " <> displayShow name <> " broke, error was:"
      logError $ display err

heartbeat :: (HasLogFunc env, HasBus env) => RIO env ()
heartbeat = forever $ do
  logInfo "Refreshing state.."
  writeBus RefreshState
  sleep _60m
  where
    _60m = 60 * 60 * 1000000
    sleep = liftIO . Concurrent.threadDelay
