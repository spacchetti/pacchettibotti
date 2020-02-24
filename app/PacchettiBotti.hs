module PacchettiBotti where

import           Spago.Prelude

import qualified Control.Concurrent            as Concurrent
import qualified Control.Concurrent.STM.TChan  as Chan
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Encoding
import qualified GHC.IO.Encoding
import qualified System.Environment            as Env

import qualified PacchettiBotti.GitHub         as GitHub
import qualified PacchettiBotti.Threads.Common as Common
import qualified PacchettiBotti.Threads.Spago  as Spago
import qualified PacchettiBotti.Threads.PackageSets
                                               as PackageSets
import qualified PacchettiBotti.Threads.PackageSetsMetadata
                                               as Metadata

import           PacchettiBotti.Threads


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

    let spawnThread :: HasLogFunc env => Text -> (Message -> RIO LogFunc ()) -> RIO env ()
        spawnThread name thread = do
          let threadLoop :: IO ()
              threadLoop = do
                pullChan <- atomically $ Chan.dupTChan bus
                forever $ atomically (Chan.readTChan pullChan) >>= (runRIO logFunc . thread)
          logInfo $ "Spawning thread " <> displayShow name
          void $ liftIO $ Concurrent.forkIO $ catch threadLoop $ \(err :: SomeException) -> runRIO logFunc $ do
            logError $ "Thread " <> displayShow name <> " broke, restarting.."
            logError $ "Error was: " <> display err
            spawnThread name thread

    -- Start spawning threads
    --   General utility
    spawnThread "writer"                  $ Common.persistState
    --   purescript repo
    spawnThread "releaseCheckPureScript"  $ Common.checkLatestRelease token Spago.purescriptRepo
    --   purescript-docs-search repo
    spawnThread "releaseCheckDocsSearch"  $ Common.checkLatestRelease token Spago.docsSearchRepo
    --   spago repo
    spawnThread "spagoUpdatePackageSets"  $ Spago.updatePackageSets token
    spawnThread "spagoUpdateDocsSearch"   $ Spago.updateDocsSearch token
    --     TODO: update purescript-metadata repo on purs release
    spawnThread "spagoUpdatePurescript"   $ Spago.updatePurescriptVersion token
    --   package-sets-metadata repo
    spawnThread "metadataFetcher"         $ Metadata.fetcher token
    spawnThread "metadataUpdater"         $ Metadata.updater
    -- package-sets repo
    spawnThread "releaseCheckPackageSets" $ Common.checkLatestRelease token PackageSets.packageSetsRepo
    spawnThread "packageSetsUpdater"      $ PackageSets.updater token
    spawnThread "packageSetsCommenter"    $ PackageSets.commenter token

    -- To kickstart the whole thing we just need to send a "heartbeat" on the bus once an hour
    -- Threads will be listening to this and act accordingly
    forever $ do
      logInfo "Refreshing state.."
      liftIO $ atomically $ Chan.writeTChan bus RefreshState
      sleep _60m

  where
    _60m = 60 * 60 * 1000000

    sleep = liftIO . Concurrent.threadDelay
