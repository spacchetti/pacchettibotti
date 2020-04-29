module PacchettiBotti where

import           PacchettiBotti.Prelude

import qualified Control.Concurrent            as Concurrent
import qualified Control.Concurrent.STM.TChan  as Chan
import qualified Network.HTTP.Simple           as Http

import qualified PacchettiBotti.Threads.Generic
                                               as Common
import qualified PacchettiBotti.Threads.Spago  as Spago
import qualified PacchettiBotti.Threads.Registry
                                               as Registry
import qualified PacchettiBotti.Threads.PackageSets
                                               as PackageSets
import qualified PacchettiBotti.Threads.PackageSetsMetadata
                                               as Metadata


main :: IO ()
main = withEnv $ do
  -- To kickstart updates we just need to send "heartbeats" on the bus every once in a while
  -- Threads will be listening to this, do stuff, post things on the bus which we handle
  -- here in the main thread
  spawnThread "Hourly Heartbeat" hourlyHeartbeat
  spawnThread "Daily Heartbeat" dailyHeartbeat

  envBus <- view busL
  pullChan <- atomically $ Chan.dupTChan envBus
  forever $ atomically (Chan.readTChan pullChan) >>= handleMessage


handleMessage :: HasEnv env => Message -> RIO env ()
handleMessage = \case
  DailyUpdate ->
    spawnThread "Bower packages daily update" $ Registry.refreshBowerPackages
  HourlyUpdate -> do
    spawnThread "releaseCheckPureScript" $ Common.checkLatestRelease Spago.purescriptRepo
    spawnThread "releaseCheckDocsSearch" $ Common.checkLatestRelease Spago.docsSearchRepo
    spawnThread "releaseCheckPackageSets" $ Common.checkLatestRelease PackageSets.packageSetsRepo
    spawnThread "metadataFetcher" Metadata.fetcher
    -- we curl this Healthchecks.io link every hour, otherwise Fabrizio gets emails :)
    Env{ envHealthchecksToken } <- view envL
    heartbeatUrl <- Http.parseRequest $ "https://hc-ping.com/" <> envHealthchecksToken
    void $ Http.httpBS heartbeatUrl

  NewPureScriptRelease ->
    spawnThread "spagoUpdatePurescript" Spago.updatePurescriptVersion
    -- TODO: update purescript-metadata repo on purs release

  NewPackageSetsRelease ->
    spawnThread "spagoUpdatePackageSets" Spago.updatePackageSets

  NewDocsSearchRelease ->
    spawnThread "spagoUpdateDocsSearch" Spago.updateDocsSearch

  NewVerification cmd result ->
    spawnThread "packageSetsCommenter" $ PackageSets.commenter cmd result

  NewMetadata -> do
    spawnThread "packageSetsUpdater" PackageSets.updater
    spawnThread "metadataUpdater"    Metadata.updater

  NewPackageSet -> pure ()

  NewBowerRefresh -> pure () -- TODO: do something after we refresh releases from there


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


hourlyHeartbeat :: (HasLogFunc env, HasBus env) => RIO env ()
hourlyHeartbeat = forever $ do
  logDebug "Sending hourly heartbeat"
  writeBus HourlyUpdate
  sleep _60m


dailyHeartbeat :: (HasLogFunc env, HasBus env) => RIO env ()
dailyHeartbeat = forever $ do
  logDebug "Sending daily heartbeat"
  writeBus DailyUpdate
  sleep _24h
