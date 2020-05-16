module PacchettiBotti where

import           PacchettiBotti.Prelude hiding (Config(..))

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM.TChan as Chan
import qualified Data.Aeson as Json
import qualified Network.HTTP.Simple as Http
import qualified System.Environment as Env

import qualified PacchettiBotti.RunEnv as RunEnv
import qualified PacchettiBotti.Threads.Generic as Common
import qualified PacchettiBotti.Threads.Spago as Spago
import qualified PacchettiBotti.Threads.Registry as Registry
import qualified PacchettiBotti.Registry.Bower as Registry
import qualified PacchettiBotti.Threads.PackageSets as PackageSets
import qualified PacchettiBotti.Threads.PackageSetsMetadata as Metadata

data Config = Config
  { enableHealthchecks :: Bool
  , enableRegistryBowerSync :: Bool
  , enablePackageSetsUpdate :: Bool
  , enableSetsMetadataUpdate :: Bool
  , enableVersionUpdates :: Bool
  } deriving (Show, Generic)

instance FromJSON Config


main :: IO ()
main = Json.eitherDecodeFileStrict "config.json" >>= \case
  Left err -> error err
  Right config@Config{..} -> RunEnv.withEnv $ do
    logInfo "Reading healthchecks.io token"

    maybeHealthcheckToken <-
      if enableHealthchecks
      then fmap Just $ liftIO $ Env.getEnv "PACCHETTIBOTTI_HEALTHCHECKSIO_TOKEN"
      else pure Nothing

    -- To kickstart updates we just need to send "heartbeats" on the bus every once in a while
    -- Threads will be listening to this, do stuff, post things on the bus which we handle
    -- here in the main thread
    spawnThread hourlyHeartbeat
    spawnThread dailyHeartbeat

    envBus <- view (the @Bus)
    pullChan <- atomically $ Chan.dupTChan envBus
    forever $ atomically (Chan.readTChan pullChan)
      >>= handleMessage maybeHealthcheckToken config


handleMessage :: Maybe String -> Config -> Message -> RIO Env ()
handleMessage healthchecksToken Config{..} = \case
  DailyUpdate ->
    spawnThread $ Thread
      enableRegistryBowerSync
      (LogContext "refreshBowerPkgs")
      "Bower packages daily update"
      Registry.refreshBowerPackages
  HourlyUpdate -> do
    spawnThread $ Thread True
      (LogContext "checkPursRelease")
      "Check for new purs releases"
      (Common.checkLatestRelease Spago.purescriptRepo)
    spawnThread $ Thread True
      (LogContext "checkDocsSearchRelease")
      "Check for new docs-search releases"
      (Common.checkLatestRelease Spago.docsSearchRepo)
    spawnThread $ Thread True
      (LogContext "checkPackageSetsRelease")
      "Check for new package-sets releases"
      (Common.checkLatestRelease PackageSets.packageSetsRepo)

    spawnThread $ Thread
      enableSetsMetadataUpdate
      (LogContext "fetchMetadata")
      "Fetch new metadata for packages in package-sets"
      Metadata.fetcher

    -- we curl this Healthchecks.io link every hour, otherwise Fabrizio gets emails :)
    case healthchecksToken of
      Nothing -> pure ()
      Just token -> do
        heartbeatUrl <- Http.parseRequest $ "https://hc-ping.com/" <> token
        void $ Http.httpBS heartbeatUrl

  NewPureScriptRelease ->
    spawnThread $ Thread
      enableVersionUpdates
      (LogContext "updatePureScriptVersion")
      "Update purs version in Spago CI"
      Spago.updatePurescriptVersion
    -- TODO: update purescript-metadata repo on purs release

  NewPackageSetsRelease ->
    spawnThread $ Thread
      enableVersionUpdates
      (LogContext "updatePackageSets")
      "Update package-sets version in Spago templates"
      Spago.updatePackageSets

  NewDocsSearchRelease ->
    spawnThread $ Thread
      enableVersionUpdates
      (LogContext "updateDocsSearch")
      "Update docs-search version in Spago"
      Spago.updateDocsSearch

  NewVerification cmd result ->
    spawnThread $ Thread
      enablePackageSetsUpdate
      (LogContext "packageSetCommenter")
      "Comment on new PRs in package-sets"
      (PackageSets.commenter cmd result)

  NewMetadata -> do
    spawnThread $ Thread
      enablePackageSetsUpdate
      (LogContext "packageSetsUpdater")
      "Update packages in package-sets"
      PackageSets.updater
    spawnThread $ Thread
      enableSetsMetadataUpdate
      (LogContext "metadataUpdater")
      "Push new metadata about packages in package-sets"
      Metadata.updater

  NewPackageSet -> pure ()

  NewBowerRefresh ->
    spawnThread $ Thread
      enableRegistryBowerSync
      (LogContext "syncFromBower")
      "Sync Bower packages to the Registry"
      Registry.syncFromBower

data Thread = Thread
  { threadEnabled :: !Bool
  , threadContext :: !LogContext
  , threadDescription :: !Text
  , threadFn :: forall env. HasEnv env => RIO env ()
  }

spawnThread :: Thread -> RIO Env ()
spawnThread Thread{..} = addLogContext threadContext $
  if not threadEnabled
  then logInfo ("Thread " <> display threadContext <> " is disabled")
  else do
    env <- view id
    logInfo $ "Spawning thread " <> displayShow threadDescription
    void
      $ liftIO $ Concurrent.forkIO
      $ catch (runRIO env threadFn)
      $ \(err :: SomeException) -> runRIO env $ do
        logError $ "Thread " <> display threadContext <> " broke, error was:"
        logError $ display err


hourlyHeartbeat :: Thread
hourlyHeartbeat = Thread{..}
  where
    threadEnabled = True
    threadContext = LogContext "hourlyHearbeat"
    threadDescription = "Hourly Heartbeat"

    threadFn :: (HasLog env, HasBus env) => RIO env ()
    threadFn = forever $ do
      logDebug "Sending hourly heartbeat"
      writeBus HourlyUpdate
      sleep _60m

dailyHeartbeat :: Thread
dailyHeartbeat = Thread{..}
  where
    threadEnabled = True
    threadContext = LogContext "dailyHearbeat"
    threadDescription = "Daily Heartbeat"

    threadFn :: (HasLog env, HasBus env) => RIO env ()
    threadFn = forever $ do
      logDebug "Sending daily heartbeat"
      writeBus DailyUpdate
      sleep _24h