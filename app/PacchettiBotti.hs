module PacchettiBotti where

import           PacchettiBotti.Prelude hiding (Config(..))

import qualified Control.Concurrent            as Concurrent
import qualified Control.Concurrent.STM.TChan  as Chan
import qualified Data.Aeson                    as Json
import qualified Network.HTTP.Simple           as Http
import qualified System.Environment            as Env

import qualified PacchettiBotti.Threads.Generic
                                               as Common
import qualified PacchettiBotti.Threads.Spago  as Spago
import qualified PacchettiBotti.Threads.Registry
                                               as Registry
import qualified PacchettiBotti.Registry.Bower as Registry
import qualified PacchettiBotti.Threads.PackageSets
                                               as PackageSets
import qualified PacchettiBotti.Threads.PackageSetsMetadata
                                               as Metadata

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
  Right config@Config{..} -> withEnv $ do
    logInfo "Reading healthchecks.io token"

    maybeHealthcheckToken <-
      if enableHealthchecks
      then fmap Just $ liftIO $ Env.getEnv "PACCHETTIBOTTI_HEALTHCHECKSIO_TOKEN"
      else pure Nothing

    -- To kickstart updates we just need to send "heartbeats" on the bus every once in a while
    -- Threads will be listening to this, do stuff, post things on the bus which we handle
    -- here in the main thread
    spawnThread "Hourly Heartbeat" hourlyHeartbeat
    spawnThread "Daily Heartbeat" dailyHeartbeat

    envBus <- view busL
    pullChan <- atomically $ Chan.dupTChan envBus
    forever $ atomically (Chan.readTChan pullChan)
      >>= handleMessage maybeHealthcheckToken config


handleMessage :: HasEnv env => Maybe String -> Config -> Message -> RIO env ()
handleMessage healthchecksToken Config{..} = \case
  DailyUpdate ->
    when enableRegistryBowerSync $
      spawnThread "Bower packages daily update" $ Registry.refreshBowerPackages
  HourlyUpdate -> do
    spawnThread "Check for new purs releases" $ Common.checkLatestRelease Spago.purescriptRepo
    spawnThread "Check for new docs-search releases" $ Common.checkLatestRelease Spago.docsSearchRepo
    spawnThread "Check for new package-sets releases" $ Common.checkLatestRelease PackageSets.packageSetsRepo

    when enableSetsMetadataUpdate $
      spawnThread "Fetch new metadata for packages in package-sets" Metadata.fetcher

    -- we curl this Healthchecks.io link every hour, otherwise Fabrizio gets emails :)
    case healthchecksToken of
      Nothing -> pure ()
      Just token -> do
        heartbeatUrl <- Http.parseRequest $ "https://hc-ping.com/" <> token
        void $ Http.httpBS heartbeatUrl

  NewPureScriptRelease ->
    when enableVersionUpdates $
      spawnThread "Update purs version in Spago CI" Spago.updatePurescriptVersion
    -- TODO: update purescript-metadata repo on purs release

  NewPackageSetsRelease ->
    when enableVersionUpdates $
      spawnThread "Update package-sets version in Spago templates" Spago.updatePackageSets

  NewDocsSearchRelease ->
    when enableVersionUpdates $
      spawnThread "Update docs-search version in Spago" Spago.updateDocsSearch

  NewVerification cmd result ->
    when enablePackageSetsUpdate $
      spawnThread "Comment on new PRs in package-sets" $ PackageSets.commenter cmd result

  NewMetadata -> do
    when enablePackageSetsUpdate $
      spawnThread "Update packages in package-sets" PackageSets.updater
    when enableSetsMetadataUpdate $
      spawnThread "Push new metadata about packages in package-sets" Metadata.updater

  NewPackageSet -> pure ()

  NewBowerRefresh ->
    when enableRegistryBowerSync $
      spawnThread "Sync Bower packages to the Registry" Registry.syncFromBower


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
