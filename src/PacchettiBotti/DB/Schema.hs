{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module PacchettiBotti.DB.Schema where

import           Spago.Prelude

import           Data.Time (UTCTime)
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Release json
    org  Text
    repo Text
    tag  Text
    date UTCTime
    Primary org repo tag
    deriving Show Eq Ord Data
|]
