{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module PacchettiBotti.DB.Schema where

import           Spago.Prelude

import qualified Spago.Types                   as Spago

import           Database.Persist.TH
import           Spago.GlobalCache              ( CommitHash(..)
                                                , Tag(..)
                                                )
import           PacchettiBotti.Types
import           PacchettiBotti.DB.Orphans      ( )
-- import qualified Data.SemVer as Semver


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Release json
  package Spago.PackageName
  tag     Tag
  commit  CommitHash

  Primary package tag
  Foreign Package fk_package package
  UniqueRelease package tag
  deriving Show Eq Ord Data


Commit json
  package Spago.PackageName
  commit  CommitHash

  Primary package commit
  Foreign Package fk_package package
  UniqueCommit package commit
  deriving Show Eq Ord Data


Package json
  name       Spago.PackageName
  address    Address
  setVersion Tag Maybe

  Primary name
  UniqueName name
  deriving Show Eq Ord Data

|]
