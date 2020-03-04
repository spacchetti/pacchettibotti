{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module PacchettiBotti.DB.Schema 
  ( module PacchettiBotti.DB.Schema
  , module PacchettiBotti.DB.Types
  ) where

import           Spago.Prelude

import qualified Spago.Types                   as Spago

import           Database.Persist.TH
import           Data.Time (UTCTime)
import           Spago.GlobalCache              ( CommitHash(..)
                                                , Tag(..)
                                                )
import           PacchettiBotti.DB.Types
-- import qualified Data.SemVer as Semver


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Package json
  name       Spago.PackageName
  address    Address
  setVersion Tag Maybe

  Primary name
  UniqueName name
  deriving Show Eq Ord Data


Release json
  package Spago.PackageName
  tag     Tag
  commit  CommitHash
  banned  Bool

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


Fetch json
  type  FetchType
  time  UTCTime

  Primary type 
  UniqueFetch type
  deriving Show Eq Ord Data
|]
