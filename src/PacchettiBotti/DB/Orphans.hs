{-# OPTIONS_GHC -Wno-orphans #-}
module PacchettiBotti.DB.Orphans where

import Spago.Prelude

-- import Data.SemVer (SemVer(..), PrereleaseTags(..), PrereleaseTag(..))
import           Spago.Types
import           Spago.GlobalCache              ( CommitHash(..)
                                                , Tag(..)
                                                )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Web.HttpApiData
import           Web.PathPieces                 ( PathPiece )
import           Database.Persist.Sqlite        ( PersistField
                                                , PersistFieldSql
                                                , sqlType
                                                , SqlType(..)
                                                , fromPersistValue
                                                , toPersistValue
                                                )


instance PersistField Tag where
  toPersistValue (Tag t) = toPersistValue t
  fromPersistValue t = Tag <$> fromPersistValue t

instance PersistFieldSql Tag where
  sqlType _ = SqlString

instance PersistField CommitHash where
  toPersistValue (CommitHash h) = toPersistValue h
  fromPersistValue h = CommitHash <$> fromPersistValue h

instance PersistFieldSql CommitHash where
  sqlType _ = SqlString

instance PersistField PackageName where
  toPersistValue  (PackageName n) = toPersistValue n
  fromPersistValue n = PackageName <$> fromPersistValue n

instance PersistFieldSql PackageName where
  sqlType _ = SqlString

instance PathPiece PackageName

instance FromHttpApiData PackageName where
  parseHeader h     = PackageName <$> parseHeader h
  parseQueryParam p = PackageName <$> parseQueryParam p

instance ToHttpApiData PackageName where
  toHeader     (PackageName t) = encodeUtf8 t
  toQueryParam (PackageName t) = t

{-
instance PersistField SemVer where 
  toPersistValue SemVer{..} = toPersistValue ([svMajor, svMinor, svPatch], (svTags, svBuildMetadata))

instance PersistFieldSql SemVer where
  sqlType _ = SqlString

instance PersistField PrereleaseTags where
  toPersistValue (PrereleaseTags ts) = toPersistValue ts

instance PersistField PrereleaseTag
-}