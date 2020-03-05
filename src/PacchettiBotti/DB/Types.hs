{-# OPTIONS_GHC -Wno-orphans #-}
module PacchettiBotti.DB.Types
  ( module PacchettiBotti.DB.Types
  , Address(..)
  , parseAddress
  ) where

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
import PacchettiBotti.DB.Address


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

data FetchType
  = ReleasesFetch Address
  | CommitsFetch Address
  deriving (Show, Read, Eq, Ord, Data, Generic)

instance FromJSON FetchType
instance ToJSON FetchType

instance PersistField FetchType where
  toPersistValue v = toPersistValue $ case v of
    ReleasesFetch address -> ("releases" :: Text, address)
    CommitsFetch address -> ("commits", address)
  fromPersistValue v = case fromPersistValue v of
    Right ("releases" :: Text, address) -> Right $ ReleasesFetch address
    Right ("commits", address) -> Right $ CommitsFetch address
    Right other -> Left $ "Got other stuff when decoding FetchType: " <> tshow other
    Left err -> Left err

instance PersistFieldSql FetchType where
  sqlType _ = SqlString

instance PathPiece FetchType

instance FromHttpApiData FetchType

instance ToHttpApiData FetchType