module PacchettiBotti.Types where

import           Spago.Prelude

import qualified GitHub
import qualified Data.Text                     as Text
import           Database.Persist.Sqlite        ( PersistField
                                                , PersistFieldSql
                                                , sqlType
                                                , SqlType(..)
                                                , fromPersistValue
                                                , toPersistValue
                                                )
import           Text.Read                      ( Read(..) )


data Address = Address
  { owner :: GitHub.Name GitHub.Owner
  , repo  :: GitHub.Name GitHub.Repo
  } deriving (Eq, Ord, Data, Generic)

instance Show Address where
  show (Address owner repo) = show (GitHub.untagName owner, GitHub.untagName repo)

instance Read Address where
  readsPrec _ input =
    case readMaybe input of
      Just (owner, repo) -> [(Address (GitHub.mkName Proxy owner) (GitHub.mkName Proxy repo), "")]
      Nothing -> []

instance FromJSON Address
instance ToJSON Address

instance PersistField Address where
  toPersistValue (Address owner repo) = toPersistValue (GitHub.untagName owner,GitHub.untagName repo)
  fromPersistValue a
    = uncurry Address . bimap (GitHub.mkName Proxy) (GitHub.mkName Proxy)
    <$> fromPersistValue a

instance PersistFieldSql Address where
  sqlType _ = SqlString


parseAddress :: Text -> Either [Text] Address
parseAddress repoUrl = case parts of
  (owner:repo:_rest) -> Right $ Address (GitHub.mkName Proxy owner) (GitHub.mkName Proxy repo)
  ps -> Left ps
  where
    parts = Text.split (=='/') $ Text.replace "https://github.com/" ""
      (if Text.isSuffixOf ".git" repoUrl
       then Text.dropEnd 4 repoUrl
       else repoUrl)
