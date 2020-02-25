module PacchettiBotti.Env where

import           Spago.Prelude           hiding ( Env )

import qualified GitHub
import qualified PacchettiBotti.DB             as DB

-- TODO: rename this to envLogFunc
data Env = Env
  { envLogFun :: !LogFunc
  , envGithubToken :: !GitHub.Auth
  , envDbHandle :: !DB.Handle
  }


class HasGitHubToken env where
  githubTokenL :: Lens' env GitHub.Auth

class HasDbHandle env where
  dbHandleL :: Lens' env DB.Handle

class (HasLogFunc env, HasGitHubToken env, HasDbHandle env) => HasEnv env where
  envL :: Lens' env Env

instance HasLogFunc Env where
  logFuncL = lens envLogFun (\x y -> x { envLogFun = y })

instance HasGitHubToken Env where
  githubTokenL = lens envGithubToken (\x y -> x { envGithubToken = y })

instance HasDbHandle Env where
  dbHandleL = lens envDbHandle (\x y -> x { envDbHandle = y })

instance HasEnv Env where
  envL = id

type HasGitHub env = (HasLogFunc env, HasGitHubToken env)
