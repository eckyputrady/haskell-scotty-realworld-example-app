module Platform.JWT where
  
import ClassyPrelude
import Data.Has
import Jose.Jwk
import Crypto.Random.Types (MonadRandom)
import System.Environment
import qualified Data.Aeson as Aeson

data Env = Env
  { envExpirationSecs :: Integer
  , envJwks :: [Jwk]
  }

type JWT r m = (MonadReader r m, Has Env r, MonadRandom m, MonadIO m)

-- * Resource acquisitions

init :: IO Env
init = Env <$> acquireJWTExpirationSecs <*> acquireJwks

acquireJwks :: IO [Jwk]
acquireJwks = do
  envUrl <- lookupEnv "JWK_PATH"
  let jwkPath = fromMaybe "secrets/jwk.sig" envUrl
  fileContent <- readFile jwkPath
  let parsed = Aeson.eitherDecodeStrict fileContent
  return $ either (\e -> error $ "invalid JWK file: " <> e) pure parsed

acquireJWTExpirationSecs :: IO Integer
acquireJWTExpirationSecs = do
  param <- lookupEnv "JWT_EXPIRATION_SECS"
  let expirationSecs = fromMaybe (2 * 60 * 60) $ param >>= readMay
  return expirationSecs
