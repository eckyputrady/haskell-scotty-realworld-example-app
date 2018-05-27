module Adapter.JWT where
  
import ClassyPrelude
import Data.Has
import Jose.Jwt
import Jose.Jwk
import Jose.Jwa
import Crypto.Random.Types (MonadRandom)
import System.Environment
import qualified Data.Aeson as Aeson
import Core.Types
import Control.Monad.Except
import Data.Time.Clock.POSIX (getPOSIXTime)

newtype JWTExpirationSecs = JWTExpirationSecs Integer

type JWT r m = (MonadReader r m, Has [Jwk] r, Has JWTExpirationSecs r, MonadRandom m, MonadIO m)

-- * Resource acquisitions

acquireJwks :: IO [Jwk]
acquireJwks = do
  envUrl <- lookupEnv "JWK_PATH"
  let jwkPath = fromMaybe "secrets/jwk.sig" envUrl
  fileContent <- readFile jwkPath
  let parsed = Aeson.eitherDecodeStrict fileContent
  return $ either (\e -> error $ "invalid JWK file: " <> e) pure parsed

acquireJWTExpirationSecs :: IO JWTExpirationSecs
acquireJWTExpirationSecs = do
  param <- lookupEnv "JWT_EXPIRATION_SECS"
  let expirationSecs = fromMaybe (2 * 60 * 60) $ param >>= readMay
  return $ JWTExpirationSecs expirationSecs

-- * Encode & Decode

resolveToken :: (JWT r m) => Token -> m (Either TokenError CurrentUser)
resolveToken token = runExceptT $ do
  jwks <- asks getter
  eitherJwt <- lift $ decode jwks (Just $ JwsEncoding RS256) (encodeUtf8 token)
  curTime <- liftIO getPOSIXTime
  userId <- either throwError return $ do
    Jws (_, claimsRaw) <- first (TokenErrorMalformed . show) eitherJwt
    jwtClaims <- first TokenErrorMalformed $ Aeson.eitherDecode $ fromStrict claimsRaw
    let (IntDate expiredAt) = fromMaybe (IntDate curTime) $ jwtExp jwtClaims
    when (expiredAt < curTime) $ Left TokenErrorExpired
    maybe (Left TokenErrorUserIdNotFound) Right $ jwtSub jwtClaims >>= readMay
  return (token, userId)

generateToken :: (JWT r m) => UserId -> m Token
generateToken userId = do
  jwks <- asks getter
  JWTExpirationSecs expirationSecs <- asks getter
  curTime <- liftIO getPOSIXTime
  let claim = JwtClaims { jwtIss = Nothing
                        , jwtSub = Just $ tshow userId
                        , jwtAud = Nothing
                        , jwtExp = Just $ IntDate $ curTime + fromInteger expirationSecs
                        , jwtNbf = Nothing
                        , jwtIat = Nothing
                        , jwtJti = Nothing
                        }
  (Jwt jwtEncoded) <- either (\e -> error $ "Failed to encode JWT: " <> show e) id <$> encode jwks (JwsEncoding RS256) (Claims . toStrict . Aeson.encode $ claim)
  return . decodeUtf8 $ jwtEncoded