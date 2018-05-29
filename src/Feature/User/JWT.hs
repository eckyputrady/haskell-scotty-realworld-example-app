module Feature.User.JWT where

import ClassyPrelude
import Platform.JWT
import Feature.User.Types
import Data.Has
import Jose.Jwt
import Jose.Jwa
import qualified Data.Aeson as Aeson
import Control.Monad.Except
import Data.Time.Clock.POSIX (getPOSIXTime)

resolveToken :: (JWT r m) => Token -> m (Either TokenError CurrentUser)
resolveToken token = runExceptT $ do
  jwks <- asks $ envJwks . getter
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
  jwks <- asks $ envJwks . getter
  expirationSecs <- asks $ envExpirationSecs . getter
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