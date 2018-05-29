module Feature.User.JWT where

import ClassyPrelude
import Platform.JWT
import Feature.Auth.Types
import Data.Has
import Jose.Jwt
import Jose.Jwa
import qualified Data.Aeson as Aeson
import Data.Time.Clock.POSIX (getPOSIXTime)

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