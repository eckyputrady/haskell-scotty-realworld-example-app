module Feature.Auth.JWT where

import ClassyPrelude
import Platform.JWT
import Feature.Auth.Types
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