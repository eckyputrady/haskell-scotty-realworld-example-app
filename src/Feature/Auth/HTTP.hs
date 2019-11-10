module Feature.Auth.HTTP where

import ClassyPrelude
import Feature.Auth.Types
import Feature.Common.Util (orThrow)
import Feature.Common.HTTP
import Control.Monad.Except
import Web.Scotty.Trans
import Network.HTTP.Types.Status

class Monad m => Service m where
  resolveToken :: Token -> m (Either TokenError CurrentUser)

getCurrentUser :: (Service m) => ActionT LText m (Either TokenError CurrentUser)
getCurrentUser = do
  mayHeaderVal <- header "Authorization"
  runExceptT $ do
    headerVal <- ExceptT $ pure mayHeaderVal `orThrow` TokenErrorNotFound
    let token = toStrict $ drop 7 headerVal
    ExceptT $ lift $ resolveToken token

optionalUser :: (Service m) => ActionT LText m (Maybe CurrentUser)
optionalUser =
  either (const Nothing) Just <$> getCurrentUser

requireUser :: (Service m) => ActionT LText m CurrentUser
requireUser = do
  result <- getCurrentUser
  stopIfError tokenErrorHandler (pure result)
  where 
    tokenErrorHandler e = do
      status status401
      json e
