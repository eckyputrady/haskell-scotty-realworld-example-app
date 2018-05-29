module Feature.User.HTTP
      ( routes
      , UserService(..)
      , TokenService(..)
      , optionalUser
      , requireUser
      ) where

import ClassyPrelude hiding (delete)

import Feature.User.Types
import Feature.Common.Util (orThrow)
import Feature.Common.HTTP
import Control.Monad.Except
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Types as DF
import Text.Digestive.Form ((.:))
import Text.Regex

class UserService m where
  login :: Auth -> m (Either UserError User)
  register :: Register -> m (Either UserError User)
  getUser :: CurrentUser -> m (Either UserError User)
  updateUser :: CurrentUser -> UpdateUser -> m (Either UserError User)
  getProfile :: Maybe CurrentUser -> Username -> m (Either UserError Profile)
  followUser :: CurrentUser -> Username -> m (Either UserError Profile)
  unfollowUser :: CurrentUser -> Username -> m (Either UserError Profile)

routes :: (TokenService m, UserService m, MonadIO m) => ScottyT LText m ()
routes = do

  -- users

  post "/api/users/login" $ do
    req <- parseJsonBody ("user" .: authForm)
    result <- stopIfError userErrorHandler $ login req
    json $ UserWrapper result

  post "/api/users" $ do
    req <- parseJsonBody ("user" .: registerForm)
    result <- stopIfError userErrorHandler $ register req
    json $ UserWrapper result

  get "/api/user" $ do
    curUser <- requireUser
    result <- stopIfError userErrorHandler $ getUser curUser
    json $ UserWrapper result

  put "/api/user" $ do
    curUser <- requireUser
    req <- parseJsonBody ("user" .: updateUserForm)
    result <- stopIfError userErrorHandler $ updateUser curUser req
    json $ UserWrapper result


  -- profiles

  get "/api/profiles/:username" $ do
    curUser <- optionalUser
    username <- param "username"
    result <- stopIfError userErrorHandler $ getProfile curUser username
    json $ ProfileWrapper result

  post "/api/profiles/:username/follow" $ do
    curUser <- requireUser
    username <- param "username"
    result <- stopIfError userErrorHandler $ followUser curUser username
    json $ ProfileWrapper result

  delete "/api/profiles/:username/follow" $ do
    curUser <- requireUser
    username <- param "username"
    result <- stopIfError userErrorHandler $ unfollowUser curUser username
    json $ ProfileWrapper result


-- * Errors

userErrorHandler :: (ScottyError e, Monad m) => UserError -> ActionT e m ()
userErrorHandler err = case err of
  UserErrorBadAuth _ -> do
    status status400
    json err
  UserErrorNotFound _ -> do
    status status404
    json err
  UserErrorNameTaken _ -> do
    status status400
    json err
  UserErrorEmailTaken _ -> do
    status status400
    json err


-- * Request deserialization & validation

minLength :: MonoFoldable a => Int -> a -> DF.Result Text a
minLength n str = if length str >= n then DF.Success str else DF.Error $ "Minimum length is " <> tshow n

matchesRegex :: v -> String -> Text -> DF.Result v Text
matchesRegex errMsg regexStr str =
  if isJust . matchRegex (mkRegexWithOpts regexStr True True) . unpack $ str
    then DF.Success str
    else DF.Error errMsg

emailValidation :: Text -> DF.Result [Text] Text
emailValidation = DF.conditions [matchesRegex "Not a valid email" "^[a-zA-Z0-9\\.\\+\\-]+@[a-zA-Z0-9]+\\.[a-zA-Z0-9]+$"]

usernameValidation :: Text -> DF.Result [Text] Text
usernameValidation = DF.conditions [minLength 3, matchesRegex "Should be alphanumeric" "^[a-zA-Z0-9]+$"]

passwordValidation :: Text -> DF.Result [Text] Text
passwordValidation = DF.conditions [minLength 5]

authForm :: (Monad m) => DF.Form [Text] m Auth
authForm = Auth <$> "email" .: DF.validate emailValidation (DF.text Nothing)
                <*> "password" .: DF.validate passwordValidation (DF.text Nothing)
                
registerForm :: (Monad m) => DF.Form [Text] m Register
registerForm = Register <$> "username" .: DF.validate usernameValidation (DF.text Nothing)
                        <*> "email" .: DF.validate emailValidation (DF.text Nothing)
                        <*> "password" .: DF.validate passwordValidation (DF.text Nothing)
                        
updateUserForm :: (Monad m) => DF.Form [Text] m UpdateUser
updateUserForm = UpdateUser <$> "email" .: DF.validateOptional emailValidation (DF.optionalText Nothing)
                            <*> "username" .: DF.validateOptional usernameValidation (DF.optionalText Nothing)
                            <*> "password" .: DF.validateOptional passwordValidation (DF.optionalText Nothing)
                            <*> "image" .: DF.optionalText Nothing
                            <*> "bio" .: DF.optionalText Nothing

-- Getting current user

class Monad m => TokenService m where
  resolveToken :: Token -> m (Either TokenError CurrentUser)

getCurrentUser :: (TokenService m) => ActionT LText m (Either TokenError CurrentUser)
getCurrentUser = do
  mayHeaderVal <- header "Authorization"
  runExceptT $ do
    headerVal <- ExceptT $ pure mayHeaderVal `orThrow` TokenErrorNotFound
    let token = toStrict $ drop 6 headerVal
    ExceptT $ lift $ resolveToken token

optionalUser :: (TokenService m) => ActionT LText m (Maybe CurrentUser)
optionalUser =
  either (const Nothing) Just <$> getCurrentUser

requireUser :: (TokenService m) => ActionT LText m CurrentUser
requireUser = do
  result <- getCurrentUser
  stopIfError tokenErrorHandler (pure result)
  where 
    tokenErrorHandler e = do
      status status401
      json e
