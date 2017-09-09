module Web
       ( main
       ) where

import ClassyPrelude hiding (delete)

import Struct
import RealWorld
import Control.Monad.Except
import Crypto.Random.Types (MonadRandom, getRandomBytes)
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import Network.Wai (Response)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.View as DF
import qualified Text.Digestive.Types as DF
import qualified Text.Digestive.Aeson as DF
import Text.Digestive.Form ((.:))
import Text.Regex

import PG
import JWT
import System.Environment

type App r m = (PG r m, JWT r m)

main :: (App r m) => (m Response -> IO Response) -> IO ()
main runner = do
  port <- acquirePort
  mayTLSSetting <- acquireTLSSetting
  case mayTLSSetting of
    Nothing ->
      scottyT port runner routes
    Just tlsSetting -> do
      app <- scottyAppT runner routes
      runTLS tlsSetting (setPort port defaultSettings) app
  where
    acquirePort = do
      port <- fromMaybe "" <$> lookupEnv "PORT"
      return . fromMaybe 3000 $ readMay port
    acquireTLSSetting = do
      env <- (>>= readMay) <$> lookupEnv "ENABLE_HTTPS"
      let enableHttps = fromMaybe True env
      return $ if enableHttps
        then Just $ tlsSettings "secrets/tls/certificate.pem" "secrets/tls/key.pem"
        else Nothing



-- * Routing

routes :: (App r m) => ScottyT AppError m ()
routes = do
  -- err 
  
  defaultHandler errHandler


  -- users

  post "/api/users/login" $ do
    req <- parseJsonBody ("user" .: authForm)
    result <- raiseIfError AppErrorUser $ login req
    json $ UserWrapper result

  post "/api/users" $ do
    req <- parseJsonBody ("user" .: registerForm)
    result <- raiseIfError AppErrorUser $ register req
    json $ UserWrapper result

  get "/api/user" $ do
    curUser <- requireUser
    result <- raiseIfError AppErrorUser $ getUser curUser
    json $ UserWrapper result

  put "/api/user" $ do
    curUser <- requireUser
    req <- parseJsonBody ("user" .: updateUserForm)
    result <- raiseIfError AppErrorUser $ updateUser curUser req
    json $ UserWrapper result


  -- profiles

  get "/api/profiles/:username" $ do
    curUser <- optionalUser
    username <- param "username"
    result <- raiseIfError AppErrorUser $ getProfile curUser username
    json $ ProfileWrapper result

  post "/api/profiles/:username/follow" $ do
    curUser <- requireUser
    username <- param "username"
    result <- raiseIfError AppErrorUser $ followUser curUser username
    json $ ProfileWrapper result

  delete "/api/profiles/:username/follow" $ do
    curUser <- requireUser
    username <- param "username"
    result <- raiseIfError AppErrorUser $ unfollowUser curUser username
    json $ ProfileWrapper result


  -- articles

  get "/api/articles" $ do
    curUser <- optionalUser
    pagination <- parsePagination
    articleFilter <- parseArticleFilter
    result <- raiseIfError AppErrorArticle $ getArticles curUser articleFilter pagination
    json $ ArticlesWrapper result (length result)

  get "/api/articles/feed" $ do
    curUser <- requireUser
    pagination <- parsePagination
    result <- raiseIfError AppErrorArticle $ getFeed curUser pagination
    json $ ArticlesWrapper result (length result)

  get "/api/articles/:slug" $ do
    curUser <- optionalUser
    slug <- param "slug"
    result <- raiseIfError AppErrorArticle $ getArticle curUser slug
    json $ ArticleWrapper result

  post "/api/articles" $ do
    curUser <- requireUser
    req <- parseJsonBody ("article" .: createArticleForm)
    result <- raiseIfError AppErrorArticle $ createArticle curUser req
    json $ ArticleWrapper result

  put "/api/articles/:slug" $ do
    curUser <- requireUser
    slug <- param "slug"
    req <- parseJsonBody ("article" .: updateArticleForm)
    result <- raiseIfError AppErrorArticle $ updateArticle curUser slug req
    json $ ArticleWrapper result

  delete "/api/articles/:slug" $ do
    curUser <- requireUser
    slug <- param "slug"
    raiseIfError AppErrorArticle $ deleteArticle curUser slug
    json $ asText ""


  -- favorites

  post "/api/articles/:slug/favorite" $ do
    curUser <- requireUser
    slug <- param "slug"
    result <- raiseIfError AppErrorArticle $ favoriteArticle curUser slug
    json $ ArticleWrapper result

  delete "/api/articles/:slug/favorite" $ do
    curUser <- requireUser
    slug <- param "slug"
    result <- raiseIfError AppErrorArticle $ unfavoriteArticle curUser slug
    json $ ArticleWrapper result


  -- comments

  post "/api/articles/:slug/comments" $ do
    curUser <- requireUser
    slug <- param "slug"
    req <- parseJsonBody ("comment" .: "body" .: DF.text Nothing)
    result <- raiseIfError AppErrorComment $ addComment curUser slug req
    json $ CommentWrapper result

  delete "/api/articles/:slug/comments/:id" $ do
    curUser <- requireUser
    slug <- param "slug"
    cId <- param "id"
    raiseIfError AppErrorComment $ delComment curUser slug cId
    json $ asText ""
  
  get "/api/articles/:slug/comments" $ do
    curUser <- optionalUser
    slug <- param "slug"
    result <- raiseIfError AppErrorComment $ getComments curUser slug
    json $ CommentsWrapper result
  

  -- tags

  get "/api/tags" $ do
    result <- raiseIfError AppErrorUnknown getTags
    json $ TagsWrapper result

  
  -- health

  get "/api/health" $
    json True


-- * Utils

orThrow :: MonadError e m => Maybe a -> e -> m a
orThrow m e = case m of
  Nothing -> throwError e
  Just a -> return a
  
parsePagination :: (ScottyError e, Monad m) => ActionT e m Pagination
parsePagination = do
  limit <- param "limit" `rescue` const (return 20)
  offset <- param "offset" `rescue` const (return 0)
  return $ Pagination limit offset

mayParam :: (ScottyError e, Monad m) => LText -> ActionT e m (Maybe Text)
mayParam name = (Just <$> param name) `rescue` const (return Nothing)
  
parseArticleFilter :: (ScottyError e, Monad m) => ActionT e m ArticleFilter
parseArticleFilter = ArticleFilter <$> mayParam "tag" <*> mayParam "author" <*> mayParam "favorited"

parseJsonBody :: (MonadIO m) => DF.Form [Text] m a -> ActionT AppError m a
parseJsonBody form = do
  val <- jsonData `rescue` const (raise AppErrorInputMalformedJson)
  (v, result) <- lift $ DF.digestJSON form val
  case result of
    Nothing -> raise $ AppErrorInput v
    Just x -> return x

requireUser :: (App r m) => ActionT AppError m CurrentUser
requireUser = do
  mayHeaderVal <- header "Authorization"
  raiseIfError AppErrorToken $ do
    headerVal <- mayHeaderVal `orThrow` TokenErrorNotFound
    let token = toStrict $ drop 6 headerVal
    resolveToken token

optionalUser :: (App r m) => ActionT AppError m (Maybe CurrentUser)
optionalUser = (Just <$> requireUser) `rescue` const (return Nothing)

raiseIfError :: (Monad m, ScottyError e') => (e -> e') -> RWActionT e m a -> ActionT e' m a
raiseIfError f action = do
  result <- lift . runExceptT . unRWActionT $ action
  case result of
    Left e -> raise $ f e
    Right a -> return a

-- this newtype is created so that we can create a non-orphan instance of MonadRandom
newtype RWActionT e m a = RWActionT
  { unRWActionT :: ExceptT e m a
  } deriving  ( Applicative, Functor, Monad, MonadTrans
              , MonadError e, MonadReader r, MonadIO, MonadThrow, MonadCatch)

instance (MonadRandom m) => MonadRandom (RWActionT e m) where
  getRandomBytes = lift . getRandomBytes


-- * Errors

data AppError
  = AppErrorToken TokenError
  | AppErrorUser UserError
  | AppErrorArticle ArticleError
  | AppErrorComment CommentError
  | AppErrorInput (DF.View [Text])
  | AppErrorInputMalformedJson
  | AppErrorUnknown String
  deriving (Show)

errHandler :: Monad m => AppError -> ActionT AppError m ()
errHandler e = case e of
  AppErrorInput v -> do
    let errs = mapFromList $ map (first (intercalate "." . drop 1)) $ DF.viewErrors v :: InputViolations
    status status422
    json $ ErrorsWrapper errs
  AppErrorToken v -> do
    status status401
    json v
  AppErrorUser err -> case err of
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
  AppErrorArticle err -> case err of
    ArticleErrorNotFound _ -> do
      status status404
      json err
    ArticleErrorNotAllowed _ -> do
      status status403
      json err
  AppErrorComment err -> case err of
    CommentErrorNotFound _ -> do
      status status404
      json err
    CommentErrorSlugNotFound _ -> do
      status status404
      json err
    CommentErrorNotAllowed _ -> do
      status status403
      json err
  AppErrorInputMalformedJson -> do
    status status422
    json $ ErrorsWrapper $ asText "Malformed JSON payload"
  AppErrorUnknown str -> do
    status status500
    json str

instance ScottyError AppError where
  showError = fromString . show
  stringError = AppErrorUnknown



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
                            
createArticleForm :: (Monad m) => DF.Form [Text] m CreateArticle
createArticleForm = CreateArticle <$> "title" .: DF.text Nothing
                                  <*> "description" .: DF.text Nothing
                                  <*> "body" .: DF.text Nothing
                                  <*> "tagList" .: DF.listOf (const $ DF.text Nothing) Nothing
                                  
updateArticleForm :: (Monad m) => DF.Form [Text] m UpdateArticle
updateArticleForm = UpdateArticle <$> "title" .: DF.optionalText Nothing
                                  <*> "description" .: DF.optionalText Nothing
                                  <*> "body" .: DF.optionalText Nothing