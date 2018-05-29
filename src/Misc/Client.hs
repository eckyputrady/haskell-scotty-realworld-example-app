module Misc.Client where

import ClassyPrelude
import Feature.Common.Types
import Feature.Auth.Types
import Feature.User.Types
import Feature.Article.Types
import Feature.Comment.Types hiding (Slug)
import Network.Wreq hiding (Auth)
import Control.Monad.Except
import Control.Lens hiding ((.=))
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import Network.HTTP.Types.Status
import qualified Network.HTTP.Client as HC
import Data.Has


-- * Types

data Err a
  = ErrMalformedJSON Text
  | ErrInvalidInput InputViolations
  | ErrInternalServerError ByteString
  | ErrUnauthorized TokenError
  | ErrApp a
  | ErrUnknown Text
  deriving (Eq, Show)
  
type RW r m = (MonadIO m, Has RWBaseUrl r, MonadReader r m, MonadUnliftIO m)

newtype RWBaseUrl = RWBaseUrl String

buildUrl :: (RW r m) => String -> ExceptT e m String
buildUrl path = do
  (RWBaseUrl baseUrl) <- asks getter
  return $ baseUrl <> path


-- * User

login :: (RW r m) => Auth -> m (Either (Err UserError) User)
login arg = runExceptT $ do
  url <- buildUrl "/users/login"
  let body = Aeson.toJSON $ UserWrapper arg
  userWrapperUser <$> exec (post url body)

register :: (RW r m) => Register -> m (Either (Err UserError) User)
register arg = runExceptT $ do
  url <- buildUrl "/users"
  let body = Aeson.toJSON $ UserWrapper arg
  userWrapperUser <$> exec (post url body)
 
getUser :: (RW r m) => Token -> m (Either (Err UserError) User)
getUser token = runExceptT $ do
  url <- buildUrl "/user"
  let opts = defaults & authHeader token
  userWrapperUser <$> exec (getWith opts url)


updateUser :: (RW r m) => Token -> UpdateUser -> m (Either (Err UserError) User)
updateUser token arg = runExceptT $ do
  url <- buildUrl "/user"
  let body = Aeson.toJSON $ UserWrapper arg
  let opts = defaults & authHeader token
  userWrapperUser <$> exec (putWith opts url body)



-- * Profiles

getProfile :: (RW r m) => Maybe Token -> Username -> m (Either (Err UserError) Profile)
getProfile mayToken username = runExceptT $ do
  url <- buildUrl $ "/profiles/" <> unpack username
  let opts = defaults & mayAuthHeader mayToken
  profileWrapperProfile <$> exec (getWith opts url)

followUser :: (RW r m) => Token -> Username -> m (Either (Err UserError) Profile)
followUser token username = runExceptT $ do
  url <- buildUrl $ "/profiles/" <> unpack username <> "/follow"
  let body = Aeson.toJSON $ asText ""
  let opts = defaults & authHeader token
  profileWrapperProfile <$> exec (postWith opts url body)

unfollowUser :: (RW r m) => Token -> Username -> m (Either (Err UserError) Profile)
unfollowUser token username = runExceptT $ do
  url <- buildUrl $ "/profiles/" <> unpack username <> "/follow"
  let opts = defaults & authHeader token
  profileWrapperProfile <$> exec (deleteWith opts url)



-- * Articles

getArticles :: (RW r m) => Maybe Token -> ArticleFilter -> Pagination -> m (Either (Err ArticleError) [Article])
getArticles mayToken filterParam pagination = runExceptT $ do
  url <- buildUrl "/articles"
  let opts = defaults & mayAuthHeader mayToken & paginate pagination & articleFilter filterParam
  articlesWrapperArticles <$> exec (getWith opts url)

getFeed :: (RW r m) => Token -> Pagination -> m (Either (Err ArticleError) [Article])
getFeed token pagination = runExceptT $ do
  url <- buildUrl "/articles/feed"
  let opts = defaults & authHeader token & paginate pagination
  articlesWrapperArticles <$> exec (getWith opts url)

getArticle :: (RW r m) => Maybe Token -> Slug -> m (Either (Err ArticleError) Article)
getArticle mayToken slug = runExceptT $ do
  url <- buildUrl $ "/articles/" <> unpack slug
  let opts = defaults & mayAuthHeader mayToken
  articleWrapperArticle <$> exec (getWith opts url)

createArticle :: (RW r m) => Token -> CreateArticle -> m (Either (Err ArticleError) Article)
createArticle token arg = runExceptT $ do
  url <- buildUrl "/articles"
  let opts = defaults & authHeader token
  let body = Aeson.toJSON $ ArticleWrapper arg
  articleWrapperArticle <$> exec (postWith opts url body)

updateArticle :: (RW r m) => Token -> Slug -> UpdateArticle -> m (Either (Err ArticleError) Article)
updateArticle token slug arg = runExceptT $ do
  url <- buildUrl $ "/articles/" <> unpack slug
  let opts = defaults & authHeader token
  let body = Aeson.toJSON $ ArticleWrapper arg
  articleWrapperArticle <$> exec (putWith opts url body)

deleteArticle :: (RW r m) => Token -> Slug -> m (Either (Err ArticleError) ())
deleteArticle token slug = runExceptT $ do
  url <- buildUrl $ "/articles/" <> unpack slug
  let opts = defaults & authHeader token
  const () . asText <$> exec (deleteWith opts url)

paginate :: Pagination -> Network.Wreq.Options -> Network.Wreq.Options
paginate (Pagination limit offset) =
  (param "limit" .~ [tshow limit]) . (param "offset" .~ [tshow offset])

articleFilter :: ArticleFilter -> Network.Wreq.Options -> Network.Wreq.Options
articleFilter (ArticleFilter mayTag mayAuthor mayFavoritedBy) = 
  maybe id (\x -> param "tag" .~ [x]) mayTag
  . maybe id (\x -> param "author" .~ [x]) mayAuthor
  . maybe id (\x -> param "favorited" .~ [x]) mayFavoritedBy



-- * Favorites

favoriteArticle :: (RW r m) => Token -> Slug -> m (Either (Err ArticleError) Article)
favoriteArticle token slug = runExceptT $ do
  url <- buildUrl $ "/articles/" <> unpack slug <> "/favorite"
  let opts = defaults & authHeader token
  let body = Aeson.toJSON $ asText ""
  articleWrapperArticle <$> exec (postWith opts url body)

unfavoriteArticle :: (RW r m) => Token -> Slug -> m (Either (Err ArticleError) Article)
unfavoriteArticle token slug = runExceptT $ do
  url <- buildUrl $ "/articles/" <> unpack slug <> "/favorite"
  let opts = defaults & authHeader token
  articleWrapperArticle <$> exec (deleteWith opts url)



-- * Comments

addComment :: (RW r m) => Token -> Slug -> Text -> m (Either (Err CommentError) Comment)
addComment token slug comment = runExceptT $ do
  url <- buildUrl $ "/articles/" <> unpack slug <> "/comments"
  let opts = defaults & authHeader token
  let body = Aeson.object [ "comment" .= Aeson.object [ "body" .= comment ] ]
  commentWrapperComment <$> exec (postWith opts url body)

delComment :: (RW r m) => Token -> Slug -> CommentId -> m (Either (Err CommentError) ())
delComment token slug cId = runExceptT $ do
  url <- buildUrl $ "/articles/" <> unpack slug <> "/comments/" <> show cId
  let opts = defaults & authHeader token
  const () . asText <$> exec (deleteWith opts url)

getComments :: (RW r m) => Maybe Token -> Slug -> m (Either (Err CommentError) [Comment])
getComments mayToken slug = runExceptT $ do
  url <- buildUrl $ "/articles/" <> unpack slug <> "/comments"
  let opts = defaults & mayAuthHeader mayToken
  commentsWrapperComments <$> exec (getWith opts url)



-- * Tags

getTags :: (RW r m) => m (Either (Err Text) (Set Tag))
getTags = runExceptT $ do
  url <- buildUrl "/tags"
  tagsWrapperTags <$> exec (get url)
  
  

-- * Healh

health :: (RW r m) => m (Either (Err Text) Bool)
health = runExceptT $ do
  url <- buildUrl "/health"
  exec $ get url



-- * Utils

exec :: (RW r m, Aeson.FromJSON a, Aeson.FromJSON e) => IO (Response LByteString) -> ExceptT (Err e) m a
exec req = ExceptT $ do
  r <- liftIO (Right <$> (req >>= asJSON))
    `catch` handleHttpException
    `catch` handleJSONError
    `catch` handleOtherException
  return $ case r of
    Left err -> Left err
    Right r' -> Right $ r' ^. responseBody
  where
    handleJSONError (JSONError err) = return . Left $ ErrMalformedJSON $ tshow err
    handleHttpException (HC.HttpExceptionRequest _ (HC.StatusCodeException res body)) =
      let status = HC.responseStatus res
      in  if status == status500 then
            return . Left $ ErrInternalServerError body
          else if status == status401 then
            parseJSONError body ErrUnauthorized
          else if status == status422 then
            parseJSONError body (ErrInvalidInput . errorsWrapperErrors)
          else
            parseJSONError body ErrApp
    handleHttpException err = return . Left $ ErrUnknown $ tshow err
    handleOtherException (e :: SomeException) = return . Left $ ErrUnknown $ tshow e
    -- parseJSONError :: LByteString -> (Aeson.Value -> e) -> Either e a
    parseJSONError src f = case Aeson.eitherDecode $ fromStrict src of
      Left parseErr -> return . Left $ ErrMalformedJSON $ tshow parseErr
      Right parseResult -> return . Left $ f parseResult

authHeader :: Token -> Network.Wreq.Options -> Network.Wreq.Options
authHeader token = header "Authorization" .~ ["Token " <> fromString (unpack token)]

mayAuthHeader :: Maybe Token -> Network.Wreq.Options -> Network.Wreq.Options
mayAuthHeader = maybe id authHeader