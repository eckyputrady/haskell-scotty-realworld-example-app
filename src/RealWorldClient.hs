module RealWorldClient where

import ClassyPrelude
import Struct
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
  
type RW e r m = (MonadError (Err e) m, MonadIO m, MonadCatch m, Has RWBaseUrl r, MonadReader r m)

newtype RWBaseUrl = RWBaseUrl String

buildUrl :: (RW e r m) => String -> m String
buildUrl path = do
  (RWBaseUrl baseUrl) <- asks getter
  return $ baseUrl <> path


-- * User

login :: (RW UserError r m) => Auth -> m User
login arg = do
  url <- buildUrl "/users/login"
  let body = Aeson.toJSON $ UserWrapper arg
  userWrapperUser <$> exec (post url body)

register :: (RW UserError r m) => Register -> m User
register arg = do
  url <- buildUrl "/users"
  let body = Aeson.toJSON $ UserWrapper arg
  userWrapperUser <$> exec (post url body)
 
getUser :: (RW UserError r m) => Token -> m User
getUser token = do
  url <- buildUrl "/user"
  let opts = defaults & authHeader token
  userWrapperUser <$> exec (getWith opts url)


updateUser :: (RW UserError r m) => Token -> UpdateUser -> m User
updateUser token arg = do
  url <- buildUrl "/user"
  let body = Aeson.toJSON $ UserWrapper arg
  let opts = defaults & authHeader token
  userWrapperUser <$> exec (putWith opts url body)



-- * Profiles

getProfile :: (RW UserError r m) => Maybe Token -> Username -> m Profile
getProfile mayToken username = do
  url <- buildUrl $ "/profiles/" <> unpack username
  let opts = defaults & mayAuthHeader mayToken
  profileWrapperProfile <$> exec (getWith opts url)

followUser :: (RW UserError r m) => Token -> Username -> m Profile
followUser token username = do
  url <- buildUrl $ "/profiles/" <> unpack username <> "/follow"
  let body = Aeson.toJSON $ asText ""
  let opts = defaults & authHeader token
  profileWrapperProfile <$> exec (postWith opts url body)

unfollowUser :: (RW UserError r m) => Token -> Username -> m Profile
unfollowUser token username = do
  url <- buildUrl $ "/profiles/" <> unpack username <> "/follow"
  let opts = defaults & authHeader token
  profileWrapperProfile <$> exec (deleteWith opts url)



-- * Articles

getArticles :: (RW ArticleError r m) => Maybe Token -> ArticleFilter -> Pagination -> m [Article]
getArticles mayToken filterParam pagination = do
  url <- buildUrl "/articles"
  let opts = defaults & mayAuthHeader mayToken & paginate pagination & articleFilter filterParam
  articlesWrapperArticles <$> exec (getWith opts url)

getFeed :: (RW ArticleError r m) => Token -> Pagination -> m [Article]
getFeed token pagination = do
  url <- buildUrl "/articles/feed"
  let opts = defaults & authHeader token & paginate pagination
  articlesWrapperArticles <$> exec (getWith opts url)

getArticle :: (RW ArticleError r m) => Maybe Token -> Slug -> m Article
getArticle mayToken slug = do
  url <- buildUrl $ "/articles/" <> unpack slug
  let opts = defaults & mayAuthHeader mayToken
  articleWrapperArticle <$> exec (getWith opts url)

createArticle :: (RW ArticleError r m) => Token -> CreateArticle -> m Article
createArticle token arg = do
  url <- buildUrl "/articles"
  let opts = defaults & authHeader token
  let body = Aeson.toJSON $ ArticleWrapper arg
  articleWrapperArticle <$> exec (postWith opts url body)

updateArticle :: (RW ArticleError r m) => Token -> Slug -> UpdateArticle -> m Article
updateArticle token slug arg = do
  url <- buildUrl $ "/articles/" <> unpack slug
  let opts = defaults & authHeader token
  let body = Aeson.toJSON $ ArticleWrapper arg
  articleWrapperArticle <$> exec (putWith opts url body)

deleteArticle :: (RW ArticleError r m) => Token -> Slug -> m ()
deleteArticle token slug = do
  url <- buildUrl $ "/articles/" <> unpack slug
  let opts = defaults & authHeader token
  (const () . asText) <$> exec (deleteWith opts url)

paginate :: Pagination -> Network.Wreq.Options -> Network.Wreq.Options
paginate (Pagination limit offset) =
  (param "limit" .~ [tshow limit]) . (param "offset" .~ [tshow offset])

articleFilter :: ArticleFilter -> Network.Wreq.Options -> Network.Wreq.Options
articleFilter (ArticleFilter mayTag mayAuthor mayFavoritedBy) = 
  maybe id (\x -> param "tag" .~ [x]) mayTag
  . maybe id (\x -> param "author" .~ [x]) mayAuthor
  . maybe id (\x -> param "favorited" .~ [x]) mayFavoritedBy



-- * Favorites

favoriteArticle :: (RW ArticleError r m) => Token -> Slug -> m Article
favoriteArticle token slug = do
  url <- buildUrl $ "/articles/" <> unpack slug <> "/favorite"
  let opts = defaults & authHeader token
  let body = Aeson.toJSON $ asText ""
  articleWrapperArticle <$> exec (postWith opts url body)

unfavoriteArticle :: (RW ArticleError r m) => Token -> Slug -> m Article
unfavoriteArticle token slug = do
  url <- buildUrl $ "/articles/" <> unpack slug <> "/favorite"
  let opts = defaults & authHeader token
  articleWrapperArticle <$> exec (deleteWith opts url)



-- * Comments

addComment :: (RW CommentError r m) => Token -> Slug -> Text -> m Comment
addComment token slug comment = do
  url <- buildUrl $ "/articles/" <> unpack slug <> "/comments"
  let opts = defaults & authHeader token
  let body = Aeson.object [ "comment" .= Aeson.object [ "body" .= comment ] ]
  commentWrapperComment <$> exec (postWith opts url body)

delComment :: (RW CommentError r m) => Token -> Slug -> CommentId -> m ()
delComment token slug cId = do
  url <- buildUrl $ "/articles/" <> unpack slug <> "/comments/" <> show cId
  let opts = defaults & authHeader token
  (const () . asText) <$> exec (deleteWith opts url)

getComments :: (RW CommentError r m) => Maybe Token -> Slug -> m [Comment]
getComments mayToken slug = do
  url <- buildUrl $ "/articles/" <> unpack slug <> "/comments"
  let opts = defaults & mayAuthHeader mayToken
  commentsWrapperComments <$> exec (getWith opts url)



-- * Tags

getTags :: (RW Text r m) => m (Set Tag)
getTags = do
  url <- buildUrl "/tags"
  tagsWrapperTags <$> exec (get url)
  
  

-- * Healh

health :: (RW Text r m) => m Bool
health = do
  url <- buildUrl "/health"
  exec $ get url



-- * Utils

exec :: (RW e r m, Aeson.FromJSON a, Aeson.FromJSON e) => IO (Response LByteString) -> m a
exec req = do
  r <- liftIO (asJSON =<< req)
    `catch` handleHttpException
    `catch` handleJSONError
    `catch` handleOtherException
  return $ r ^. responseBody
  where
    handleJSONError (JSONError err) = throwError $ ErrMalformedJSON $ tshow err
    handleHttpException (HC.HttpExceptionRequest _ (HC.StatusCodeException res body)) =
      let status = HC.responseStatus res
      in  if status == status500 then
            throwError $ ErrInternalServerError body
          else if status == status401 then
            parseJSONError body ErrUnauthorized
          else if status == status422 then
            parseJSONError body (ErrInvalidInput . errorsWrapperErrors)
          else
            parseJSONError body ErrApp
    handleHttpException err = throwError $ ErrUnknown $ tshow err
    handleOtherException (e :: SomeException) = throwError $ ErrUnknown $ tshow e
    parseJSONError src f = case Aeson.eitherDecode $ fromStrict src of
      Left parseErr -> throwError $ ErrMalformedJSON $ tshow parseErr
      Right parseResult -> throwError $ f parseResult

authHeader :: Token -> Network.Wreq.Options -> Network.Wreq.Options
authHeader token = header "Authorization" .~ ["Token " <> fromString (unpack token)]

mayAuthHeader :: Maybe Token -> Network.Wreq.Options -> Network.Wreq.Options
mayAuthHeader = maybe id authHeader