module Core.Services where

import ClassyPrelude hiding (RealWorld)
import Core.Types
import Control.Monad.Except
import qualified Web.Slug as WSlug
import Data.Convertible (convert)
import System.Posix.Types (EpochTime)

orThrow :: (MonadError e m) => m (Maybe a) -> e -> m a
orThrow action e = do
  result <- action
  maybe (throwError e) return result

-- * User

login :: (UserRepo m, TokenRepo m) => Auth -> ExceptT UserError m User
login auth = do
  (uId, user) <- (lift $ findUserByAuth auth) `orThrow` UserErrorBadAuth auth
  token <- lift $ generateToken uId
  return $ user { userToken = token }

register :: (UserRepo m, TokenRepo m) => Register -> ExceptT UserError m User
register param@(Register _ email pass) = do
  let defaultImgUrl = "https://static.productionready.io/images/smiley-cyrus.jpg"
  addUser param defaultImgUrl
  login $ Auth email pass

getUser :: (UserRepo m) => CurrentUser -> ExceptT UserError m User
getUser (token, userId) = do
  user <- (lift $ findUserById userId) `orThrow` UserErrorNotFound (tshow userId)
  return $ user { userToken = token }

updateUser :: (UserRepo m) => CurrentUser -> UpdateUser -> ExceptT UserError m User
updateUser curUser@(_, userId) param = do
  updateUserById userId param
  getUser curUser



-- * Profiles

getProfile :: (ProfileRepo m) => Maybe CurrentUser -> Username -> ExceptT UserError m Profile
getProfile mayCurUser username =
  (lift $ findProfile (snd <$> mayCurUser) username) `orThrow` UserErrorNotFound username

followUser :: (ProfileRepo m) => CurrentUser -> Username -> ExceptT UserError m Profile
followUser curUser@(_, curUserId) username = do
  followUserByUsername curUserId username
  getProfile (Just curUser) username

unfollowUser :: (ProfileRepo m) => CurrentUser -> Username -> ExceptT UserError m Profile
unfollowUser curUser@(_, curUserId) username = do
  lift $ unfollowUserByUsername curUserId username
  getProfile (Just curUser) username




-- * Articles

getArticles :: (ArticleRepo m) => Maybe CurrentUser -> ArticleFilter -> Pagination -> m [Article]
getArticles = findArticles Nothing Nothing

getFeed :: (ArticleRepo m) => CurrentUser -> Pagination -> m [Article]
getFeed curUser pagination =
  findArticles Nothing (Just True) (Just curUser) (ArticleFilter Nothing Nothing Nothing) pagination

getArticle :: (ArticleRepo m) => Maybe CurrentUser -> Slug -> ExceptT ArticleError m Article
getArticle mayCurUser slug = do
  result <- lift $ findArticles (Just slug) Nothing mayCurUser (ArticleFilter Nothing Nothing Nothing) (Pagination 1 0)
  case result of
    [article] -> return article
    _ -> throwError $ ArticleErrorNotFound slug

createArticle :: (ArticleRepo m, TimeRepo m) => CurrentUser -> CreateArticle -> ExceptT ArticleError m Article
createArticle curUser@(_, curUserId) param = do
  slug <- lift $ genSlug' (createArticleTitle param) curUserId
  lift $ addArticle curUserId param slug
  getArticle (Just curUser) slug
 
updateArticle :: (ArticleRepo m, TimeRepo m) => CurrentUser -> Slug -> UpdateArticle -> ExceptT ArticleError m Article
updateArticle curUser slug param = do
  validateArticleOwnedBy (snd curUser) slug
  newSlug <- case updateArticleTitle param of
    Nothing -> return slug
    Just newTitle -> lift $ genSlug' newTitle (snd curUser)
  lift $ updateArticleBySlug slug param newSlug
  getArticle (Just curUser) newSlug

genSlug' :: (TimeRepo m) => Text -> Integer -> m Text
genSlug' title uId = do
  createdAt <- currentTime
  return $ genSlug title uId $ convert createdAt

genSlug :: Text -> Integer -> EpochTime -> Text
genSlug title userId unixTs = 
  maybe "invalidSlug" WSlug.unSlug $ WSlug.mkSlug $ unwords [tshow userId, tshow unixTs, title]

deleteArticle :: (ArticleRepo m) => CurrentUser -> Slug -> ExceptT ArticleError m ()
deleteArticle (_, curUserId) slug = do
  validateArticleOwnedBy curUserId slug
  lift $ deleteArticleBySlug slug

validateArticleOwnedBy :: (ArticleRepo m) => UserId -> Slug -> ExceptT ArticleError m ()
validateArticleOwnedBy uId slug = do
  result <- lift $ isArticleOwnedBy uId slug
  case result of
    Nothing -> throwError $ ArticleErrorNotFound slug
    Just False -> throwError $ ArticleErrorNotAllowed slug
    _ -> return ()



-- * Favorites

favoriteArticle :: (ArticleRepo m) => CurrentUser -> Slug -> ExceptT ArticleError m Article
favoriteArticle curUser@(_, curUserId) slug = do
  lift $ favoriteArticleBySlug curUserId slug
  getArticle (Just curUser) slug

unfavoriteArticle :: (ArticleRepo m) => CurrentUser -> Slug -> ExceptT ArticleError m Article
unfavoriteArticle curUser@(_, curUserId) slug = do
  lift $ unfavoriteArticleBySlug curUserId slug
  getArticle (Just curUser) slug



-- * Comments

addComment :: (CommentRepo m, ArticleRepo m) => CurrentUser -> Slug -> Text -> ExceptT CommentError m Comment
addComment curUser@(_, curUserId) slug comment = do
  cId <- lift $ addCommentToSlug curUserId slug comment
  comments <- getComments' (Just curUser) slug (Just cId)
  case comments of
    [a] -> return a
    _ -> throwError $ CommentErrorNotFound cId

delComment :: (CommentRepo m, ArticleRepo m) => CurrentUser -> Slug -> CommentId -> ExceptT CommentError m ()
delComment (_, curUserId) slug cId = do
  validateArticleExists slug
  validateCommentExists cId
  validateCommentOwnedBy curUserId cId
  lift $ delCommentById cId

getComments :: (CommentRepo m, ArticleRepo m) => Maybe CurrentUser -> Slug -> ExceptT CommentError m [Comment]
getComments mayCurUser slug = getComments' mayCurUser slug Nothing

getComments' :: (CommentRepo m, ArticleRepo m) => Maybe CurrentUser -> Slug -> Maybe CommentId -> ExceptT CommentError m [Comment]
getComments' mayCurUser slug mayCommentId = do
  validateArticleExists slug
  lift $ findComments (snd <$> mayCurUser) slug mayCommentId

validateArticleExists :: (CommentRepo m, ArticleRepo m) => Slug -> ExceptT CommentError m ()
validateArticleExists slug = do
  result <- lift $ isArticleExist slug
  unless result $ throwError (CommentErrorSlugNotFound slug)

validateCommentOwnedBy :: (CommentRepo m) => UserId -> CommentId -> ExceptT CommentError m ()
validateCommentOwnedBy uId cId = do
  result <- lift $ isCommentOwnedBy uId cId
  unless result $ throwError (CommentErrorNotAllowed cId)

validateCommentExists :: (CommentRepo m) => CommentId -> ExceptT CommentError m ()
validateCommentExists cId = do
  result <- lift $ isCommentExist cId
  unless result $ throwError (CommentErrorNotFound cId)


-- * Tags

getTags :: (TagRepo m) => m (Set Tag)
getTags = allTags