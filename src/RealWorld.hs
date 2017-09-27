module RealWorld where

import ClassyPrelude hiding (RealWorld)
import Struct
import Control.Monad.Except
import qualified Web.Slug as WSlug
import Data.Convertible (convert)
import System.Posix.Types (EpochTime)

orThrow :: (MonadError e m) => m (Maybe a) -> e -> m a
orThrow action e = do
  result <- action
  maybe (throwError e) return result

orThrowE :: (MonadError e' m) => m (Either e a) -> (e -> e') -> m a
orThrowE action f = do
  result <- action
  either (throwError . f) return result

-- * User

login :: (MonadError UserError m, UserRepo m, TokenRepo m) => Auth -> m User
login auth = do
  (uId, user) <- findUserByAuth auth `orThrow` UserErrorBadAuth auth
  token <- generateToken uId
  return $ user { userToken = token }

register :: (MonadError UserError m, UserRepo m, TokenRepo m) => Register -> m User
register param@(Register _ email pass) = do
  let defaultImgUrl = "https://static.productionready.io/images/smiley-cyrus.jpg"
  addUser param defaultImgUrl `orThrowE` id
  login $ Auth email pass

getUser :: (MonadError UserError m,  UserRepo m) => CurrentUser -> m User
getUser (token, userId) = do
  user <- findUserById userId `orThrow` UserErrorNotFound (tshow userId)
  return $ user { userToken = token }

updateUser :: (MonadError UserError m, UserRepo m) => CurrentUser -> UpdateUser -> m User
updateUser curUser@(_, userId) param = do
  updateUserById userId param `orThrowE` id
  getUser curUser



-- * Profiles

getProfile :: (MonadError UserError m, ProfileRepo m) => Maybe CurrentUser -> Username -> m Profile
getProfile mayCurUser username =
  findProfile (snd <$> mayCurUser) username `orThrow` UserErrorNotFound username

followUser :: (MonadError UserError m, ProfileRepo m) => CurrentUser -> Username -> m Profile
followUser curUser@(_, curUserId) username = do
  followUserByUsername curUserId username `orThrowE` id
  getProfile (Just curUser) username

unfollowUser :: (MonadError UserError m, ProfileRepo m) => CurrentUser -> Username -> m Profile
unfollowUser curUser@(_, curUserId) username = do
  unfollowUserByUsername curUserId username
  getProfile (Just curUser) username




-- * Articles

getArticles :: (ArticleRepo m) => Maybe CurrentUser -> ArticleFilter -> Pagination -> m [Article]
getArticles = findArticles Nothing Nothing

getFeed :: (ArticleRepo m) => CurrentUser -> Pagination -> m [Article]
getFeed curUser pagination =
  findArticles Nothing (Just True) (Just curUser) (ArticleFilter Nothing Nothing Nothing) pagination

getArticle :: (MonadError ArticleError m, ArticleRepo m) => Maybe CurrentUser -> Slug -> m Article
getArticle mayCurUser slug = do
  result <- findArticles (Just slug) Nothing mayCurUser (ArticleFilter Nothing Nothing Nothing) (Pagination 1 0)
  case result of
    [article] -> return article
    _ -> throwError $ ArticleErrorNotFound slug

createArticle :: (MonadError ArticleError m, ArticleRepo m, TimeRepo m) => CurrentUser -> CreateArticle -> m Article
createArticle curUser@(_, curUserId) param = do
  slug <- genSlug' (createArticleTitle param) curUserId
  addArticle curUserId param slug
  getArticle (Just curUser) slug
 
updateArticle :: (MonadError ArticleError m, ArticleRepo m, TimeRepo m) => CurrentUser -> Slug -> UpdateArticle -> m Article
updateArticle curUser slug param = do
  validateArticleOwnedBy (snd curUser) slug
  newSlug <- case updateArticleTitle param of
    Nothing -> return slug
    Just newTitle -> genSlug' newTitle (snd curUser)
  updateArticleBySlug slug param newSlug
  getArticle (Just curUser) newSlug

genSlug' :: (TimeRepo m) => Text -> Integer -> m Text
genSlug' title uId = do
  createdAt <- currentTime
  return $ genSlug title uId $ convert createdAt

genSlug :: Text -> Integer -> EpochTime -> Text
genSlug title userId unixTs = 
  maybe "invalidSlug" WSlug.unSlug $ WSlug.mkSlug $ unwords [tshow userId, tshow unixTs, title]

deleteArticle :: (MonadError ArticleError m, ArticleRepo m) => CurrentUser -> Slug -> m ()
deleteArticle (_, curUserId) slug = do
  validateArticleOwnedBy curUserId slug
  deleteArticleBySlug slug

validateArticleOwnedBy :: (MonadError ArticleError m, ArticleRepo m) => UserId -> Slug -> m ()
validateArticleOwnedBy uId slug = do
  result <- isArticleOwnedBy uId slug
  case result of
    Nothing -> throwError $ ArticleErrorNotFound slug
    Just False -> throwError $ ArticleErrorNotAllowed slug
    _ -> return ()



-- * Favorites

favoriteArticle :: (MonadError ArticleError m, ArticleRepo m) => CurrentUser -> Slug -> m Article
favoriteArticle curUser@(_, curUserId) slug = do
  favoriteArticleBySlug curUserId slug
  getArticle (Just curUser) slug

unfavoriteArticle :: (MonadError ArticleError m, ArticleRepo m) => CurrentUser -> Slug -> m Article
unfavoriteArticle curUser@(_, curUserId) slug = do
  unfavoriteArticleBySlug curUserId slug
  getArticle (Just curUser) slug



-- * Comments

addComment :: (MonadError CommentError m, CommentRepo m, ArticleRepo m) => CurrentUser -> Slug -> Text -> m Comment
addComment curUser@(_, curUserId) slug comment = do
  cId <- addCommentToSlug curUserId slug comment
  comments <- getComments' (Just curUser) slug (Just cId)
  case comments of
    [a] -> return a
    _ -> throwError $ CommentErrorNotFound cId

delComment :: (MonadError CommentError m, CommentRepo m, ArticleRepo m) => CurrentUser -> Slug -> CommentId -> m ()
delComment (_, curUserId) slug cId = do
  validateArticleExists slug
  validateCommentExists cId
  validateCommentOwnedBy curUserId cId
  delCommentById cId

getComments :: (MonadError CommentError m, CommentRepo m, ArticleRepo m) => Maybe CurrentUser -> Slug -> m [Comment]
getComments mayCurUser slug = getComments' mayCurUser slug Nothing

getComments' :: (MonadError CommentError m, CommentRepo m, ArticleRepo m) => Maybe CurrentUser -> Slug -> Maybe CommentId -> m [Comment]
getComments' mayCurUser slug mayCommentId = do
  validateArticleExists slug
  findComments (snd <$> mayCurUser) slug mayCommentId

validateArticleExists :: (MonadError CommentError m, CommentRepo m, ArticleRepo m) => Slug -> m ()
validateArticleExists slug = do
  result <- isArticleExist slug
  unless result $ throwError (CommentErrorSlugNotFound slug)

validateCommentOwnedBy :: (MonadError CommentError m, CommentRepo m) => UserId -> CommentId -> m ()
validateCommentOwnedBy uId cId = do
  result <- isCommentOwnedBy uId cId
  unless result $ throwError (CommentErrorNotAllowed cId)

validateCommentExists :: (MonadError CommentError m, CommentRepo m) => CommentId -> m ()
validateCommentExists cId = do
  result <- isCommentExist cId
  unless result $ throwError (CommentErrorNotFound cId)


-- * Tags

getTags :: (TagRepo m) => m (Set Tag)
getTags = allTags