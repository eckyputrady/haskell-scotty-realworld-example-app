module RealWorld where

import ClassyPrelude hiding (RealWorld)
import Struct
import Control.Monad.Except
import qualified Web.Slug as WSlug
import Data.Convertible (convert)
import System.Posix.Types (EpochTime)
import Control.Lens

type RW e r m = (MonadError e m)

orThrow :: (MonadError e m) => m (Maybe a) -> e -> m a
orThrow action e = do
  result <- action
  maybe (throwError e) return result

orThrowE :: (MonadError e' m) => m (Either e a) -> (e -> e') -> m a
orThrowE action f = do
  result <- action
  either (throwError . f) return result

-- * User

login :: (AsUserError e, RW e r m, UserRepo m, TokenRepo m) => Auth -> m User
login auth = do
  (uId, user) <- findUserByAuth auth `orThrow` review _UserErrorBadAuth auth
  token <- generateToken uId
  return $ user { userToken = token }

register :: (AsUserError e, RW e r m, UserRepo m, TokenRepo m) => Register -> m User
register param@(Register _ email pass) = do
  let defaultImgUrl = "https://static.productionready.io/images/smiley-cyrus.jpg"
  addUser param defaultImgUrl `orThrowE` id
  login $ Auth email pass

getUser :: (AsUserError e, MonadError e m,  UserRepo m) => CurrentUser -> m User
getUser (token, userId) = do
  user <- findUserById userId `orThrow` review _UserErrorNotFound (tshow userId)
  return $ user { userToken = token }

updateUser :: (AsUserError e, RW e r m, UserRepo m) => CurrentUser -> UpdateUser -> m User
updateUser curUser@(_, userId) param = do
  updateUserById userId param `orThrowE` id
  getUser curUser



-- * Profiles

getProfile :: (AsUserError e, RW e r m, ProfileRepo m) => Maybe CurrentUser -> Username -> m Profile
getProfile mayCurUser username =
  findProfile (snd <$> mayCurUser) username `orThrow` review _UserErrorNotFound username

followUser :: (AsUserError e, RW e r m, ProfileRepo m) => CurrentUser -> Username -> m Profile
followUser curUser@(_, curUserId) username = do
  followUserByUsername curUserId username `orThrowE` id
  getProfile (Just curUser) username

unfollowUser :: (AsUserError e, RW e r m, ProfileRepo m) => CurrentUser -> Username -> m Profile
unfollowUser curUser@(_, curUserId) username = do
  unfollowUserByUsername curUserId username
  getProfile (Just curUser) username




-- * Articles

getArticles :: (ArticleRepo m) => Maybe CurrentUser -> ArticleFilter -> Pagination -> m [Article]
getArticles = findArticles Nothing Nothing

getFeed :: (ArticleRepo m) => CurrentUser -> Pagination -> m [Article]
getFeed curUser pagination =
  findArticles Nothing (Just True) (Just curUser) (ArticleFilter Nothing Nothing Nothing) pagination

getArticle :: (AsArticleError e, RW e r m, ArticleRepo m) => Maybe CurrentUser -> Slug -> m Article
getArticle mayCurUser slug = do
  result <- findArticles (Just slug) Nothing mayCurUser (ArticleFilter Nothing Nothing Nothing) (Pagination 1 0)
  case result of
    [article] -> return article
    _ -> throwError $ review _ArticleErrorNotFound slug

createArticle :: (AsArticleError e, RW e r m, ArticleRepo m, TimeRepo m) => CurrentUser -> CreateArticle -> m Article
createArticle curUser@(_, curUserId) param = do
  slug <- genSlug' (createArticleTitle param) curUserId
  addArticle curUserId param slug
  getArticle (Just curUser) slug
 
updateArticle :: (AsArticleError e, RW e r m, ArticleRepo m, TimeRepo m) => CurrentUser -> Slug -> UpdateArticle -> m Article
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

deleteArticle :: (AsArticleError e, RW e r m, ArticleRepo m) => CurrentUser -> Slug -> m ()
deleteArticle (_, curUserId) slug = do
  validateArticleOwnedBy curUserId slug
  deleteArticleBySlug slug

validateArticleOwnedBy :: (AsArticleError e, RW e r m, ArticleRepo m) => UserId -> Slug -> m ()
validateArticleOwnedBy uId slug = do
  result <- isArticleOwnedBy uId slug
  case result of
    Nothing -> throwError $ review _ArticleErrorNotFound slug
    Just False -> throwError $ review _ArticleErrorNotAllowed slug
    _ -> return ()



-- * Favorites

favoriteArticle :: (AsArticleError e, RW e r m, ArticleRepo m) => CurrentUser -> Slug -> m Article
favoriteArticle curUser@(_, curUserId) slug = do
  favoriteArticleBySlug curUserId slug
  getArticle (Just curUser) slug

unfavoriteArticle :: (AsArticleError e, RW e r m, ArticleRepo m) => CurrentUser -> Slug -> m Article
unfavoriteArticle curUser@(_, curUserId) slug = do
  unfavoriteArticleBySlug curUserId slug
  getArticle (Just curUser) slug



-- * Comments

addComment :: (AsCommentError e, RW e r m, CommentRepo m, ArticleRepo m) => CurrentUser -> Slug -> Text -> m Comment
addComment curUser@(_, curUserId) slug comment = do
  cId <- addCommentToSlug curUserId slug comment
  comments <- getComments' (Just curUser) slug (Just cId)
  case comments of
    [a] -> return a
    _ -> throwError $ review _CommentErrorNotFound cId

delComment :: (AsCommentError e, RW e r m, CommentRepo m, ArticleRepo m) => CurrentUser -> Slug -> CommentId -> m ()
delComment (_, curUserId) slug cId = do
  validateArticleExists slug
  validateCommentExists cId
  validateCommentOwnedBy curUserId cId
  delCommentFromSlug slug cId

getComments :: (AsCommentError e, RW e r m, CommentRepo m, ArticleRepo m) => Maybe CurrentUser -> Slug -> m [Comment]
getComments mayCurUser slug = getComments' mayCurUser slug Nothing

getComments' :: (AsCommentError e, RW e r m, CommentRepo m, ArticleRepo m) => Maybe CurrentUser -> Slug -> Maybe CommentId -> m [Comment]
getComments' mayCurUser slug mayCommentId = do
  validateArticleExists slug
  findComments (snd <$> mayCurUser) slug mayCommentId

validateArticleExists :: (AsCommentError e, RW e r m, CommentRepo m, ArticleRepo m) => Slug -> m ()
validateArticleExists slug = do
  result <- isArticleExist slug
  unless result $ throwError (review _CommentErrorSlugNotFound slug)

validateCommentOwnedBy :: (AsCommentError e, RW e r m, CommentRepo m) => UserId -> CommentId -> m ()
validateCommentOwnedBy uId cId = do
  result <- isCommentOwnedBy uId cId
  unless result $ throwError (review _CommentErrorNotAllowed cId)

validateCommentExists :: (AsCommentError e, RW e r m, CommentRepo m) => CommentId -> m ()
validateCommentExists cId = do
  result <- isCommentExist cId
  unless result $ throwError (review _CommentErrorNotFound cId)


-- * Tags

getTags :: (TagRepo m) => m (Set Tag)
getTags = allTags