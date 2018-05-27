module Core.Services where

import ClassyPrelude hiding (RealWorld)
import Core.Types
import Control.Monad.Except
import qualified Web.Slug as WSlug
import Data.Convertible (convert)
import System.Posix.Types (EpochTime)

orThrow :: Monad m => m (Maybe a) -> e -> m (Either e a)
orThrow action e =
  maybe (Left e) Right <$> action

-- * User

login :: (UserRepo m, TokenRepo m) => Auth -> m (Either UserError User)
login auth = runExceptT $ do
  (uId, user) <- ExceptT $ findUserByAuth auth `orThrow` UserErrorBadAuth auth
  token <- lift $ generateToken uId
  return $ user { userToken = token }

register :: (UserRepo m, TokenRepo m) => Register -> m (Either UserError User)
register param@(Register _ email pass) = do
  let defaultImgUrl = "https://static.productionready.io/images/smiley-cyrus.jpg"
  result <- addUser param defaultImgUrl
  case result of
    Left err -> return . Left $ err
    Right _ -> login $ Auth email pass

getUser :: (UserRepo m) => CurrentUser -> m (Either UserError User)
getUser (token, userId) = runExceptT $ do
  user <- ExceptT $ findUserById userId `orThrow` UserErrorNotFound (tshow userId)
  return $ user { userToken = token }

updateUser :: (UserRepo m) => CurrentUser -> UpdateUser -> m (Either UserError User)
updateUser curUser@(_, userId) param = runExceptT $ do
  ExceptT $ updateUserById userId param
  ExceptT $ getUser curUser



-- * Profiles

getProfile :: (ProfileRepo m) => Maybe CurrentUser -> Username -> m (Either UserError Profile)
getProfile mayCurUser username =
  findProfile (snd <$> mayCurUser) username `orThrow` UserErrorNotFound username

followUser :: (ProfileRepo m) => CurrentUser -> Username -> m (Either UserError Profile)
followUser curUser@(_, curUserId) username = runExceptT $ do
  ExceptT $ followUserByUsername curUserId username
  ExceptT $ getProfile (Just curUser) username

unfollowUser :: (ProfileRepo m) => CurrentUser -> Username -> m (Either UserError Profile)
unfollowUser curUser@(_, curUserId) username = do
  unfollowUserByUsername curUserId username
  getProfile (Just curUser) username




-- * Articles

getArticles :: (ArticleRepo m) => Maybe CurrentUser -> ArticleFilter -> Pagination -> m [Article]
getArticles = findArticles Nothing Nothing

getFeed :: (ArticleRepo m) => CurrentUser -> Pagination -> m [Article]
getFeed curUser =
  findArticles Nothing (Just True) (Just curUser) (ArticleFilter Nothing Nothing Nothing)

getArticle :: (ArticleRepo m) => Maybe CurrentUser -> Slug -> m (Either ArticleError Article)
getArticle mayCurUser slug = runExceptT $ do
  result <- lift $ findArticles (Just slug) Nothing mayCurUser (ArticleFilter Nothing Nothing Nothing) (Pagination 1 0)
  case result of
    [article] -> return article
    _ -> throwError $ ArticleErrorNotFound slug

createArticle :: (ArticleRepo m, TimeRepo m) => CurrentUser -> CreateArticle -> m (Either ArticleError Article)
createArticle curUser@(_, curUserId) param = do
  slug <- genSlug' (createArticleTitle param) curUserId
  addArticle curUserId param slug
  getArticle (Just curUser) slug
 
updateArticle :: (ArticleRepo m, TimeRepo m) => CurrentUser -> Slug -> UpdateArticle -> m (Either ArticleError Article)
updateArticle curUser slug param = runExceptT $ do
  ExceptT $ validateArticleOwnedBy (snd curUser) slug
  newSlug <- case updateArticleTitle param of
    Nothing -> return slug
    Just newTitle -> lift $ genSlug' newTitle (snd curUser)
  lift $ updateArticleBySlug slug param newSlug
  ExceptT $ getArticle (Just curUser) newSlug

genSlug' :: (TimeRepo m) => Text -> Integer -> m Text
genSlug' title uId = genSlug title uId . convert <$> currentTime

genSlug :: Text -> Integer -> EpochTime -> Text
genSlug title userId unixTs = 
  maybe "invalidSlug" WSlug.unSlug $ WSlug.mkSlug $ unwords [tshow userId, tshow unixTs, title]

deleteArticle :: (ArticleRepo m) => CurrentUser -> Slug -> m (Either ArticleError ())
deleteArticle (_, curUserId) slug = runExceptT $ do
  ExceptT $ validateArticleOwnedBy curUserId slug
  lift $ deleteArticleBySlug slug

validateArticleOwnedBy :: (ArticleRepo m) => UserId -> Slug -> m (Either ArticleError ())
validateArticleOwnedBy uId slug = runExceptT $ do
  result <- lift $ isArticleOwnedBy uId slug
  case result of
    Nothing -> throwError $ ArticleErrorNotFound slug
    Just False -> throwError $ ArticleErrorNotAllowed slug
    _ -> return ()



-- * Favorites

favoriteArticle :: (ArticleRepo m) => CurrentUser -> Slug -> m (Either ArticleError Article)
favoriteArticle curUser@(_, curUserId) slug = do
  favoriteArticleBySlug curUserId slug
  getArticle (Just curUser) slug

unfavoriteArticle :: (ArticleRepo m) => CurrentUser -> Slug -> m (Either ArticleError Article)
unfavoriteArticle curUser@(_, curUserId) slug = do
  unfavoriteArticleBySlug curUserId slug
  getArticle (Just curUser) slug



-- * Comments

addComment :: (CommentRepo m, ArticleRepo m) => CurrentUser -> Slug -> Text -> m (Either CommentError Comment)
addComment curUser@(_, curUserId) slug comment = runExceptT $ do
  cId <- lift $ addCommentToSlug curUserId slug comment
  comments <- ExceptT $ getComments' (Just curUser) slug (Just cId)
  case comments of
    [a] -> return a
    _ -> throwError $ CommentErrorNotFound cId

delComment :: (CommentRepo m, ArticleRepo m) => CurrentUser -> Slug -> CommentId -> m (Either CommentError ())
delComment (_, curUserId) slug cId = runExceptT $ do
  ExceptT $ validateArticleExists slug
  ExceptT $ validateCommentExists cId
  ExceptT $ validateCommentOwnedBy curUserId cId
  lift $ delCommentById cId

getComments :: (CommentRepo m, ArticleRepo m) => Maybe CurrentUser -> Slug -> m (Either CommentError [Comment])
getComments mayCurUser slug = getComments' mayCurUser slug Nothing

getComments' :: (CommentRepo m, ArticleRepo m) => Maybe CurrentUser -> Slug -> Maybe CommentId -> m (Either CommentError [Comment])
getComments' mayCurUser slug mayCommentId = runExceptT $ do
  ExceptT $ validateArticleExists slug
  lift $ findComments (snd <$> mayCurUser) slug mayCommentId

validateArticleExists :: (CommentRepo m, ArticleRepo m) => Slug -> m (Either CommentError ())
validateArticleExists slug = runExceptT $ do
  result <- lift $ isArticleExist slug
  unless result $ throwError (CommentErrorSlugNotFound slug)

validateCommentOwnedBy :: (CommentRepo m) => UserId -> CommentId -> m (Either CommentError ())
validateCommentOwnedBy uId cId = runExceptT $ do
  result <- lift $ isCommentOwnedBy uId cId
  unless result $ throwError (CommentErrorNotAllowed cId)

validateCommentExists :: (CommentRepo m) => CommentId -> m (Either CommentError ())
validateCommentExists cId = runExceptT $ do
  result <- lift $ isCommentExist cId
  unless result $ throwError (CommentErrorNotFound cId)


-- * Tags

getTags :: (TagRepo m) => m (Set Tag)
getTags = allTags