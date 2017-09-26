module RealWorld where

import ClassyPrelude hiding (RealWorld)
import Struct
import Control.Monad.Except
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Types
import Data.Has
import PG
import JWT
import qualified Web.Slug as WSlug
import Data.Convertible (convert)
import System.Posix.Types (EpochTime)

type RW e r m = (MonadError e m, PG r m, JWT r m)

orThrow :: (MonadError e m) => m (Maybe a) -> e -> m a
orThrow action e = do
  result <- action
  maybe (throwError e) return result

-- * User

login :: (RW UserError r m) => Auth -> m User
login auth = do
  (uId, user) <- findUserByAuth auth `orThrow` UserErrorBadAuth auth
  token <- generateToken uId
  return $ user { userToken = token }

register :: (RW UserError r m) => Register -> m User
register param@(Register _ email pass) = do
  let defaultImgUrl = "https://static.productionready.io/images/smiley-cyrus.jpg"
  result <- addUser param defaultImgUrl
  either throwError return result
  login $ Auth email pass

getUser :: (RW UserError r m) => CurrentUser -> m User
getUser (token, userId) = do
  user <- findUserById userId `orThrow` UserErrorNotFound (tshow userId)
  return $ user { userToken = token }

updateUser :: (RW UserError r m) => CurrentUser -> UpdateUser -> m User
updateUser curUser@(_, userId) param = do
  result <- updateUserById userId param
  either throwError return result
  getUser curUser



-- * Profiles

getProfile :: (RW UserError r m) => Maybe CurrentUser -> Username -> m Profile
getProfile mayCurUser username =
  findProfile (snd <$> mayCurUser) username `orThrow` UserErrorNotFound username

followUser :: (RW UserError r m) => CurrentUser -> Username -> m Profile
followUser curUser@(_, curUserId) username = do
  result <- followUserByUsername curUserId username
  either throwError return result
  getProfile (Just curUser) username

unfollowUser :: (RW UserError r m) => CurrentUser -> Username -> m Profile
unfollowUser curUser@(_, curUserId) username = do
  unfollowUserByUsername curUserId username
  getProfile (Just curUser) username




-- * Articles

getArticles :: (RW ArticleError r m) => Maybe CurrentUser -> ArticleFilter -> Pagination -> m [Article]
getArticles = findArticles Nothing Nothing

getFeed :: (RW ArticleError r m) => CurrentUser -> Pagination -> m [Article]
getFeed curUser pagination =
  findArticles Nothing (Just True) (Just curUser) (ArticleFilter Nothing Nothing Nothing) pagination

getArticle :: (RW ArticleError r m) => Maybe CurrentUser -> Slug -> m Article
getArticle mayCurUser slug = do
  result <- findArticles (Just slug) Nothing mayCurUser (ArticleFilter Nothing Nothing Nothing) (Pagination 1 0)
  case result of
    [article] -> return article
    _ -> throwError $ ArticleErrorNotFound slug

createArticle :: (RW ArticleError r m) => CurrentUser -> CreateArticle -> m Article
createArticle curUser@(_, curUserId) param = do
  slug <- genSlug' (createArticleTitle param) curUserId
  addArticle curUserId param slug
  getArticle (Just curUser) slug
 
updateArticle :: (RW ArticleError r m) => CurrentUser -> Slug -> UpdateArticle -> m Article
updateArticle curUser slug param = do
  validateArticleOwnedBy (snd curUser) slug
  newSlug <- case updateArticleTitle param of
    Nothing -> return slug
    Just newTitle -> genSlug' newTitle (snd curUser)
  updateArticleBySlug slug param newSlug
  getArticle (Just curUser) newSlug

genSlug' :: (MonadIO m) => Text -> Integer -> m Text
genSlug' title uId = do
  createdAt <- liftIO getCurrentTime
  return $ genSlug title uId $ convert createdAt

genSlug :: Text -> Integer -> EpochTime -> Text
genSlug title userId unixTs = 
  maybe "invalidSlug" WSlug.unSlug $ WSlug.mkSlug $ unwords [tshow userId, tshow unixTs, title]

deleteArticle :: (RW ArticleError r m) => CurrentUser -> Slug -> m ()
deleteArticle (_, curUserId) slug = do
  validateArticleOwnedBy curUserId slug
  deleteArticleBySlug slug



-- * Favorites

favoriteArticle :: (RW ArticleError r m) => CurrentUser -> Slug -> m Article
favoriteArticle curUser@(_, curUserId) slug = do
  favoriteArticleBySlug curUserId slug
  getArticle (Just curUser) slug

unfavoriteArticle :: (RW ArticleError r m) => CurrentUser -> Slug -> m Article
unfavoriteArticle curUser@(_, curUserId) slug = do
  unfavoriteArticleBySlug curUserId slug
  getArticle (Just curUser) slug



-- * Comments

addComment :: (RW CommentError r m) => CurrentUser -> Slug -> Text -> m Comment
addComment curUser@(_, curUserId) slug comment = do
  conn <- asks getter
  results <- liftIO $ query conn qry arg
  cId <- return $ case results of
    [Only cmtId] -> cmtId
    _ -> -1
  comments <- getComments' (Just curUser) slug (Just cId)
  case comments of
    [a] -> return a
    _ -> throwError $ CommentErrorNotFound cId
  where
    qry = "with cte as ( \
          \ select id from articles where slug = ? limit 1 \
          \) \
          \insert into comments (article_id, created_at, updated_at, body, author_id) \
          \(select id, now(), now(), ?, ? from cte) \
          \returning id"
    arg = (slug, comment, curUserId)

delComment :: (RW CommentError r m) => CurrentUser -> Slug -> CommentId -> m ()
delComment (_, curUserId) slug cId = do
  validateArticleExists slug
  validateCommentExists cId
  validateCommentOwnedBy curUserId cId
  conn <- asks getter
  void . liftIO $ execute conn qry (Only cId)
  where
    qry = "delete from comments where id = ?"

getComments :: (RW CommentError r m) => Maybe CurrentUser -> Slug -> m [Comment]
getComments mayCurUser slug = getComments' mayCurUser slug Nothing

getComments' :: (RW CommentError r m) => Maybe CurrentUser -> Slug -> Maybe CommentId -> m [Comment]
getComments' mayCurUser slug mayCommentId = do
  validateArticleExists slug
  conn <- asks getter
  results <- liftIO $ query conn qry arg
  return $ unFR <$> results
  where
    qry = [sql|
            with profiles as (
              select
                id, name, bio, image, exists(select 1 from followings where user_id = id and followed_by = ?) as following
              from
                users
            ), formatted_comments as (
              select
                c.article_id, c.id, c.created_at, c.updated_at, c.body,
                p.name as pname, p.bio as pbio, p.image as pimage, p.following as pfollowing
              from
                comments c join profiles p on p.id = c.author_id
            )
            select
              c.id, c.created_at, c.updated_at, c.body,
              cast (c.pname as text), c.pbio, c.pimage, c.pfollowing
            from
              formatted_comments c join articles a on c.article_id = a.id
            where
              a.slug = ? and
              coalesce(c.id = ?, true)
          |]
    arg = (snd <$> mayCurUser, slug, mayCommentId)

validateArticleExists :: (RW CommentError r m) => Slug -> m ()
validateArticleExists slug = do
  conn <- asks getter
  results <- liftIO $ query conn qry (Only slug)
  unless (results == [Only True]) $ throwError $ CommentErrorSlugNotFound slug
  where
    qry = "select true from articles where slug = ? limit 1"

validateCommentOwnedBy :: (RW CommentError r m) => UserId -> CommentId -> m ()
validateCommentOwnedBy uId cId = do
  conn <- asks getter
  results <- liftIO $ query conn qry arg
  unless (results == [Only True]) $ throwError $ CommentErrorNotAllowed cId
  where
    qry = "select true from comments where author_id = ? and id = ? limit 1"
    arg = (uId, cId)

validateCommentExists :: (RW CommentError r m) => CommentId -> m ()
validateCommentExists cId = do
  conn <- asks getter
  results <- liftIO $ query conn qry arg
  unless (results == [Only True]) $ throwError $ CommentErrorNotFound cId
  where
    qry = "select true from comments where id = ? limit 1"
    arg = (Only cId)


-- * Tags

getTags :: (RW e r m) => m (Set Tag)
getTags = do
  conn <- asks getter
  results <- liftIO $ query_ conn qry
  return $ setFromList $ (\(Only tag) -> tag) <$> results
  where
    qry = "select cast(tag as text) from (select distinct unnest(tags) as tag from articles) tags"


-- -- * PG Deserializations

-- -- newtype so that we can create non-orphan FromRow instance
-- newtype FRow a = FRow { unFR :: a }

-- instance FromRow (FRow Comment) where
--   fromRow = FRow <$> (Comment <$> field <*> field <*> field <*> field <*> (unFR <$> fromRow))

-- instance FromRow (FRow Article) where
--   fromRow = FRow <$> (Article <$> field <*> field <*> field <*> field <*> (fromPGArray <$> field) <*> field <*> field <*> field <*> field <*> (unFR <$> fromRow))

-- instance FromRow (FRow Profile) where
--   fromRow = FRow <$> (Profile <$> field <*> field <*> field <*> field)