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

-- * User

login :: (RW UserError r m) => Auth -> m User
login auth@(Auth email pass) = do
  conn <- asks getter
  results <- liftIO $ query conn qry (email, pass)
  case results of
    [(uId, name, bio, image)] -> do
      token <- generateToken uId
      return $ User email token name bio image
    _ ->
      throwError $ UserErrorBadAuth auth
  where
    qry = "select id, cast (name as text), bio, image from users where email = ? AND pass = ? limit 1"

register :: (RW UserError r m) => Register -> m User
register (Register name email pass) = do
  conn <- asks getter
  (void . liftIO $ execute conn "insert into users (name, email, pass, bio, image) values (?, ?, ?, '', 'https://static.productionready.io/images/smiley-cyrus.jpg')" (name, email, pass))
    `catch` handleSqlUserError email name
  login (Auth email pass)

getUser :: (RW UserError r m) => CurrentUser -> m User
getUser (token, userId) = do
  conn <- asks getter
  results <- liftIO $ query conn qry (Only userId)
  case results of
    [(name, email, bio, image)] -> return $ User email token name bio image
    _ -> throwError $ UserErrorNotFound $ tshow userId
  where
    qry = "select cast (name as text), cast (email as text), bio, image from users where id = ? limit 1"

updateUser :: (RW UserError r m) => CurrentUser -> UpdateUser -> m User
updateUser (token, userId) (UpdateUser email uname pass img bio) = do
  conn <- asks getter
  (void . liftIO $ execute conn qry (email, uname, pass, img, bio, userId))
    `catch` handleSqlUserError (fromMaybe "" email) (fromMaybe "" uname)
  getUser (token, userId)
  where
    qry = "update users set \
          \email = coalesce(?, email), \
          \name = coalesce(?, name), \
          \pass = coalesce(?, pass), \
          \image = coalesce(?, image), \
          \bio = coalesce(?, bio) \
          \where id = ?"



-- * Profiles

getProfile :: (RW UserError r m) => Maybe CurrentUser -> Username -> m Profile
getProfile mayCurUser username = do
  conn <- asks getter
  results <- liftIO $ query conn qry (snd <$> mayCurUser, username)
  case results of
    [a] -> return $ unFR a
    _ -> throwError $ UserErrorNotFound username
  where
    qry = "select cast (name as text), bio, image, exists(select 1 from followings where user_id = id and followed_by = ?) as following \
          \from users where name = ? limit 1"

followUser :: (RW UserError r m) => CurrentUser -> Username -> m Profile
followUser curUser@(_, curUserId) username = do
  conn <- asks getter
  (void . liftIO $ execute conn qry (curUserId, username))
    `catch` \SqlError{sqlState="23503"} -> throwError (UserErrorNotFound username)
  getProfile (Just curUser) username
  where
    qry = "insert into followings (followed_by, user_id) (select ?, id from users where name = ? limit 1) on conflict do nothing"

unfollowUser :: (RW UserError r m) => CurrentUser -> Username -> m Profile
unfollowUser curUser@(_, curUserId) username = do
  conn <- asks getter
  void . liftIO $ execute conn qry (curUserId, username)
  getProfile (Just curUser) username
  where
    qry = "delete from followings where followed_by = ? and user_id in (select id from users where name = ? limit 1)"




-- * Articles

getArticles' :: (RW ArticleError r m) => Maybe Slug -> Maybe Bool -> Maybe CurrentUser -> ArticleFilter -> Pagination -> m [Article]
getArticles' maySlug mayFollowing mayCurrentUser articleFilter pagination = do
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
            ),
            formatted_articles as (
              select 
                articles.id, slug, title, description, body, tags, created_at, updated_at,
                exists(select 1 from favorites where article_id = articles.id and favorited_by = ?) as favorited,
                (select count(1) from favorites where article_id = articles.id) as favorites_count,
                profiles.name as pname, profiles.bio as pbio, profiles.image as pimage, profiles.following as pfollowing
              from
                articles join profiles on articles.author_id = profiles.id
            )
            select
              cast (slug as text), title, description, body, cast (tags as text[]), created_at, updated_at,
              favorited, favorites_count, cast (pname as text), pbio, pimage, pfollowing
            from
              formatted_articles
            where
              -- by slug (for find one)
              coalesce(slug in ?, true) AND
              -- by if user following author (for feed)
              coalesce(pfollowing in ?, true) and
              -- by tag (for normal filter)
              (cast (tags as text[]) @> ?) and
              -- by author (for normal filter)
              coalesce (pname in ?, true) and
              -- by fav by (for normal filter)
              (? is null OR exists(
                select 1 
                from favorites join users on users.id = favorites.favorited_by 
                where article_id = formatted_articles.id and users.name = ?)
              )
            order by id desc
            limit greatest(0, ?) offset greatest(0, ?)
          |]
    curUserId = maybe (-1) snd mayCurrentUser
    arg = ( curUserId, curUserId
          -- ^ 2 slots for current user id
          , In $ maybeToList maySlug
          -- ^ 1 slot for slug
          , In $ maybeToList $ mayFollowing
          -- ^ 1 slot for following
          , PGArray $ maybeToList $ articleFilterTag articleFilter
          -- ^ 1 slot for tags
          , In $ maybeToList $ articleFilterAuthor articleFilter
          -- ^ 1 slot for author
          , articleFilterFavoritedBy articleFilter, articleFilterFavoritedBy articleFilter
          -- ^ 2 slots for favorited by user name
          , paginationLimit pagination, paginationOffset pagination
          -- ^ 2 slot for limit & offset
          )

getArticles :: (RW ArticleError r m) => Maybe CurrentUser -> ArticleFilter -> Pagination -> m [Article]
getArticles = getArticles' Nothing Nothing

getFeed :: (RW ArticleError r m) => CurrentUser -> Pagination -> m [Article]
getFeed curUser pagination =
  getArticles' Nothing (Just True) (Just curUser) (ArticleFilter Nothing Nothing Nothing) pagination

getArticle :: (RW ArticleError r m) => Maybe CurrentUser -> Slug -> m Article
getArticle mayCurUser slug = do
  result <- getArticles' (Just slug) Nothing mayCurUser (ArticleFilter Nothing Nothing Nothing) (Pagination 1 0)
  case result of
    [article] -> return article
    _ -> throwError $ ArticleErrorNotFound slug

createArticle :: (RW ArticleError r m) => CurrentUser -> CreateArticle -> m Article
createArticle curUser@(_, curUserId) param = do
  createdAt <- liftIO getCurrentTime
  let slug = genSlug (createArticleTitle param) curUserId (convert createdAt)
  conn <- asks getter
  void . liftIO $ execute conn qry 
    ( slug, createArticleTitle param, createArticleDescription param, createArticleBody param
    , createdAt, createdAt, curUserId, PGArray $ createArticleTagList param
    )
  getArticle (Just curUser) slug
  where
    qry = "insert into articles (slug, title, description, body, created_at, updated_at, author_id, tags) \
          \values (?, ?, ?, ?, ?, ?, ?, ?)"
 
updateArticle :: (RW ArticleError r m) => CurrentUser -> Slug -> UpdateArticle -> m Article
updateArticle curUser slug param = do
  validateArticleOwnedBy (snd curUser) slug
  createdAt <- liftIO getCurrentTime
  let newSlug = fromMaybe slug $ (\newTitle -> genSlug newTitle (snd curUser) (convert createdAt)) <$> (updateArticleTitle param)
  conn <- asks getter
  void . liftIO $ execute conn qry (newSlug, updateArticleTitle param, updateArticleDescription param, updateArticleBody param, slug)
  getArticle (Just curUser) newSlug
  where
    qry = "update articles \
          \set slug = ?, title = coalesce(?, title), description = coalesce(?, description), body = coalesce(?, body) \
          \where slug = ?"

genSlug :: Text -> Integer -> EpochTime -> Text
genSlug title userId unixTs = 
  maybe "invalidSlug" WSlug.unSlug $ WSlug.mkSlug $ unwords [tshow userId, tshow unixTs, title]

deleteArticle :: (RW ArticleError r m) => CurrentUser -> Slug -> m ()
deleteArticle (_, curUserId) slug = do
  validateArticleOwnedBy curUserId slug
  conn <- asks getter
  void . liftIO $ execute conn qry (Only slug)
  where
    qry = "delete from articles where slug = ?"

validateArticleOwnedBy :: (RW ArticleError r m) => UserId -> Slug -> m ()
validateArticleOwnedBy userId slug = do
  conn <- asks getter
  result <- liftIO $ query conn qry (userId, slug)
  case result of
    [Only True] -> return ()
    [Only False] -> throwError $ ArticleErrorNotAllowed slug
    _ -> throwError $ ArticleErrorNotFound slug
  where
    qry = "select author_id = ? from articles where slug = ? limit 1"



-- * Favorites

favoriteArticle :: (RW ArticleError r m) => CurrentUser -> Slug -> m Article
favoriteArticle curUser@(_, curUserId) slug = do
  conn <- asks getter
  void . liftIO $ execute conn qry arg
  getArticle (Just curUser) slug
  where
    qry = "with cte as ( \
          \ select id, ? from articles where slug = ? limit 1 \
          \) \
          \insert into favorites (article_id, favorited_by) (select * from cte) on conflict do nothing"
    arg = (curUserId, slug)

unfavoriteArticle :: (RW ArticleError r m) => CurrentUser -> Slug -> m Article
unfavoriteArticle curUser@(_, curUserId) slug = do
  conn <- asks getter
  void . liftIO $ execute conn qry arg
  getArticle (Just curUser) slug
  where
    qry = "with cte as ( \
          \ select id from articles where slug = ? limit 1 \
          \) \
          \delete from favorites where article_id in (select id from cte) and favorited_by = ?"
    arg = (slug, curUserId)



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



-- * Utils

isUniqueConstraintsViolation :: SqlError -> ByteString -> Bool
isUniqueConstraintsViolation SqlError{sqlState = state, sqlErrorMsg = msg} constraintName =
  state == "23505" && constraintName `isInfixOf` msg

handleSqlUserError :: (MonadError UserError m) => Email -> Username -> SqlError -> m a
handleSqlUserError email username sqlError = do
  when (isUniqueConstraintsViolation sqlError "users_email_key") (throwError $ UserErrorEmailTaken email)
  when (isUniqueConstraintsViolation sqlError "users_name_key") (throwError $ UserErrorNameTaken username)
  error $ "Unknown SQL error: " <> show sqlError


-- * PG Deserializations

-- newtype so that we can create non-orphan FromRow instance
newtype FRow a = FRow { unFR :: a }

instance FromRow (FRow Comment) where
  fromRow = FRow <$> (Comment <$> field <*> field <*> field <*> field <*> (unFR <$> fromRow))

instance FromRow (FRow Article) where
  fromRow = FRow <$> (Article <$> field <*> field <*> field <*> field <*> (fromPGArray <$> field) <*> field <*> field <*> field <*> field <*> (unFR <$> fromRow))

instance FromRow (FRow Profile) where
  fromRow = FRow <$> (Profile <$> field <*> field <*> field <*> field)