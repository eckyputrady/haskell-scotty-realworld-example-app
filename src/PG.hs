module PG where

import ClassyPrelude
import Data.Has
import Data.Pool
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Types
import System.Environment
import Struct
import Control.Monad.Except

type PG r m = (MonadReader r m, Has Connection r, MonadIO m, MonadCatch m)
    
acquirePool :: IO (Pool Connection)
acquirePool = do
  envUrl <- lookupEnv "DATABASE_URL"
  let pgUrl = fromString $ fromMaybe "postgresql://localhost/realworld" envUrl
  createPool (connectPostgreSQL pgUrl) close 1 10 10

migrateDb :: Pool Connection -> IO ()
migrateDb pool = withResource pool $ \conn ->
  void $ withTransaction conn (runMigration (ctx conn))
  where
    ctx = MigrationContext cmd False
    cmd = MigrationCommands [ MigrationInitialization, MigrationDirectory "postgresql" ]

-- * UserRepo

findUserByAuth :: PG r m => Auth -> m (Maybe (UserId, User))
findUserByAuth (Auth email pass) = do
  conn <- asks getter
  results <- liftIO $ query conn qry (email, pass)
  case results of
    [(uId, name, bio, image)] -> do
      return $ Just $ (uId, User email "tempToken" name bio image)
    _ ->
      return Nothing
  where
    qry = "select id, cast (name as text), bio, image \
          \from users where email = ? AND pass = ? limit 1"

findUserById :: PG r m => UserId -> m (Maybe User)
findUserById uId = do
  conn <- asks getter
  results <- liftIO $ query conn qry (Only uId)
  case results of
    [(name, email, bio, image)] ->
      return $ Just $ User email "tempToken" name bio image
    _ ->
      return Nothing
  where
    qry = "select cast (name as text), cast (email as text), bio, image \
          \from users where id = ? limit 1"

addUser :: PG r m => Register -> Text -> m (Either UserError ())
addUser (Register name email pass) defaultImgUrl = do
  conn <- asks getter
  ((Right <$>) . void . liftIO $ action conn)
    `catch` (return . Left . translateSqlUserError email name)
  where
    action conn = execute conn qry (name, email, pass, defaultImgUrl)
    qry = "insert into users (name, email, pass, bio, image) \
          \values (?, ?, ?, '', ?)"

updateUserById :: PG r m => UserId -> UpdateUser -> m (Either UserError ())
updateUserById uId (UpdateUser email uname pass img bio) = do
  conn <- asks getter
  ((Right <$>) . void . liftIO $ action conn)
    `catch` (return . Left . translateSqlUserError (fromMaybe "" email) (fromMaybe "" uname))
  where
    action conn = execute conn qry (email, uname, pass, img, bio, uId)
    qry = "update users set \
          \email = coalesce(?, email), \
          \name = coalesce(?, name), \
          \pass = coalesce(?, pass), \
          \image = coalesce(?, image), \
          \bio = coalesce(?, bio) \
          \where id = ?"

translateSqlUserError :: Email -> Username -> SqlError -> UserError
translateSqlUserError email username sqlError
  | isUniqueConstraintsViolation sqlError "users_email_key" =
      UserErrorEmailTaken email
  | isUniqueConstraintsViolation sqlError "users_name_key" =
      UserErrorNameTaken username
  | otherwise =
      error $ "Unknown SQL error: " <> show sqlError

isUniqueConstraintsViolation :: SqlError -> ByteString -> Bool
isUniqueConstraintsViolation SqlError{sqlState = state, sqlErrorMsg = msg} constraintName =
  state == "23505" && constraintName `isInfixOf` msg

-- * ProfileRepo

findProfile :: PG r m => Maybe UserId -> Username -> m (Maybe Profile)
findProfile mayUserId username = do
  conn <- asks getter
  results <- liftIO $ query conn qry (mayUserId, username)
  return $ case results of
    [a] -> Just $ unFR a
    _ -> Nothing
  where
    qry = "select cast (name as text), bio, image, exists(select 1 from followings where user_id = id and followed_by = ?) as following \
          \from users where name = ? limit 1"

followUserByUsername :: PG r m => UserId -> Username -> m (Either UserError ())
followUserByUsername uId username = do
  conn <- asks getter
  ((Right <$>) . void . liftIO $ execute conn qry (uId, username))
    `catch` \SqlError{sqlState="23503"} -> return $ Left (UserErrorNotFound username)
  where
    qry = "insert into followings (followed_by, user_id) \
          \(select ?, id from users where name = ? limit 1) on conflict do nothing"

unfollowUserByUsername :: PG r m => UserId -> Username -> m ()
unfollowUserByUsername uId username = do
  conn <- asks getter
  void . liftIO $ execute conn qry (uId, username)
  where
    qry = "delete from followings where \
          \followed_by = ? and user_id in (select id from users where name = ? limit 1)"

-- * ArticleRepo

addArticle :: PG r m => UserId -> CreateArticle -> Slug -> m ()
addArticle uId param slug = do
  conn <- asks getter
  void . liftIO $ execute conn qry 
    ( slug, createArticleTitle param, createArticleDescription param
    , createArticleBody param, uId, PGArray $ createArticleTagList param
    )
  where
    qry = "insert into articles (slug, title, description, body, created_at, updated_at, author_id, tags) \
          \values (?, ?, ?, ?, now(), now(), ?, ?)"

updateArticleBySlug :: PG r m => Slug -> UpdateArticle -> Slug -> m ()
updateArticleBySlug slug param newSlug = do
  conn <- asks getter
  void . liftIO $ execute conn qry (newSlug, updateArticleTitle param, updateArticleDescription param, updateArticleBody param, slug)
  where
    qry = "update articles \
          \set slug = ?, title = coalesce(?, title), description = coalesce(?, description), \
          \    body = coalesce(?, body), updated_at = now() \
          \where slug = ?"

deleteArticleBySlug :: PG r m => Slug -> m ()
deleteArticleBySlug slug = do
  conn <- asks getter
  void . liftIO $ execute conn qry (Only slug)
  where
    qry = "delete from articles where slug = ?"

validateArticleOwnedBy :: (MonadError ArticleError m, PG r m) 
                       => UserId -> Slug -> m ()
validateArticleOwnedBy uId slug = do
  conn <- asks getter
  result <- liftIO $ query conn qry (uId, slug)
  case result of
    [Only True] -> return ()
    [Only False] -> throwError $ ArticleErrorNotAllowed slug
    _ -> throwError $ ArticleErrorNotFound slug
  where
    qry = "select author_id = ? from articles where slug = ? limit 1"

findArticles  :: PG r m
              => Maybe Slug -> Maybe Bool -> Maybe CurrentUser
              -> ArticleFilter -> Pagination
              -> m [Article]
findArticles maySlug mayFollowing mayCurrentUser articleFilter pagination = do
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

-- * PG Deserializations

-- newtype so that we can create non-orphan FromRow instance
newtype FRow a = FRow { unFR :: a }

instance FromRow (FRow Comment) where
  fromRow = FRow <$> (Comment <$> field <*> field <*> field <*> field <*> (unFR <$> fromRow))

instance FromRow (FRow Article) where
  fromRow = FRow <$> (Article <$> field <*> field <*> field <*> field <*> (fromPGArray <$> field) <*> field <*> field <*> field <*> field <*> (unFR <$> fromRow))

instance FromRow (FRow Profile) where
  fromRow = FRow <$> (Profile <$> field <*> field <*> field <*> field)