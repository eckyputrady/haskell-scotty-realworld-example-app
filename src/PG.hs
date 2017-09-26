module PG where

import ClassyPrelude
import Data.Has
import Data.Pool
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
import System.Environment
import Struct

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