module Feature.User.PG where

import ClassyPrelude
import Feature.User.Types
import Feature.Auth.Types
import Platform.PG
import Database.PostgreSQL.Simple

-- * UserRepo

findUserByAuth :: PG r m => Auth -> m (Maybe (UserId, User))
findUserByAuth (Auth email pass) = do
  results <- withConn $ \conn -> query conn qry (email, pass)
  case results of
    [(uId, name, bio, image)] ->
      return $ Just (uId, User email "tempToken" name bio image)
    _ ->
      return Nothing
  where
    qry = "select id, cast (name as text), bio, image \
          \from users where email = ? AND pass = crypt(?, pass) \
          \limit 1"

findUserById :: PG r m => UserId -> m (Maybe User)
findUserById uId = do
  results <- withConn $ \conn -> query conn qry (Only uId)
  case results of
    [(name, email, bio, image)] ->
      return $ Just $ User email "tempToken" name bio image
    _ ->
      return Nothing
  where
    qry = "select cast (name as text), cast (email as text), bio, image \
          \from users where id = ? limit 1"

addUser :: (PG r m) => Register -> Text -> m (Either UserError ())
addUser (Register name email pass) defaultImgUrl = do
  result <- withConn $ try . action
  return $ bimap (translateSqlUserError email name) (const ()) result
  where
    action conn = execute conn qry (name, email, pass, defaultImgUrl)
    qry = "insert into users (name, email, pass, bio, image) \
          \values (?, ?, crypt(?, gen_salt('bf')), '', ?)"

updateUserById :: (PG r m) => UserId -> UpdateUser -> m (Either UserError ())
updateUserById uId (UpdateUser email uname pass img bio) = do
  result <- withConn $ try . action
  return $ bimap (translateSqlUserError (fromMaybe "" email) (fromMaybe "" uname)) (const ()) result
  where
    action conn = execute conn qry (email, uname, pass, img, bio, uId)
    qry = "update users set \
          \email = coalesce(?, email), \
          \name = coalesce(?, name), \
          \pass = coalesce(crypt(?, gen_salt('bf')), pass), \
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
  results <- withConn $ \conn -> query conn qry (mayUserId, username)
  return $ case results of
    [a] -> Just a
    _ -> Nothing
  where
    qry = "select cast (name as text), bio, image, exists(select 1 from followings where user_id = id and followed_by = ?) as following \
          \from users where name = ? limit 1"

followUserByUsername :: (PG r m) => UserId -> Username -> m (Either UserError ())
followUserByUsername uId username = do
  result <- withConn $ \conn -> try $ execute conn qry (uId, username)
  return $ case result of
    Left SqlError{sqlState="23503"} ->
      Left $ UserErrorNotFound username
    Left err ->
      error $ "Unhandled PG error: " <> show err
    Right _ ->
      Right ()
  where
    qry = "insert into followings (followed_by, user_id) \
          \(select ?, id from users where name = ? limit 1) on conflict do nothing"

unfollowUserByUsername :: PG r m => UserId -> Username -> m ()
unfollowUserByUsername uId username =
  void . withConn $ \conn -> execute conn qry (uId, username)
  where
    qry = "delete from followings where \
          \followed_by = ? and user_id in (select id from users where name = ? limit 1)"
