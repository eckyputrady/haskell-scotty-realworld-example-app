module Feature.Comment.PG where

import ClassyPrelude
import Feature.Comment.Types
import Feature.Auth.Types
import Platform.PG
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple

findComments :: PG r m => Maybe UserId -> Slug -> Maybe CommentId -> m [Comment]
findComments mayUserId slug mayCommentId =
  withConn $ \conn -> query conn qry arg
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
    arg = (mayUserId, slug, mayCommentId)

addCommentToSlug :: PG r m => UserId -> Slug -> Text -> m CommentId
addCommentToSlug uId slug comment = do
  results <- withConn $ \conn -> query conn qry arg
  return $ case results of
    [Only cmtId] -> cmtId
    _ -> -1
  where
    qry = "with cte as ( \
          \ select id from articles where slug = ? limit 1 \
          \) \
          \insert into comments (article_id, created_at, updated_at, body, author_id) \
          \(select id, now(), now(), ?, ? from cte) \
          \returning id"
    arg = (slug, comment, uId)

isCommentExist :: PG r m => CommentId -> m Bool
isCommentExist cId = do
  results <- withConn $ \conn -> query conn qry arg
  return $ results == [Only True]
  where
    qry = "select true from comments where id = ? limit 1"
    arg = (Only cId)

isCommentOwnedBy :: PG r m => UserId -> CommentId -> m Bool
isCommentOwnedBy uId cId = do
  results <- withConn $ \conn -> query conn qry arg
  return $ results == [Only True]
  where
    qry = "select true from comments where author_id = ? and id = ? limit 1"
    arg = (uId, cId)
  
delCommentById :: PG r m => CommentId -> m ()
delCommentById cId =
  void . withConn $ \conn -> execute conn qry (Only cId)
  where
    qry = "delete from comments where id = ?"
