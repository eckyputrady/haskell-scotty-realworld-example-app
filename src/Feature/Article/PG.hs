module Feature.Article.PG where

import ClassyPrelude
import Feature.Article.Types
import Feature.Auth.Types
import Feature.Common.Types
import Platform.PG
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types

addArticle :: PG r m => UserId -> CreateArticle -> Slug -> m ()
addArticle uId param slug =
  void . withConn $ \conn -> execute conn qry 
    ( slug, createArticleTitle param, createArticleDescription param
    , createArticleBody param, uId, PGArray $ createArticleTagList param
    )
  where
    qry = "insert into articles (slug, title, description, body, created_at, updated_at, author_id, tags) \
          \values (?, ?, ?, ?, now(), now(), ?, ?)"

updateArticleBySlug :: PG r m => Slug -> UpdateArticle -> Slug -> m ()
updateArticleBySlug slug param newSlug =
  void . withConn $ \conn -> execute conn qry (newSlug, updateArticleTitle param, updateArticleDescription param, updateArticleBody param, slug)
  where
    qry = "update articles \
          \set slug = ?, title = coalesce(?, title), description = coalesce(?, description), \
          \    body = coalesce(?, body), updated_at = now() \
          \where slug = ?"

deleteArticleBySlug :: PG r m => Slug -> m ()
deleteArticleBySlug slug =
  void . withConn $ \conn -> execute conn qry (Only slug)
  where
    qry = "delete from articles where slug = ?"

isArticleOwnedBy :: PG r m
                 => UserId -> Slug -> m (Maybe Bool)
isArticleOwnedBy uId slug = do
  result <- withConn $ \conn -> query conn qry (uId, slug)
  case result of
    [Only True] -> return $ Just True
    [Only False] -> return $ Just False
    _ -> return Nothing
  where
    qry = "select author_id = ? from articles where slug = ? limit 1"

favoriteArticleBySlug :: PG r m => UserId -> Slug -> m ()
favoriteArticleBySlug uId slug =
  void . withConn $ \conn -> execute conn qry (uId, slug)
  where
    qry = "with cte as ( \
          \ select id, ? from articles where slug = ? limit 1 \
          \) \
          \insert into favorites (article_id, favorited_by) (select * from cte) on conflict do nothing"

unfavoriteArticleBySlug :: PG r m => UserId -> Slug -> m ()
unfavoriteArticleBySlug uId slug =
  void . withConn $ \conn -> execute conn qry (slug, uId)
  where
    qry = "with cte as ( \
          \ select id from articles where slug = ? limit 1 \
          \) \
          \delete from favorites where article_id in (select id from cte) and favorited_by = ?"

findArticles  :: PG r m
              => Maybe Slug -> Maybe Bool -> Maybe CurrentUser
              -> ArticleFilter -> Pagination
              -> m [Article]
findArticles maySlug mayFollowing mayCurrentUser articleFilter pagination =
  withConn $ \conn -> query conn qry arg
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
          
isArticleExist :: PG r m => Slug -> m Bool
isArticleExist slug = do
  results <- withConn $ \conn -> query conn qry (Only slug)
  return $ results == [Only True]
  where
    qry = "select true from articles where slug = ? limit 1"

allTags :: PG r m => m (Set Tag)
allTags = do
  results <- withConn $ \conn -> query_ conn qry
  return $ setFromList $ (\(Only tag) -> tag) <$> results
  where
    qry = "select cast(tag as text) from (select distinct unnest(tags) as tag from articles) tags"
