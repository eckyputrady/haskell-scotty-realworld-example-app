module Feature.Article.HTTP
      ( routes
      , Service(..)
      ) where

import ClassyPrelude hiding (delete)

import Feature.Article.Types
import Feature.User.Types
import Feature.Common.Types
import Feature.User.HTTP (TokenService, requireUser, optionalUser)
import Feature.Common.HTTP
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import qualified Text.Digestive.Form as DF
import Text.Digestive.Form ((.:))

class Service m where
  getArticles :: Maybe CurrentUser -> ArticleFilter -> Pagination -> m [Article]
  getFeed :: CurrentUser -> Pagination -> m [Article]
  getArticle :: Maybe CurrentUser -> Slug -> m (Either ArticleError Article)
  createArticle :: CurrentUser -> CreateArticle -> m (Either ArticleError Article)
  updateArticle :: CurrentUser -> Slug -> UpdateArticle -> m (Either ArticleError Article)
  deleteArticle :: CurrentUser -> Slug -> m (Either ArticleError ())


routes :: (TokenService m, Service m, MonadIO m) => ScottyT LText m ()
routes = do

  get "/api/articles" $ do
    curUser <- optionalUser
    pagination <- parsePagination
    articleFilter <- parseArticleFilter
    result <- lift $ getArticles curUser articleFilter pagination
    json $ ArticlesWrapper result (length result)

  get "/api/articles/feed" $ do
    curUser <- requireUser
    pagination <- parsePagination
    result <- lift $ getFeed curUser pagination
    json $ ArticlesWrapper result (length result)

  get "/api/articles/:slug" $ do
    curUser <- optionalUser
    slug <- param "slug"
    result <- stopIfError articleErrorHandler $ getArticle curUser slug
    json $ ArticleWrapper result

  post "/api/articles" $ do
    curUser <- requireUser
    req <- parseJsonBody ("article" .: createArticleForm)
    result <- stopIfError articleErrorHandler $ createArticle curUser req
    json $ ArticleWrapper result

  put "/api/articles/:slug" $ do
    curUser <- requireUser
    slug <- param "slug"
    req <- parseJsonBody ("article" .: updateArticleForm)
    result <- stopIfError articleErrorHandler $ updateArticle curUser slug req
    json $ ArticleWrapper result

  delete "/api/articles/:slug" $ do
    curUser <- requireUser
    slug <- param "slug"
    stopIfError articleErrorHandler $ deleteArticle curUser slug
    json $ asText ""


-- * Errors

articleErrorHandler :: (ScottyError e, Monad m) => ArticleError -> ActionT e m ()
articleErrorHandler err = case err of
  ArticleErrorNotFound _ -> do
    status status404
    json err
  ArticleErrorNotAllowed _ -> do
    status status403
    json err

-- * Utils

mayParam :: (ScottyError e, Monad m) => LText -> ActionT e m (Maybe Text)
mayParam name = (Just <$> param name) `rescue` const (return Nothing)
  
parseArticleFilter :: (ScottyError e, Monad m) => ActionT e m ArticleFilter
parseArticleFilter = ArticleFilter <$> mayParam "tag" <*> mayParam "author" <*> mayParam "favorited"

-- * Forms
    
createArticleForm :: (Monad m) => DF.Form [Text] m CreateArticle
createArticleForm = CreateArticle <$> "title" .: DF.text Nothing
                                  <*> "description" .: DF.text Nothing
                                  <*> "body" .: DF.text Nothing
                                  <*> "tagList" .: DF.listOf (const $ DF.text Nothing) Nothing
                                  
updateArticleForm :: (Monad m) => DF.Form [Text] m UpdateArticle
updateArticleForm = UpdateArticle <$> "title" .: DF.optionalText Nothing
                                  <*> "description" .: DF.optionalText Nothing
                                  <*> "body" .: DF.optionalText Nothing
