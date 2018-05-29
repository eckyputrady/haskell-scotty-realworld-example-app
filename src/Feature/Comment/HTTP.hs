module Feature.Comment.HTTP
      ( routes
      , Service(..)
      ) where

import ClassyPrelude hiding (delete)

import Feature.Comment.Types
import Feature.Auth.Types
import Feature.Common.HTTP
import qualified Feature.Auth.HTTP as Auth
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import qualified Text.Digestive.Form as DF
import Text.Digestive.Form ((.:))

class Monad m => Service m where
  addComment :: CurrentUser -> Slug -> Text -> m (Either CommentError Comment)
  delComment :: CurrentUser -> Slug -> CommentId -> m (Either CommentError ())
  getComments :: Maybe CurrentUser -> Slug -> m (Either CommentError [Comment])

routes :: (Auth.Service m, Service m, MonadIO m) => ScottyT LText m ()
routes = do

  post "/api/articles/:slug/comments" $ do
    curUser <- Auth.requireUser
    slug <- param "slug"
    req <- parseJsonBody ("comment" .: "body" .: DF.text Nothing)
    result <- stopIfError commentErrorHandler $ addComment curUser slug req
    json $ CommentWrapper result

  delete "/api/articles/:slug/comments/:id" $ do
    curUser <- Auth.requireUser
    slug <- param "slug"
    cId <- param "id"
    stopIfError commentErrorHandler $ delComment curUser slug cId
    json $ asText ""
  
  get "/api/articles/:slug/comments" $ do
    curUser <- Auth.optionalUser
    slug <- param "slug"
    result <- stopIfError commentErrorHandler $ getComments curUser slug
    json $ CommentsWrapper result

commentErrorHandler :: (ScottyError e, Monad m) => CommentError -> ActionT e m ()
commentErrorHandler err = case err of
  CommentErrorNotFound _ -> do
    status status404
    json err
  CommentErrorSlugNotFound _ -> do
    status status404
    json err
  CommentErrorNotAllowed _ -> do
    status status403
    json err
