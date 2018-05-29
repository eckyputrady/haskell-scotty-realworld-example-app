module Feature.Comment.Service where

import ClassyPrelude
import Control.Monad.Except
import Feature.Comment.Types
import Feature.Auth.Types

class (Monad m) => CommentRepo m where
  addCommentToSlug :: UserId -> Slug -> Text -> m CommentId
  delCommentById :: CommentId -> m ()
  findComments :: Maybe UserId -> Slug -> Maybe CommentId -> m [Comment]
  isCommentOwnedBy :: UserId -> CommentId -> m Bool
  isCommentExist :: CommentId -> m Bool
  isSlugExist :: Slug -> m Bool

addComment :: (CommentRepo m) => CurrentUser -> Slug -> Text -> m (Either CommentError Comment)
addComment curUser@(_, curUserId) slug comment = runExceptT $ do
  cId <- lift $ addCommentToSlug curUserId slug comment
  comments <- ExceptT $ getComments' (Just curUser) slug (Just cId)
  case comments of
    [a] -> return a
    _ -> throwError $ CommentErrorNotFound cId

delComment :: (CommentRepo m) => CurrentUser -> Slug -> CommentId -> m (Either CommentError ())
delComment (_, curUserId) slug cId = runExceptT $ do
  ExceptT $ validateArticleExists slug
  ExceptT $ validateCommentExists cId
  ExceptT $ validateCommentOwnedBy curUserId cId
  lift $ delCommentById cId

getComments :: (CommentRepo m) => Maybe CurrentUser -> Slug -> m (Either CommentError [Comment])
getComments mayCurUser slug = getComments' mayCurUser slug Nothing

getComments' :: (CommentRepo m) => Maybe CurrentUser -> Slug -> Maybe CommentId -> m (Either CommentError [Comment])
getComments' mayCurUser slug mayCommentId = runExceptT $ do
  ExceptT $ validateArticleExists slug
  lift $ findComments (snd <$> mayCurUser) slug mayCommentId

validateArticleExists :: (CommentRepo m) => Slug -> m (Either CommentError ())
validateArticleExists slug = runExceptT $ do
  result <- lift $ isSlugExist slug
  unless result $ throwError (CommentErrorSlugNotFound slug)

validateCommentOwnedBy :: (CommentRepo m) => UserId -> CommentId -> m (Either CommentError ())
validateCommentOwnedBy uId cId = runExceptT $ do
  result <- lift $ isCommentOwnedBy uId cId
  unless result $ throwError (CommentErrorNotAllowed cId)

validateCommentExists :: (CommentRepo m) => CommentId -> m (Either CommentError ())
validateCommentExists cId = runExceptT $ do
  result <- lift $ isCommentExist cId
  unless result $ throwError (CommentErrorNotFound cId)