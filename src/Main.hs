module Main
    ( main
    ) where

import ClassyPrelude
import Crypto.Random.Types (MonadRandom, getRandomBytes)

import qualified Platform.PG as PG
import qualified Platform.JWT as JWT
import qualified Platform.HTTP as HTTP

import qualified Feature.Auth.HTTP as AuthHTTP
import qualified Feature.Auth.JWT as AuthJWT

import qualified Feature.User.HTTP as UserHTTP
import qualified Feature.User.JWT as UserJWT
import qualified Feature.User.PG as UserPG
import qualified Feature.User.Service as UserService

import qualified Feature.Article.HTTP as ArticleHTTP
import qualified Feature.Article.PG as ArticlePG
import qualified Feature.Article.Service as ArticleService

import qualified Feature.Comment.HTTP as CommentHTTP
import qualified Feature.Comment.PG as CommentPG
import qualified Feature.Comment.Service as CommentService

main :: IO ()
main = do
  -- acquire resources
  pgEnv <- PG.init
  jwtEnv <- JWT.init
  -- start the app
  let runner app = flip runReaderT (pgEnv, jwtEnv) $ unAppT app
  HTTP.main runner

type Env = (PG.Env, JWT.Env)

newtype AppT a = AppT
  { unAppT :: ReaderT Env IO a
  } deriving  ( Applicative, Functor, Monad
              , MonadIO, MonadReader Env)

-- configuration

instance MonadRandom AppT where
  getRandomBytes = liftIO . getRandomBytes

instance AuthHTTP.Service AppT where
  resolveToken = AuthJWT.resolveToken

instance UserHTTP.Service AppT where
  login = UserService.login
  register = UserService.register
  getUser = UserService.getUser
  updateUser = UserService.updateUser
  getProfile = UserService.getProfile
  followUser = UserService.followUser
  unfollowUser = UserService.unfollowUser
  
instance UserService.UserRepo AppT where
  findUserByAuth = UserPG.findUserByAuth
  findUserById = UserPG.findUserById
  addUser = UserPG.addUser
  updateUserById = UserPG.updateUserById

instance UserService.ProfileRepo AppT where
  findProfile = UserPG.findProfile
  followUserByUsername = UserPG.followUserByUsername
  unfollowUserByUsername = UserPG.unfollowUserByUsername

instance UserService.TokenRepo AppT where
  generateToken = UserJWT.generateToken

instance ArticleHTTP.Service AppT where
  getArticles = ArticleService.getArticles
  getFeed = ArticleService.getFeed
  getArticle = ArticleService.getArticle
  createArticle = ArticleService.createArticle
  updateArticle = ArticleService.updateArticle
  deleteArticle = ArticleService.deleteArticle
  favoriteArticle = ArticleService.favoriteArticle
  unfavoriteArticle = ArticleService.unfavoriteArticle

instance ArticleService.ArticleRepo AppT where
  findArticles = ArticlePG.findArticles
  addArticle = ArticlePG.addArticle
  updateArticleBySlug = ArticlePG.updateArticleBySlug
  deleteArticleBySlug = ArticlePG.deleteArticleBySlug
  favoriteArticleBySlug = ArticlePG.favoriteArticleBySlug
  unfavoriteArticleBySlug = ArticlePG.unfavoriteArticleBySlug
  isArticleOwnedBy = ArticlePG.isArticleOwnedBy
  isArticleExist = ArticlePG.isArticleExist

instance ArticleService.TimeRepo AppT where
  currentTime = liftIO getCurrentTime
  
instance ArticleService.TagRepo AppT where
  allTags = ArticlePG.allTags

instance CommentHTTP.Service AppT where
  addComment = CommentService.addComment
  delComment = CommentService.delComment
  getComments = CommentService.getComments

instance CommentService.CommentRepo AppT where
  addCommentToSlug = CommentPG.addCommentToSlug
  delCommentById = CommentPG.delCommentById
  findComments = CommentPG.findComments
  isCommentOwnedBy = CommentPG.isCommentOwnedBy
  isCommentExist = CommentPG.isCommentExist
  isSlugExist = ArticlePG.isArticleExist
