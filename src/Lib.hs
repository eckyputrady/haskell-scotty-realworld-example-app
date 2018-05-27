module Lib
    ( main
    ) where

import ClassyPrelude
import Crypto.Random.Types (MonadRandom, getRandomBytes)
import Data.Pool
import qualified Adapter.PG as PG
import qualified Database.PostgreSQL.Simple as PG
import qualified Adapter.JWT as JWT
import qualified Jose.Jwk as JWT
import qualified Adapter.HTTP.API as API
import Core.Types

main :: IO ()
main = do
  -- acquire resources
  pgPool <- PG.acquirePool
  PG.migrateDb pgPool
  jwks <- JWT.acquireJwks
  jwtExpirationSecs <- JWT.acquireJWTExpirationSecs
  -- start the app
  let runner app = withResource pgPool $ \conn -> flip runReaderT (conn, jwks, jwtExpirationSecs) $ unAppT app
  API.main runner

type Env = (PG.Connection, [JWT.Jwk], JWT.JWTExpirationSecs)

newtype AppT a = AppT
  { unAppT :: ReaderT Env IO a
  } deriving  ( Applicative, Functor, Monad
              , MonadIO, MonadUnliftIO, MonadReader Env)

instance MonadRandom AppT where
  getRandomBytes = liftIO . getRandomBytes

instance UserRepo AppT where
  findUserByAuth = PG.findUserByAuth
  findUserById = PG.findUserById
  addUser = PG.addUser
  updateUserById = PG.updateUserById

instance ProfileRepo AppT where
  findProfile = PG.findProfile
  followUserByUsername = PG.followUserByUsername
  unfollowUserByUsername = PG.unfollowUserByUsername

instance ArticleRepo AppT where
  findArticles = PG.findArticles
  addArticle = PG.addArticle
  updateArticleBySlug = PG.updateArticleBySlug
  deleteArticleBySlug = PG.deleteArticleBySlug
  favoriteArticleBySlug = PG.favoriteArticleBySlug
  unfavoriteArticleBySlug = PG.unfavoriteArticleBySlug
  isArticleOwnedBy = PG.isArticleOwnedBy
  isArticleExist = PG.isArticleExist
  
instance TagRepo AppT where
  allTags = PG.allTags

instance CommentRepo AppT where
  addCommentToSlug = PG.addCommentToSlug
  delCommentById = PG.delCommentById
  findComments = PG.findComments
  isCommentOwnedBy = PG.isCommentOwnedBy
  isCommentExist = PG.isCommentExist

instance TimeRepo AppT where
  currentTime = liftIO getCurrentTime

instance TokenRepo AppT where
  generateToken = JWT.generateToken
  resolveToken = JWT.resolveToken