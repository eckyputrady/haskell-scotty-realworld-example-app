module Lib
    ( main
    ) where

import ClassyPrelude
import Crypto.Random.Types (MonadRandom, getRandomBytes)
import Data.Pool
import PG
import JWT
import qualified Web

newtype AppT r m a = AppT
  { unAppT :: ReaderT r m a
  } deriving  ( Applicative, Functor, Monad, MonadTrans
              , MonadIO, MonadThrow, MonadCatch, MonadReader r)

instance (MonadRandom m) => MonadRandom (AppT r m) where
  getRandomBytes = lift . getRandomBytes

main :: IO ()
main = do
  -- acquire resources
  pgPool <- acquirePool
  migrateDb pgPool
  jwks <- acquireJwks
  jwtExpirationSecs <- acquireJWTExpirationSecs
  -- start the app
  let runner app = withResource pgPool $ \conn -> flip runReaderT (conn, jwks, jwtExpirationSecs) $ unAppT app
  Web.main runner