module Lib
    ( main
    ) where

import ClassyPrelude
import Control.Monad.Logger
import Data.Pool
import PG
import JWT

import qualified Web

main :: IO ()
main = do
  -- acquire resources
  pgPool <- acquirePool
  migrateDb pgPool
  jwks <- acquireJwks
  jwtExpirationSecs <- acquireJWTExpirationSecs
  -- start the app
  let runner app = withResource pgPool $ \conn -> runStdoutLoggingT . runReaderT app $ (conn, jwks, jwtExpirationSecs)
  Web.main runner