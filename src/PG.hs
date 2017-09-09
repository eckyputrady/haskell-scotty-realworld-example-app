module PG where

import ClassyPrelude
import Data.Has
import Data.Pool
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
import System.Environment

type PG r m = (MonadReader r m, Has Connection r, MonadIO m, MonadCatch m)
    
acquirePool :: IO (Pool Connection)
acquirePool = do
  envUrl <- lookupEnv "DATABASE_URL"
  let pgUrl = fromString $ fromMaybe "postgresql://localhost/realworld" envUrl
  createPool (connectPostgreSQL pgUrl) close 1 10 10

migrateDb :: Pool Connection -> IO ()
migrateDb pool = withResource pool $ \conn -> do
  withTransaction conn $ runMigration (ctx conn)
  return ()
  where
    ctx = MigrationContext cmd False
    cmd = MigrationCommands [ MigrationInitialization, MigrationDirectory "postgresql" ]