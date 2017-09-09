import ClassyPrelude
import Control.Monad.Except hiding (forM_)
import Struct
import Test.Hspec
import System.Environment
import Database.PostgreSQL.Simple
import qualified RealWorldClient as RW
import qualified Lib
import Text.StringRandom

import Spec.Common
import qualified Spec.User as User
import qualified Spec.Article as Article
import qualified Spec.Comment as Comment

main :: IO ()
main = withEnv . hspec . parallel $ do
  User.spec
  Article.spec
  Comment.spec

withEnv :: IO () -> IO ()
withEnv = bracket startEnv cleanEnv . const

startEnv :: IO ThreadId
startEnv = do
  execPGQuery ["drop database if exists realworld_test", "create database realworld_test"]
  setEnv "DATABASE_URL" "postgresql://127.0.0.1/realworld_test"
  setEnv "ENABLE_HTTPS" "False"
  setEnv "JWT_EXPIRATION_SECS" "4"
  tId <- fork Lib.main
  unlessM healthCheck $ do
    putStrLn "Waiting for server ..."
    threadDelay 1000000
  return tId
  where
    healthCheck = do
      result <- runClient RW.health
      return $ either (const False) id result

cleanEnv :: ThreadId -> IO ()
cleanEnv tId = do
  killThread tId
  putStrLn $ "Sever killed (" <> tshow tId <> ")"

execPGQuery :: [Query] -> IO ()
execPGQuery qrys =
  bracket acquire release execQuery
  where
    acquire = connectPostgreSQL "postgresql://127.0.0.1"
    release = close
    execQuery conn = forM_ qrys (void . execute_ conn)