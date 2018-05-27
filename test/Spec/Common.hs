module Spec.Common where
    
import ClassyPrelude
import Core.Types
import qualified Adapter.HTTP.Client as RW
import Text.StringRandom

randomRegisterParam :: IO Register
randomRegisterParam = do
  name <- stringRandomIO "[a-zA-Z0-9]{10}"
  mail <- stringRandomIO "[a-zA-Z0-9]{10}@test\\.com"
  return $ Register name mail "test1234"

registerRandomUser :: IO (Either (RW.Err UserError) User)
registerRandomUser = do
  param <- randomRegisterParam
  runClient . RW.register $ param

randomTags :: Int -> IO [Text]
randomTags n =
  mapM (const $ stringRandomIO "[a-zA-z0-9]{5}") [1..n] 

randomCreateArticleParam :: [Tag] -> IO CreateArticle
randomCreateArticleParam tags = do
  title <- stringRandomIO "[a-zA-z0-9 ]{10}"
  desc <- stringRandomIO "[a-zA-z0-9 ]{10}"
  body <- stringRandomIO "[a-zA-z0-9 ]{10}"
  return $ CreateArticle title desc body tags

createRandomArticle :: User -> [Tag] -> IO (Either (RW.Err ArticleError) Article)
createRandomArticle user tags = do
  let token = userToken user
  param <- randomCreateArticleParam tags
  runClient (RW.createArticle token param)

runClient :: ReaderT RW.RWBaseUrl m a -> m a
runClient = flip runReaderT (RW.RWBaseUrl "http://127.0.0.1:3000/api")