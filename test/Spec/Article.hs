module Spec.Article (spec) where

import ClassyPrelude
import Struct
import Test.Hspec
import qualified RealWorldClient as RW
import Text.StringRandom

import Spec.Common

spec :: Spec
spec = do
  articleSpec
  favoriteSpec
  tagSpec


articleSpec :: Spec
articleSpec =
  describe "articles" $ do

    describe "create articles" $ do

      it "should require valid token" $
        runClient (RW.createArticle "invalidToken" $ CreateArticle "" "" "" [])
          `shouldReturn` Left (RW.ErrUnauthorized $ TokenErrorMalformed "BadCrypto")

      it "should create article successfully" $ do
        Right user <- registerRandomUser
        Right article <- createRandomArticle user []
        Right article' <- runClient $ RW.getArticle (Just $ userToken user) (articleSlug article)
        article `shouldBe` article'

    describe "update articles" $ do

      it "should require valid token" $
        runClient (RW.updateArticle "invalidToken" "invalidSlug" $ UpdateArticle Nothing Nothing Nothing)
          `shouldReturn` Left (RW.ErrUnauthorized $ TokenErrorMalformed "BadCrypto")

      it "should reject updating non-existent articles" $ do
        Right user <- registerRandomUser
        runClient (RW.updateArticle (userToken user) "invalidSlug" $ UpdateArticle Nothing Nothing Nothing)
          `shouldReturn` Left (RW.ErrApp $ ArticleErrorNotFound "invalidSlug")

      it "should reject updating articles who is not owned by the user" $ do
        Right user1 <- registerRandomUser
        Right user2 <- registerRandomUser
        Right article <- createRandomArticle user2 []
        runClient (RW.updateArticle (userToken user1) (articleSlug article) $ UpdateArticle Nothing Nothing Nothing)
          `shouldReturn` Left (RW.ErrApp $ ArticleErrorNotAllowed (articleSlug article))

      it "should update title successfully" $ do
        Right user <- registerRandomUser
        Right article <- createRandomArticle user []
        newTitle <- stringRandomIO "[a-zA-Z0-9 ]{5}"
        Right article' <- runClient (RW.updateArticle (userToken user) (articleSlug article) $ UpdateArticle (Just newTitle) Nothing Nothing)
        let expected = article { articleSlug = articleSlug article', articleTitle = newTitle }
        article' `shouldBe` expected

      it "should update desc, body successfully" $ do
        Right user <- registerRandomUser
        Right article <- createRandomArticle user []
        newDesc <- stringRandomIO "[a-zA-Z0-9 ]{5}"
        newBody <- stringRandomIO "[a-zA-Z0-9 ]{5}"
        Right article' <- runClient (RW.updateArticle (userToken user) (articleSlug article) $ UpdateArticle Nothing (Just newDesc) (Just newBody))
        let expected = article { articleSlug = articleSlug article', articleDescription = newDesc, articleBody = newBody }
        article' `shouldBe` expected

    describe "delete articles" $ do

      it "should require valid token" $
        runClient (RW.deleteArticle "invalidToken" "invalidSlug")
          `shouldReturn` Left (RW.ErrUnauthorized $ TokenErrorMalformed "BadCrypto")

      it "should reject deleting non-existent article" $ do
        Right user <- registerRandomUser
        runClient (RW.deleteArticle (userToken user) "invalidSlug")
          `shouldReturn` Left (RW.ErrApp $ ArticleErrorNotFound "invalidSlug")

      it "should reject deleting articles who is not owned by the user" $ do
        Right user1 <- registerRandomUser
        Right user2 <- registerRandomUser
        Right article <- createRandomArticle user2 []
        runClient (RW.deleteArticle (userToken user1) (articleSlug article))
          `shouldReturn` Left (RW.ErrApp $ ArticleErrorNotAllowed (articleSlug article))

      it "should delete articles successfully" $ do
        Right user <- registerRandomUser
        Right article <- createRandomArticle user []
        runClient (RW.deleteArticle (userToken user) (articleSlug article))
          `shouldReturn` Right ()
        runClient (RW.getArticle (Just $ userToken user) (articleSlug article))
          `shouldReturn` Left (RW.ErrApp $ ArticleErrorNotFound (articleSlug article))

    describe "get articles" $ do

      it "should able to filter by tag and author" $ do
        Right user1 <- registerRandomUser
        Right user2 <- registerRandomUser
        tags@[tag] <- randomTags 1
        Right article1 <- createRandomArticle user2 tags
        Right article2 <- createRandomArticle user2 tags
        Right article3 <- createRandomArticle user1 tags
        let filter = ArticleFilter (Just tag) (Just $ userUsername user2) Nothing
        let pagination = Pagination 100 0
        runClient (RW.getArticles Nothing filter pagination)
          `shouldReturn` Right [article2, article1]

      it "should able to filter by favoritedBy" $ do
        Right user <- registerRandomUser
        Right article <- createRandomArticle user []
        let token = userToken user
        let slug = articleSlug article
        runClient (RW.favoriteArticle token slug)
        let expected = article { articleFavorited = True, articleFavoritesCount = 1 }
        let filter = ArticleFilter Nothing Nothing (Just $ userUsername user)
        let pagination = Pagination 100 0
        runClient (RW.getArticles (Just token) filter pagination)
          `shouldReturn` Right [expected]

    describe "get feed" $ do

      it "should require valid token" $
        runClient (RW.getFeed "invalidToken" (Pagination 100 0))
          `shouldReturn` Left (RW.ErrUnauthorized $ TokenErrorMalformed "BadCrypto")

      it "should return empty if the user doesn't follow anyone" $ do
        Right user <- registerRandomUser
        runClient (RW.getFeed (userToken user) (Pagination 100 0))
          `shouldReturn` Right []

      it "should get feed successfully" $ do
        Right user1 <- registerRandomUser
        Right user2 <- registerRandomUser
        Right article <- createRandomArticle user2 []
        runClient (RW.followUser (userToken user1) (userUsername user2))
        let expected = article { articleAuthor = (articleAuthor article) { profileFollowing = True } }
        runClient (RW.getFeed (userToken user1) (Pagination 100 0))
          `shouldReturn` Right [expected]

    describe "get article" $

      it "should return err if the article is not found" $
        runClient (RW.getArticle Nothing "invalidSlug")
          `shouldReturn` Left (RW.ErrApp $ ArticleErrorNotFound "invalidSlug")

    describe "pagination" $ do

      it "should return first page for < 0 pagination offset" $ do
        Right user <- registerRandomUser
        createRandomArticle user []
        let filter = ArticleFilter Nothing (Just $ userUsername user) Nothing
        let pagination = Pagination 100 (-1)
        actual <- runClient (RW.getArticles Nothing filter pagination)
        expected <- runClient (RW.getArticles Nothing filter $ Pagination 100 0)
        actual `shouldBe` expected

      it "should return empty for > out of bounds pagination offset" $ do
        Right user <- registerRandomUser
        createRandomArticle user []
        let filter = ArticleFilter Nothing Nothing Nothing
        let pagination = Pagination 100 1000000 -- arbitrarily big offset
        runClient (RW.getArticles Nothing filter pagination)
          `shouldReturn` Right []

      it "should return empty for <= 0 pagination limit" $ do
        Right user <- registerRandomUser
        createRandomArticle user []
        let filter = ArticleFilter Nothing Nothing Nothing
        let pagination = Pagination (-1) 0
        runClient (RW.getArticles Nothing filter pagination)
          `shouldReturn` Right []

      it "should correctly break the pages based on limit" $ do
        Right user <- registerRandomUser
        tags@[tag] <- randomTags 1 
        Right article1 <- createRandomArticle user tags
        Right article2 <- createRandomArticle user tags
        let filter = ArticleFilter (Just tag) Nothing Nothing
        runClient (RW.getArticles Nothing filter (Pagination 1 0))
          `shouldReturn` Right [article2]
        runClient (RW.getArticles Nothing filter (Pagination 1 1))
          `shouldReturn` Right [article1]

          
favoriteSpec :: Spec
favoriteSpec =
  describe "favorites" $ do

    describe "favorite" $ do

      it "should require valid token" $
        runClient (RW.favoriteArticle "invalidToken" "invalidSlug")
          `shouldReturn` Left (RW.ErrUnauthorized $ TokenErrorMalformed "BadCrypto")
          
      it "should throw err if trying to favorite non-existent article" $ do
        Right user <- registerRandomUser
        let token = userToken user
        runClient (RW.favoriteArticle token "invalidSlug")
          `shouldReturn` Left (RW.ErrApp $ ArticleErrorNotFound "invalidSlug")

      it "should favorite successfully & idempotently" $ do
        Right user <- registerRandomUser
        Right article <- createRandomArticle user []
        let token = userToken user
        let slug = articleSlug article
        let expected = article { articleFavorited = True, articleFavoritesCount = 1 }
        runClient (RW.favoriteArticle token slug)
          `shouldReturn` Right expected
        runClient (RW.favoriteArticle token slug)
          `shouldReturn` Right expected

    describe "unfavorite" $ do

      it "should require valid token" $
        runClient (RW.favoriteArticle "invalidToken" "invalidSlug")
          `shouldReturn` Left (RW.ErrUnauthorized $ TokenErrorMalformed "BadCrypto")
          
      it "should throw err if trying to unfavorite non-existent article" $ do
        Right user <- registerRandomUser
        let token = userToken user
        runClient (RW.unfavoriteArticle token "invalidSlug")
          `shouldReturn` Left (RW.ErrApp $ ArticleErrorNotFound "invalidSlug")

      it "should unfavorite successfully & idempotently" $ do
        Right user <- registerRandomUser
        Right article <- createRandomArticle user []
        let token = userToken user
        let slug = articleSlug article
        runClient (RW.favoriteArticle token slug)
        runClient (RW.unfavoriteArticle token slug)
          `shouldReturn` Right article
        runClient (RW.unfavoriteArticle token slug)
          `shouldReturn` Right article


tagSpec :: Spec
tagSpec =
  describe "tags" $
    it "should get tags successfully" $ do
      Right user <- registerRandomUser
      tags <- forM [1..10] $ const (stringRandomIO "[a-zA-Z]{10}")
      createRandomArticle user tags
      Right allTags <- runClient RW.getTags
      (setFromList tags `union` allTags) `shouldBe` allTags