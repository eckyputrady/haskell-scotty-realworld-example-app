module Spec.Comment (spec) where

import ClassyPrelude
import Test.Hspec
import Feature.Comment.Types
import Feature.User.Types
import Feature.Article.Types
import Feature.Auth.Types
import qualified Misc.Client as RW

import Spec.Common

spec :: Spec
spec = commentSpec

commentSpec :: Spec
commentSpec =
  describe "comments" $ do

    describe "add comment" $ do

      it "should require token" $
        runClient (RW.addComment "invalidToken" "invalidSlug" "comment")
          `shouldReturn` Left (RW.ErrUnauthorized $ TokenErrorMalformed "BadCrypto")

      it "should reject commenting non-existent article" $ do
        Right user <- registerRandomUser
        let token = userToken user
        runClient (RW.addComment token "invalidSlug" "comment")
          `shouldReturn` Left (RW.ErrApp $ CommentErrorSlugNotFound "invalidSlug")

      it "should add comment successfully" $ do
        Right user <- registerRandomUser
        Right article <- createRandomArticle user []
        let token = userToken user
        let slug = articleSlug article
        Right comment <- runClient (RW.addComment token slug "comment")
        runClient (RW.getComments (Just token) slug)
          `shouldReturn` Right [comment]

    describe "del comment" $ do

      it "should require token" $
        runClient (RW.delComment "invalidToken" "invalidSlug" 1)
          `shouldReturn` Left (RW.ErrUnauthorized $ TokenErrorMalformed "BadCrypto")

      it "should reject deleting comment from non-existent article" $ do
        Right user <- registerRandomUser
        let token = userToken user
        runClient (RW.delComment token "invalidSlug" 1)
          `shouldReturn` Left (RW.ErrApp $ CommentErrorSlugNotFound "invalidSlug")

      it "should throw err for not found comment" $ do
        Right user <- registerRandomUser
        Right article <- createRandomArticle user []
        let token = userToken user
        let slug = articleSlug article
        runClient (RW.delComment token slug (-1))
          `shouldReturn` Left (RW.ErrApp $ CommentErrorNotFound (-1))

      it "should reject deleting comment that is not owned by the user" $ do
        Right user1 <- registerRandomUser
        Right user2 <- registerRandomUser
        Right article <- createRandomArticle user1 []
        let token1 = userToken user1
        let token2 = userToken user2
        let slug = articleSlug article
        Right comment <- runClient (RW.addComment token2 slug "comment")
        let cId = commentId comment
        runClient (RW.delComment token1 slug cId)
          `shouldReturn` Left (RW.ErrApp $ CommentErrorNotAllowed cId)

      it "should del comment successfully" $ do
        Right user <- registerRandomUser
        Right article <- createRandomArticle user []
        let token = userToken user
        let slug = articleSlug article
        Right comment <- runClient (RW.addComment token slug "comment")
        let cId = commentId comment
        runClient (RW.delComment token slug cId)
        runClient (RW.getComments (Just token) slug)
          `shouldReturn` Right []

    describe "get comments" $

      it "should throw error for not found slug" $
        runClient (RW.getComments Nothing "invalidSlug")
          `shouldReturn` Left (RW.ErrApp $ CommentErrorSlugNotFound "invalidSlug")