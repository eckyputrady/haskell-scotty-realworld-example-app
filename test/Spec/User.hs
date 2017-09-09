module Spec.User (spec) where

import ClassyPrelude
import Struct
import Test.Hspec
import qualified RealWorldClient as RW
import Text.StringRandom

import Spec.Common

spec :: Spec
spec = do
  userSpec
  profileSpec

  
userSpec :: Spec
userSpec =
  describe "user" $ do

    describe "registration" $ do

      it "should register successfully" $ do
        registerRandomUser
        return ()

      it "should reject duplicate email" $ do
        Right user <- registerRandomUser
        param <- randomRegisterParam
        let duplicatedEmail = userEmail user
        let param' = param { registerEmail = duplicatedEmail }
        runClient (RW.register param')
          `shouldReturn` Left (RW.ErrApp (UserErrorEmailTaken duplicatedEmail))

      it "should reject duplicate name" $ do
        Right user <- registerRandomUser
        param <- randomRegisterParam
        let duplicatedName = userUsername user
        let param' = param { registerUsername = duplicatedName }
        runClient (RW.register param')
          `shouldReturn` Left (RW.ErrApp (UserErrorNameTaken duplicatedName))

      describe "input validation" $ do

        it "should reject too short username & too short password" $ do
          let param = Register "u" "test@test.com" "p"
          runClient (RW.register param)
            `shouldReturn` (Left $ RW.ErrInvalidInput $ mapFromList [("password", ["Minimum length is 5"]), ("username", ["Minimum length is 3"])])

        it "should reject non-alphanumeric username" $ do
          let param = Register "u asdasda!!" "test@test.com" "pasdasdasda"
          runClient (RW.register param)
            `shouldReturn` (Left $ RW.ErrInvalidInput $ mapFromList [("username", ["Should be alphanumeric"])])

        it "should reject invalid email" $ do
          let param = Register "uasodhasasd" "test invalid@test.com" "aasidhoasihdp"
          runClient (RW.register param)
            `shouldReturn` (Left $ RW.ErrInvalidInput $ mapFromList [("email", ["Not a valid email"])])

    describe "login" $ do

      it "should login successfully" $ do
        Right user <- registerRandomUser
        Right user' <- runClient $ RW.login $ Auth (userEmail user) "test1234"
        user `shouldBe` user'

      it "should expire login eventually" $ do
        Right user <- registerRandomUser
        threadDelay 5000000 -- wait 5 secs as expiration is 4 secs
        let token = userToken user
        runClient (RW.getUser token)
          `shouldReturn` Left (RW.ErrUnauthorized TokenErrorExpired)

      it "should reject invalid login" $ do
        let auth = Auth "invalidLogin@test.com" "test1234"
        runClient (RW.login auth) `shouldReturn` Left (RW.ErrApp (UserErrorBadAuth auth))

      describe "input validation" $

        it "should reject too invalid email & too short password" $ do
          let param = Auth "test invalid@test.com" "p"
          runClient (RW.login param)
            `shouldReturn` (Left $ RW.ErrInvalidInput $ mapFromList [("password", ["Minimum length is 5"]), ("email", ["Not a valid email"])])
    
    describe "update" $ do

      it "should require valid token" $ do
        result <- runClient $ RW.updateUser "invalidToken" $ UpdateUser Nothing Nothing Nothing Nothing Nothing
        result `shouldBe` Left (RW.ErrUnauthorized $ TokenErrorMalformed "BadCrypto")

      it "should reject duplicate email" $ do
        Right user1 <- registerRandomUser
        Right user2 <- registerRandomUser
        let token = userToken user1
        let duplicate = userEmail user2
        runClient (RW.updateUser token $ UpdateUser (Just duplicate) Nothing Nothing Nothing Nothing)
          `shouldReturn` Left (RW.ErrApp (UserErrorEmailTaken duplicate))

      it "should reject duplicate name" $ do
        Right user1 <- registerRandomUser
        Right user2 <- registerRandomUser
        let token = userToken user1
        let duplicate = userUsername user2
        runClient (RW.updateUser token $ UpdateUser Nothing (Just duplicate) Nothing Nothing Nothing)
          `shouldReturn` Left (RW.ErrApp (UserErrorNameTaken duplicate))

      it "should update email, name, pass successfully" $ do
        Right user <- registerRandomUser
        let token = userToken user
        Register name mail pass <- randomRegisterParam
        let expected = user { userUsername = name, userEmail = mail }
        runClient (RW.updateUser token $ UpdateUser (Just mail) (Just name) (Just pass) Nothing Nothing)
          `shouldReturn` Right expected
        
      it "should update image, bio successfully" $ do
        Right user <- registerRandomUser
        let token = userToken user
        Register name mail pass <- randomRegisterParam
        let expected = user { userBio = "bio", userImage = "image" }
        runClient (RW.updateUser token $ UpdateUser Nothing Nothing Nothing (Just "image") (Just "bio"))
          `shouldReturn` Right expected

      describe "input validation" $

        it "should reject too invalid email, too short username & too short password" $ do
          Right user <- registerRandomUser
          let token = userToken user
          let param = UpdateUser (Just "test invalid@test.com") (Just "u") (Just "p") Nothing Nothing
          runClient (RW.updateUser token param)
            `shouldReturn` (Left $ RW.ErrInvalidInput $ mapFromList [("username", ["Minimum length is 3"]), ("password", ["Minimum length is 5"]), ("email", ["Not a valid email"])])

    describe "get user" $ do

      it "should require valid token" $
        runClient (RW.getUser "invalidToken")
          `shouldReturn` Left (RW.ErrUnauthorized $ TokenErrorMalformed "BadCrypto")

      it "should return correct user" $ do
        Right user <- registerRandomUser
        let token = userToken user
        runClient (RW.getUser token) `shouldReturn` Right user


profileSpec :: Spec
profileSpec =
  describe "profile" $ do

    describe "get" $ do

      it "should return correct profile" $ do
        Right user <- registerRandomUser
        let name = userUsername user
        runClient (RW.getProfile Nothing name)
          `shouldReturn` Right (Profile (userUsername user) (userBio user) (userImage user) False)

      it "should throw error if not found" $ do
        invalidUsername <- stringRandomIO "[a-zA-Z0-9]{10}"
        runClient (RW.getProfile Nothing invalidUsername)
          `shouldReturn` Left (RW.ErrApp (UserErrorNotFound invalidUsername))

    describe "follow" $ do

      it "should require valid token" $ do
        invalidUsername <- stringRandomIO "[a-zA-Z0-9]{10}"
        runClient (RW.followUser "invalidToken" invalidUsername)
          `shouldReturn` Left (RW.ErrUnauthorized $ TokenErrorMalformed "BadCrypto")

      it "should throw error if not found" $ do
        Right user <- registerRandomUser
        let token = userToken user
        invalidUsername <- stringRandomIO "[a-zA-Z0-9]{10}"
        runClient (RW.followUser token invalidUsername)
          `shouldReturn` Left(RW.ErrApp (UserErrorNotFound invalidUsername))

      it "should follow successfully and idempotent" $ do
        Right user1 <- registerRandomUser
        Right user2 <- registerRandomUser
        let token = userToken user1
        let username = userUsername user2
        result <- runClient $ do
          RW.followUser token username
          RW.followUser token username
          RW.getProfile (Just token) username
        result `shouldBe` Right (Profile (userUsername user2) (userBio user2) (userImage user2) True)

    describe "unfollow" $ do

      it "should require valid token" $ do
        invalidUsername <- stringRandomIO "[a-zA-Z0-9]{10}"
        runClient (RW.unfollowUser "invalidToken" invalidUsername)
          `shouldReturn` Left (RW.ErrUnauthorized $ TokenErrorMalformed "BadCrypto")

      it "should throw error if not found" $ do
        Right user <- registerRandomUser
        let token = userToken user
        invalidUsername <- stringRandomIO "[a-zA-Z0-9]{10}"
        runClient (RW.unfollowUser token invalidUsername)
          `shouldReturn` Left (RW.ErrApp $ UserErrorNotFound invalidUsername)

      it "should unfollow successfully & idempotent" $ do
        Right user1 <- registerRandomUser
        Right user2 <- registerRandomUser
        let token = userToken user1
        let username = userUsername user2
        result <- runClient $ do
          RW.followUser token username
          RW.unfollowUser token username
          RW.unfollowUser token username
          RW.getProfile (Just token) username
        result `shouldBe` Right (Profile (userUsername user2) (userBio user2) (userImage user2) False)