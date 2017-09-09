module Struct where

import ClassyPrelude
import Data.Aeson.TH


-- * Misc

data Pagination = Pagination
  { paginationLimit :: Int
  , paginationOffset :: Int
  } deriving (Eq, Show)

type InputViolations = Map Text [Text]



-- * Auth, users & profiles

type Username = Text
type Password = Text
type Email = Text
type Token = Text
type UserId = Integer

type CurrentUser = (Token, UserId)

data Auth = Auth
  { authEmail :: Email
  , authPassword :: Password
  } deriving (Eq, Show)

data Register = Register
  { registerUsername :: Username
  , registerEmail :: Email
  , registerPassword :: Text
  } deriving (Show)

data UpdateUser = UpdateUser
  { updateUserEmail :: Maybe Email
  , updateUserUsername :: Maybe Username
  , updateUserPassword :: Maybe Password
  , updateUserImage :: Maybe Text
  , updateUserBio :: Maybe Text
  } deriving (Eq, Show)

data User = User
  { userEmail :: Email
  , userToken :: Token
  , userUsername :: Username
  , userBio :: Text
  , userImage :: Text
  } deriving (Eq, Show)

data Profile = Profile
  { profileUsername :: Username
  , profileBio :: Text
  , profileImage :: Text
  , profileFollowing :: Bool
  } deriving (Eq, Show)

data UserError
  = UserErrorBadAuth Auth
  | UserErrorNotFound Username
  | UserErrorNameTaken Username
  | UserErrorEmailTaken Email
  deriving (Eq, Show)

data TokenError
  = TokenErrorUserIdNotFound
  | TokenErrorNotFound
  | TokenErrorExpired
  | TokenErrorMalformed String
  deriving (Eq, Show)


-- * Articles

type Slug = Text

type Tag = Text

data ArticleFilter = ArticleFilter
  { articleFilterTag :: Maybe Text
  , articleFilterAuthor :: Maybe Text
  , articleFilterFavoritedBy :: Maybe Text
  } deriving (Eq, Show)

data Article = Article
  { articleSlug :: Slug
  , articleTitle :: Text
  , articleDescription :: Text
  , articleBody :: Text
  , articleTagList :: [Tag]
  , articleCreatedAt :: UTCTime
  , articleUpdatedAt :: UTCTime
  , articleFavorited :: Bool
  , articleFavoritesCount :: Int
  , articleAuthor :: Profile
  } deriving (Eq, Show)

data CreateArticle = CreateArticle
  { createArticleTitle :: Text
  , createArticleDescription :: Text
  , createArticleBody :: Text
  , createArticleTagList :: [Tag]
  } deriving (Eq, Show)

data UpdateArticle = UpdateArticle
  { updateArticleTitle :: Maybe Text
  , updateArticleDescription :: Maybe Text
  , updateArticleBody :: Maybe Text
  } deriving (Eq, Show)

data ArticleError
  = ArticleErrorNotFound Slug
  | ArticleErrorNotAllowed Slug
  deriving (Eq, Show)



-- * Comments

type CommentId = Integer

data Comment = Comment
  { commentId :: CommentId
  , commentCreatedAt :: UTCTime
  , commentUpdatedAt :: UTCTime
  , commentBody :: Text
  , commentAuthor :: Profile
  } deriving (Eq, Show)
  
newtype CreateComment = CreateComment
  { createCommentBody :: Text
  } deriving (Eq, Show)

data CommentError
  = CommentErrorNotFound CommentId
  | CommentErrorSlugNotFound Slug
  | CommentErrorNotAllowed CommentId
  deriving (Eq, Show)



-- * Wrappers

newtype UserWrapper a = UserWrapper { userWrapperUser :: a } deriving (Eq, Show)
newtype ProfileWrapper a = ProfileWrapper { profileWrapperProfile :: a } deriving (Eq, Show)
newtype CommentWrapper a = CommentWrapper { commentWrapperComment :: a } deriving (Eq, Show)
newtype CommentsWrapper a = CommentsWrapper { commentsWrapperComments :: [a] } deriving (Eq, Show)
newtype TagsWrapper a = TagsWrapper { tagsWrapperTags :: a } deriving (Eq, Show)
newtype ErrorsWrapper a = ErrorsWrapper { errorsWrapperErrors :: a } deriving (Eq, Show)
newtype ArticleWrapper a = ArticleWrapper { articleWrapperArticle :: a } deriving (Eq, Show)
data ArticlesWrapper a = ArticlesWrapper { articlesWrapperArticles :: [a], articlesWrapperArticlesCount :: Int } deriving (Eq, Show)


-- * JSON SerDe

$(concat <$> 
  mapM (\name -> 
    let lowerCaseFirst (y:ys) = toLower [y] <> ys 
        lowerCaseFirst "" = ""
        structName = fromMaybe "" . lastMay . splitElem '.' . show $ name
    in deriveJSON defaultOptions{fieldLabelModifier = lowerCaseFirst . drop (length structName)} name)
  [ ''User
  , ''Profile
  , ''Article
  , ''Comment
  , ''UserError
  , ''TokenError
  , ''ArticleError
  , ''CommentError
  , ''UserWrapper
  , ''ProfileWrapper
  , ''ArticleWrapper
  , ''ArticlesWrapper
  , ''CommentWrapper
  , ''CommentsWrapper
  , ''TagsWrapper
  , ''ErrorsWrapper
  , ''Auth
  , ''Register
  , ''UpdateUser
  , ''CreateArticle
  , ''UpdateArticle
  ])