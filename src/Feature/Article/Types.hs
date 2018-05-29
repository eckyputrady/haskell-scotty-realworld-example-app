module Feature.Article.Types where

import ClassyPrelude
import Feature.User.Types
import Data.Aeson.TH
import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.FromRow

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

newtype ArticleWrapper a = ArticleWrapper { articleWrapperArticle :: a } deriving (Eq, Show)
data ArticlesWrapper a = ArticlesWrapper { articlesWrapperArticles :: [a], articlesWrapperArticlesCount :: Int } deriving (Eq, Show)

-- * Instances

$(concat <$> 
  mapM (\name -> 
    let lowerCaseFirst (y:ys) = toLower [y] <> ys 
        lowerCaseFirst "" = ""
        structName = fromMaybe "" . lastMay . splitElem '.' . show $ name
    in deriveJSON defaultOptions{fieldLabelModifier = lowerCaseFirst . drop (length structName)} name)
  [ ''ArticleFilter
  , ''Article
  , ''CreateArticle
  , ''UpdateArticle
  , ''ArticleError
  , ''ArticleWrapper
  , ''ArticlesWrapper
  ])

instance FromRow Article where
  fromRow = Article 
    <$> field
    <*> field
    <*> field
    <*> field
    <*> (fromPGArray <$> field)
    <*> field
    <*> field
    <*> field
    <*> field
    <*> fromRow