module Feature.Comment.Types where

import ClassyPrelude
import Feature.User.Types
import Data.Aeson.TH
import Database.PostgreSQL.Simple.FromRow

type CommentId = Integer

type Slug = Text

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

newtype CommentWrapper a = CommentWrapper { commentWrapperComment :: a } deriving (Eq, Show)
newtype CommentsWrapper a = CommentsWrapper { commentsWrapperComments :: [a] } deriving (Eq, Show)

$(concat <$> 
  mapM (\name -> 
    let lowerCaseFirst (y:ys) = toLower [y] <> ys 
        lowerCaseFirst "" = ""
        structName = fromMaybe "" . lastMay . splitElem '.' . show $ name
    in deriveJSON defaultOptions{fieldLabelModifier = lowerCaseFirst . drop (length structName)} name)
  [ ''Comment
  , ''CommentError
  , ''CommentWrapper
  , ''CommentsWrapper
  ])

instance FromRow Comment where
  fromRow = Comment
    <$> field 
    <*> field 
    <*> field 
    <*> field 
    <*> fromRow