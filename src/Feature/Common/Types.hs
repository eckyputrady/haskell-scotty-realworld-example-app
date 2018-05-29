module Feature.Common.Types where

import ClassyPrelude
import Data.Aeson.TH

data Pagination = Pagination
  { paginationLimit :: Int
  , paginationOffset :: Int
  } deriving (Eq, Show)

type InputViolations = Map Text [Text]

newtype ErrorsWrapper a = ErrorsWrapper { errorsWrapperErrors :: a } deriving (Eq, Show)

$(concat <$> 
  mapM (\name -> 
    let lowerCaseFirst (y:ys) = toLower [y] <> ys 
        lowerCaseFirst "" = ""
        structName = fromMaybe "" . lastMay . splitElem '.' . show $ name
    in deriveJSON defaultOptions{fieldLabelModifier = lowerCaseFirst . drop (length structName)} name)
  [ ''ErrorsWrapper
  ])