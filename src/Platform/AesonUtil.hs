module Platform.AesonUtil where

import ClassyPrelude
import Data.Aeson.TH
import Language.Haskell.TH.Syntax

commonJSONDeriveMany :: [Name] -> Q [Dec]
commonJSONDeriveMany names =
  concat <$> mapM commonJSONDerive names

commonJSONDerive :: Name -> Q [Dec]
commonJSONDerive name =
  let lowerCaseFirst (y:ys) = toLower [y] <> ys 
      lowerCaseFirst "" = ""
      structName = fromMaybe "" . lastMay . splitElem '.' . show $ name
  in deriveJSON defaultOptions{fieldLabelModifier = lowerCaseFirst . drop (length structName)} name
