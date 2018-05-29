module Feature.Auth.Types where

import ClassyPrelude

import Platform.AesonUtil

type Token = Text
type UserId = Integer
type CurrentUser = (Token, UserId)

data TokenError
  = TokenErrorUserIdNotFound
  | TokenErrorNotFound
  | TokenErrorExpired
  | TokenErrorMalformed String
  deriving (Eq, Show)

$(commonJSONDeriveMany
  [ ''TokenError
  ])