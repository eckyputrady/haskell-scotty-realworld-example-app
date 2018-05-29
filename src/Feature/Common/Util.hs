module Feature.Common.Util where

import ClassyPrelude

orThrow :: Monad m => m (Maybe a) -> e -> m (Either e a)
orThrow action e =
  maybe (Left e) Right <$> action