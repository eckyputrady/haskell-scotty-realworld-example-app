module Feature.Common.HTTP where

import ClassyPrelude

import Feature.Common.Types
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.View as DF
import qualified Text.Digestive.Aeson as DF
  
parsePagination :: (ScottyError e, Monad m) => ActionT e m Pagination
parsePagination = do
  limit <- param "limit" `rescue` const (return 20)
  offset <- param "offset" `rescue` const (return 0)
  return $ Pagination limit offset

parseJsonBody :: (MonadIO m) => DF.Form [Text] m a -> ActionT LText m a
parseJsonBody form = do
  val <- jsonData `rescue` inputMalformedJSONErrorHandler
  (v, result) <- lift $ DF.digestJSON form val
  case result of
    Nothing -> inputErrorHandler v
    Just x -> return x

stopIfError :: (Monad m, ScottyError e') => (e -> ActionT e' m ()) -> m (Either e a) -> ActionT e' m a
stopIfError errHandler action = do
  result <- lift action
  case result of
    Left e -> do 
      errHandler e
      finish
    Right a ->
      return a

inputErrorHandler :: (ScottyError e, Monad m) => DF.View [Text] -> ActionT e m a
inputErrorHandler v = do
  let errs = mapFromList $ map (first (intercalate "." . drop 1)) $ DF.viewErrors v :: InputViolations
  status status422
  json $ ErrorsWrapper errs
  finish

inputMalformedJSONErrorHandler :: (ScottyError e, Monad m) => err -> ActionT e m a
inputMalformedJSONErrorHandler _ = do
  status status422
  json $ ErrorsWrapper $ asText "Malformed JSON payload"
  finish