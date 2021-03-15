-- | This module exports various utilities for working with a REST API and Json. It also provides
-- | a few helpers shared among requests which I found useful when implementing the production
-- | monad, `Listasio.AppM`.
module Listasio.Api.Utils where

import Prelude

import Affjax (printError, request)
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Codec.Argonaut (JsonCodec, printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Listasio.Api.Request (BaseURL, RequestOptions, Token, defaultRequest, readToken, removeToken, writeToken)
import Listasio.Capability.LogMessages (class LogMessages, logError)
import Listasio.Capability.Now (class Now)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Env (UserEnv)

-- TODO: parse API errors into an union type
data HttpError
  = RequestError
  | ServerError StatusCode String
  | ClientError StatusCode String
  | DecodingError String

instance showHttpError :: Show HttpError where
  show RequestError = "RequestError"
  show (ServerError code msg) = "ServerError " <> show code <> " " <> msg
  show (ClientError code msg) = "ClientError " <> show code <> " " <> msg
  show (DecodingError msg) = "DecodingError " <> msg

-- | This function performs a request that does not require authentication by
-- | pulling the base URL out of the app environment and running an asynchronous
-- | request. This function only requires the `baseUrl` field from the app
-- | environment.
mkRequest
  :: forall a m r
   . MonadAff m
  => MonadAsk { baseUrl :: BaseURL | r } m
  => LogMessages m
  => Now m
  => RequestOptions
  -> JsonCodec a
  -> m (Either HttpError a)
mkRequest opts codec = do
  {baseUrl} <- ask
  response <- liftAff $ request $ defaultRequest baseUrl Nothing opts
  case response of
    Left err -> do
      logError $ printError err
      pure $ Left RequestError
    Right {status} | status >= (StatusCode 500) -> pure $ Left $ ServerError status "Something went wrong"
    Right {status} | status >= (StatusCode 400) -> pure $ Left $ ClientError status "Something went wrong"
    Right {body} ->
      case CA.decode codec body of
        Left err -> do
           let errorMsg = printJsonDecodeError err
           logError errorMsg
           pure $ Left $ DecodingError errorMsg
        Right decodedBody -> pure $ Right decodedBody

clearUserEnv :: forall m r . MonadAff m => MonadAsk { userEnv :: UserEnv | r } m => m Unit
clearUserEnv = do
  {userEnv} <- ask
  liftEffect do
    removeToken
    Ref.write Nothing userEnv.currentUser
  liftAff $ Bus.write Nothing userEnv.userBus

-- | This function performs a request that requires authentication by pulling
-- | the base URL out of the app environment, reading the auth token from local
-- | storage, and then performing the asynchronous request. In case of an auth
-- | error (401 or fetch failure) the user is cleared from the `userEnv`.
mkAuthRequest
  :: forall a m r
   . MonadAff m
  => MonadAsk { baseUrl :: BaseURL, userEnv :: UserEnv | r } m
  => LogMessages m
  => Now m
  => RequestOptions
  -> JsonCodec a
  -> m (Either HttpError a)
mkAuthRequest opts codec = do
  {baseUrl} <- ask
  token <- liftEffect readToken
  response <- liftAff $ request $ defaultRequest baseUrl token opts
  case response of
    Left err -> do
      logError $ printError err
      clearUserEnv
      pure $ Left RequestError

    Right {status: StatusCode 401} -> do
      clearUserEnv
      pure $ Left $ ClientError (StatusCode 401) "Something went wrong"

    Right {status} | status >= (StatusCode 500) -> pure $ Left $ ServerError status "Something went wrong"
    Right {status} | status >= (StatusCode 400) -> pure $ Left $ ClientError status "Something went wrong"

    Right {body} ->
      case CA.decode codec body of
        Left err -> do
           let errorMsg = printJsonDecodeError err
           logError errorMsg
           pure $ Left $ DecodingError errorMsg
        Right decodedBody -> pure $ Right decodedBody

-- | Logging requires uptading the application environment and writing the auth
-- | token to local storage. This helper function makes it easy to layer those
-- | behaviors on top of the request. This also performs the work of broadcasting
-- | changes in the current user to all subscribed components.
authenticate
  :: forall m a r
   . MonadAff m
  => MonadAsk { baseUrl :: BaseURL, userEnv :: UserEnv | r } m
  => LogMessages m
  => Now m
  => (BaseURL -> a -> m (Either String (Tuple Token ProfileWithIdAndEmail)))
  -> a
  -> m (Maybe ProfileWithIdAndEmail)
authenticate req fields = do
  { baseUrl, userEnv } <- ask
  req baseUrl fields >>= case _ of
    Left err -> logError err *> pure Nothing
    Right (Tuple token profile) -> do
      liftEffect do
        writeToken token
        Ref.write (Just profile) userEnv.currentUser
      -- any time we write to the current user ref, we should also broadcast the change
      liftAff $ Bus.write (Just profile) userEnv.userBus
      pure (Just profile)
