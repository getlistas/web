module Doneq.Api.Request
  ( Token -- constructor and decoders not exported
  , BaseURL(..)
  , RequestMethod(..)
  , RequestOptions(..)
  , defaultRequest
  , RegisterFields(..)
  , LoginFields(..)
  , login
  , register
  , readToken
  , writeToken
  , removeToken
  ) where

import Prelude
import Affjax (Request, printError, request)
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Data.Argonaut.Core (Json)
import Data.Bifunctor (lmap)
import Data.Codec as Codec
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError, printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..), note)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Doneq.Api.Endpoint (Endpoint(..), endpointCodec)
import Doneq.Data.Email (Email)
import Doneq.Data.Email as Email
import Doneq.Data.Profile (Profile)
import Doneq.Data.Profile as Profile
import Doneq.Data.Username (Username)
import Doneq.Data.Username as Username
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Routing.Duplex (print)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)

newtype Token
  = Token String

-- | No `newtype` instance allowed!
derive instance eqToken :: Eq Token

derive instance ordToken :: Ord Token

-- | Manual instance to avoid revealing the token.
instance showToken :: Show Token where
  show (Token _) = "Token {- token -}"

newtype BaseURL
  = BaseURL String

data RequestMethod
  = Get
  | Post (Maybe Json)
  | Put (Maybe Json)
  | Delete

type RequestOptions
  = { endpoint :: Endpoint
    , method :: RequestMethod
    }

defaultRequest :: BaseURL -> Maybe Token -> RequestOptions -> Request Json
defaultRequest (BaseURL baseUrl) auth { endpoint, method } =
  { method: Left method
  , url: baseUrl <> print endpointCodec endpoint
  , headers:
      case auth of
        Nothing -> []
        Just (Token t) -> [ RequestHeader "Authorization" $ "Bearer " <> t ]
  , content: RB.json <$> body
  , username: Nothing
  , password: Nothing
  , withCredentials: false
  , responseFormat: RF.json
  }
  where
  Tuple method body = case method of
    Get -> Tuple GET Nothing
    Post b -> Tuple POST b
    Put b -> Tuple PUT b
    Delete -> Tuple DELETE Nothing

-- | The following data types and functions aren't a natural fit for this module,
-- | but they've been included here because they operate on tokens.
-- |
-- | We can't create or manipulate the `Token` type outside this module!
-- | These requests will be the only way to create an auth token in the system.
type RegisterFields
  = { email :: Email
    , password :: String
    , name :: Username
    , slug :: Username
    }

registerCodec :: JsonCodec RegisterFields
registerCodec =
  CAR.object "RegisterFields"
    { email: Email.codec
    , password: CA.string
    , name: Username.codec
    , slug: Username.codec
    }

type LoginFields
  = { email :: Email
    , password :: String
    }

loginCodec :: JsonCodec LoginFields
loginCodec =
  CAR.object "LoginFields"
    { email: Email.codec
    , password: CA.string
    }

-- {"access_token":"<asdf>.<asdf>.<asdf>"}
login :: forall m. MonadAff m => BaseURL -> LoginFields -> m (Either String (Tuple Token Profile))
login baseUrl fields =
  let
    method = Post $ Just $ Codec.encode loginCodec fields
  in
    requestUser baseUrl { endpoint: Login, method }

-- {"id":"<asdf>","email":"<asdf>@<asdf>.<asdf>","name":"<asdf>","slug":"<asdf>"}
register :: forall m. MonadAff m => BaseURL -> RegisterFields -> m (Either String (Tuple Token Profile))
register baseUrl fields =
  let
    method = Post $ Just $ Codec.encode registerCodec fields
  in
    requestUser baseUrl { endpoint: Users, method }

-- | The login and registration requests share the same underlying implementation, just a different
-- | endpoint. This function can be re-used by both requests.
requestUser :: forall m. MonadAff m => BaseURL -> RequestOptions -> m (Either String (Tuple Token Profile))
requestUser baseUrl opts = do
  res <- liftAff $ request $ defaultRequest baseUrl Nothing opts
  case res of
    Left e -> pure $ Left $ printError e
    Right v -> pure $ lmap printJsonDecodeError $ decodeAuthProfile =<< Codec.decode CA.json v.body

-- | This JSON decoder is defined in this module because it manipulates a token. First, we'll decode
-- | only the token field from the payload, and then we'll decode everything else separately into
-- | the user's profile.
decodeAuthProfile :: Json -> Either JsonDecodeError (Tuple Token Profile)
decodeAuthProfile user = do
  -- TODO HERE
  { access_token } <- Codec.decode (CAR.object "access_token" { access_token: tokenCodec }) user
  profile <- case Codec.decode Profile.profileCodec user of
    Right p -> Right p
    Left e -> case Username.parse "test1" of
      Just name -> note e $ { name, slug: _ } <$> Username.parse "test1"
      Nothing -> Left e
  pure (Tuple access_token profile)
  where
  tokenCodec = CA.prismaticCodec (Just <<< Token) (\(Token t) -> t) CA.string

tokenKey = "token" :: String

readToken :: Effect (Maybe Token)
readToken = do
  str <- getItem tokenKey =<< localStorage =<< window
  pure $ map Token str

writeToken :: Token -> Effect Unit
writeToken (Token str) = setItem tokenKey str =<< localStorage =<< window

removeToken :: Effect Unit
removeToken = removeItem tokenKey =<< localStorage =<< window
