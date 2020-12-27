module Listasio.Api.Request
  ( Token -- constructor and decoders not exported
  , BaseURL(..)
  , RequestMethod(..)
  , RequestOptions(..)
  , defaultRequest
  , RegisterFields(..)
  , LoginFields(..)
  , login
  , register
  , decodeToken
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
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Listasio.Api.Endpoint (Endpoint(..), endpointCodec)
import Listasio.Data.Email (Email)
import Listasio.Data.Email as Email
import Listasio.Data.Profile (Profile)
import Listasio.Data.Profile as Profile
import Listasio.Data.Username (Username)
import Listasio.Data.Username as Username
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Jwt as Jwt
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

-- | The following data types and functions aren't a natural fit for this module,
-- | but they've been included here because they operate on tokens.
-- |
-- | We can't create or manipulate the `Token` type outside this module!
-- | These requests will be the only way to create an auth token in the system.
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

type User = { user :: Profile }

userCodec ::  JsonCodec User
userCodec = CAR.object "user" { user: Profile.profileCodec }

login :: forall m. MonadAff m => BaseURL -> LoginFields -> m (Either String (Tuple Token Profile))
login baseUrl fields = do
  res <- liftAff $ request $ defaultRequest baseUrl Nothing conf
  case res of
    Left e -> pure $ Left $ printError e
    Right v -> pure $ lmap printJwtError $ decodeAuthProfileFromToken =<< decode v.body
  where conf = { endpoint: Login, method: Post $ Just $ Codec.encode loginCodec fields }
        decode = lmap Jwt.JsonDecodeError <<< Codec.decode CA.json

printJwtError :: Jwt.JwtError JsonDecodeError -> String
printJwtError = case _ of
  Jwt.MalformedToken -> "Malformed token"
  Jwt.Base64DecodeError _ -> "Token base64 decode failed"
  Jwt.JsonDecodeError err -> "Token Json decode error: " <> printJsonDecodeError err
  Jwt.JsonParseError s -> "Token Json parse error: " <> s

-- | This JSON decoder is defined in this module because it manipulates a token.
-- | First, we'll decode the token field from the payload, and then we'll decode
-- | the user's profile from the token itself.
decodeAuthProfileFromToken :: Json -> Either (Jwt.JwtError JsonDecodeError) (Tuple Token Profile)
decodeAuthProfileFromToken payload = do
  { access_token } <- lmap Jwt.JsonDecodeError $ Codec.decode tokenCodec payload
  { user } <- Jwt.decodeWith (Codec.decode userCodec) access_token
  pure $ Tuple (Token access_token) user
  where tokenCodec = CAR.object "access_token" { access_token: CA.string }

register :: forall m. MonadAff m => BaseURL -> RegisterFields -> m (Either String Profile)
register baseUrl fields = do
  res <- liftAff $ request $ defaultRequest baseUrl Nothing conf
  pure $ decode <$> _.body =<< lmap printError res
  where method = Post $ Just $ Codec.encode registerCodec fields
        conf = { endpoint: Users, method }
        decode = lmap printJsonDecodeError <<< Codec.decode Profile.profileCodec

tokenKey = "token" :: String

decodeToken :: Token -> Either String Profile
decodeToken (Token token) =
   lmap printJwtError $ _.user <$> Jwt.decodeWith (Codec.decode userCodec) token

readToken :: Effect (Maybe Token)
readToken = do
  str <- getItem tokenKey =<< localStorage =<< window
  pure $ map Token str

writeToken :: Token -> Effect Unit
writeToken (Token str) = setItem tokenKey str =<< localStorage =<< window

removeToken :: Effect Unit
removeToken = removeItem tokenKey =<< localStorage =<< window
