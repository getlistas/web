module Listasio.Api.Request
  ( Token -- constructor and decoders not exported
  , BaseURL(..)
  , RequestMethod(..)
  , RequestOptions(..)
  , defaultRequest
  , RegisterFields(..)
  , LoginFields(..)
  , login
  , googleLogin
  , register
  , decodeToken
  , readToken
  , writeToken
  , removeToken
  , initGoogleAuth
  ) where

import Prelude

import Affjax as Fetch
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Argonaut.Core (Json)
import Data.Bifunctor (lmap)
import Data.Codec as Codec
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError, printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Jwt as Jwt
import Listasio.Api.Endpoint (Endpoint(..), endpointCodec)
import Listasio.Data.Email (Email)
import Listasio.Data.Email as Email
import Listasio.Data.ID (ID)
import Listasio.Data.ID as ID
import Listasio.Data.Profile (ProfileWithIdAndEmail, ProfileRep)
import Listasio.Data.Username (Username)
import Listasio.Data.Username as Username
import Routing.Duplex (print)
import Slug as Slug
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)

type GoogleAuthResult
  =
  { success :: Boolean
  , token :: Nullable String
  , image_url :: Nullable String
  }

foreign import googleAuth_ :: Effect (Promise GoogleAuthResult)

foreign import initAuth_ :: String -> Effect (Promise Unit)

doGoogleAuth :: Aff GoogleAuthResult
doGoogleAuth = Promise.toAffE googleAuth_

initGoogleAuth :: Aff Unit
initGoogleAuth = Promise.toAffE $ initAuth_ "580260201094-fqnilnjt95lpl4clqe465cjhh0plde4v"

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
  =
  { endpoint :: Endpoint
  , method :: RequestMethod
  }

defaultRequest :: BaseURL -> Maybe Token -> RequestOptions -> Fetch.Request Json
defaultRequest (BaseURL baseUrl) auth { endpoint, method } =
  Fetch.defaultRequest
    { method = Left method
    , url = baseUrl <> print endpointCodec endpoint
    , headers =
        case auth of
          Nothing -> []
          Just (Token t) -> [ RequestHeader "Authorization" $ "Bearer " <> t ]
    , content = RB.json <$> body
    , withCredentials = false
    , responseFormat = RF.json
    }
  where
  Tuple method body = case method of
    Get -> Tuple GET Nothing
    Post b -> Tuple POST b
    Put b -> Tuple PUT b
    Delete -> Tuple DELETE Nothing

type RegisterFields
  =
  { email :: Email
  , password :: String
  , name :: Username
  }

registerCodec :: JsonCodec RegisterFields
registerCodec =
  CAR.object "RegisterFields"
    { email: Email.codec
    , password: CA.string
    , name: Username.codec
    }

-- | The following data types and functions aren't a natural fit for this module,
-- | but they've been included here because they operate on tokens.
-- |
-- | We can't create or manipulate the `Token` type outside this module!
-- | These requests will be the only way to create an auth token in the system.
type LoginFields
  =
  { email :: Email
  , password :: String
  }

loginCodec :: JsonCodec LoginFields
loginCodec =
  CAR.object "LoginFields"
    { email: Email.codec
    , password: CA.string
    }

type CreatedProfile = { | ProfileRep (email :: Email, id :: ID) }

type User
  = { user :: CreatedProfile }

createdProfileCodec :: JsonCodec CreatedProfile
createdProfileCodec =
  CAR.object "TokenUser.user"
    { email: Email.codec
    , name: Username.codec
    , id: ID.codec
    , slug: Slug.codec
    }

userCodec :: JsonCodec User
userCodec = CAR.object "TokenUser" { user: createdProfileCodec }

addAvatarField :: CreatedProfile -> ProfileWithIdAndEmail
addAvatarField { email, name, id, slug } =
  { email, name, id, slug, avatar: Nothing }

login :: forall m. MonadAff m => BaseURL -> LoginFields -> m (Either String (Tuple Token ProfileWithIdAndEmail))
login baseUrl fields = do
  res <- liftAff $ Fetch.request $ defaultRequest baseUrl Nothing conf
  case res of
    Left e -> pure $ Left $ Fetch.printError e
    Right v -> pure $ lmap printJwtError $ decodeAuthProfileFromToken =<< decode v.body
  where
  conf = { endpoint: Login, method: Post $ Just $ Codec.encode loginCodec fields }
  decode = lmap Jwt.JsonDecodeError <<< Codec.decode CA.json

type GoogleLoginFields = { token :: String }

googleLoginCodec :: JsonCodec GoogleLoginFields
googleLoginCodec = CAR.object "GoogleLoginFields" { token: CA.string }

googleLogin :: forall m. MonadAff m => BaseURL -> Unit -> m (Either String (Tuple Token ProfileWithIdAndEmail))
googleLogin baseUrl _ = do
  mbGoogleToken <- (\r -> r { token = Nullable.toMaybe r.token }) <$> liftAff doGoogleAuth
  case mbGoogleToken of
    { success } | not success ->
      pure $ Left "Failed to login with Google"
    { token: Nothing } ->
      pure $ Left "Failed to login with Google"
    { token: Just googleToken } -> do
      let
        conf = { endpoint: GoogleLogin, method: Post $ Just $ body }
        body = Codec.encode googleLoginCodec { token: googleToken }
        decode = lmap Jwt.JsonDecodeError <<< Codec.decode CA.json
      res <- liftAff $ Fetch.request $ defaultRequest baseUrl Nothing conf
      case res of
        Left e -> pure $ Left $ Fetch.printError e
        Right v -> pure $ lmap printJwtError $ decodeAuthProfileFromToken =<< decode v.body

printJwtError :: Jwt.JwtError JsonDecodeError -> String
printJwtError = case _ of
  Jwt.MalformedToken -> "Malformed token"
  Jwt.Base64DecodeError _ -> "Token base64 decode failed"
  Jwt.JsonDecodeError err -> "Token Json decode error: " <> printJsonDecodeError err
  Jwt.JsonParseError s -> "Token Json parse error: " <> s

-- | This JSON decoder is defined in this module because it manipulates a token.
-- | First, we'll decode the token field from the payload, and then we'll decode
-- | the user's profile from the token itself.
decodeAuthProfileFromToken :: Json -> Either (Jwt.JwtError JsonDecodeError) (Tuple Token ProfileWithIdAndEmail)
decodeAuthProfileFromToken payload = do
  { access_token } <- lmap Jwt.JsonDecodeError $ Codec.decode tokenCodec payload
  user <- addAvatarField <$> _.user <$> Jwt.decodeWith (Codec.decode userCodec) access_token
  pure $ Tuple (Token access_token) user
  where
  tokenCodec = CAR.object "access_token" { access_token: CA.string }

register :: forall m. MonadAff m => BaseURL -> RegisterFields -> m (Either String ProfileWithIdAndEmail)
register baseUrl fields = do
  res <- liftAff $ Fetch.request $ defaultRequest baseUrl Nothing conf
  pure $ decode <$> _.body =<< lmap Fetch.printError res
  where
  method = Post $ Just $ Codec.encode registerCodec fields
  conf = { endpoint: Users, method }
  decode = map addAvatarField <$> lmap printJsonDecodeError <<< Codec.decode createdProfileCodec

tokenKey = "token" :: String

decodeToken :: Token -> Either String ProfileWithIdAndEmail
decodeToken (Token token) =
  lmap printJwtError $ addAvatarField <$> _.user <$> Jwt.decodeWith (Codec.decode userCodec) token

readToken :: Effect (Maybe Token)
readToken = do
  str <- getItem tokenKey =<< localStorage =<< window
  pure $ map Token str

writeToken :: Token -> Effect Unit
writeToken (Token str) = setItem tokenKey str =<< localStorage =<< window

removeToken :: Effect Unit
removeToken = removeItem tokenKey =<< localStorage =<< window
