module Listasio.Env where

import Prelude

import Listasio.Api.Request (BaseURL)
import Listasio.Data.Profile (Profile)
import Data.Maybe (Maybe)
import Effect.Aff.Bus (BusRW)
import Effect.Ref (Ref)
import Routing.PushState (PushStateInterface)

type Env =
  { nav :: PushStateInterface
  , logLevel :: LogLevel
  , baseUrl :: BaseURL
  , userEnv :: UserEnv
  }

data LogLevel = Dev | Prod

derive instance eqLogLevel :: Eq LogLevel
derive instance ordLogLevel :: Ord LogLevel

type UserEnv =
  { currentUser :: Ref (Maybe Profile)
  , userBus :: BusRW (Maybe Profile)
  }
