module Listasio.Env where

import Prelude

import Data.Maybe (Maybe)
import Effect.Aff.Bus (BusRW)
import Effect.Ref (Ref)
import Listasio.Api.Request (BaseURL)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
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
  { currentUser :: Ref (Maybe ProfileWithIdAndEmail)
  , userBus :: BusRW (Maybe ProfileWithIdAndEmail)
  }
