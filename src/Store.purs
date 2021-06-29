-- | A global state containing information useful to most components in the
-- | application. If you've ever used Redux, this should look familiar.
-- | Components can read, write, and subscribe to this central state, which is
-- | called a "store" by convention.
-- |
-- | More in-depth documentation on creating a store can be found in the
-- | docs for the `halogen-store` library:
-- | https://github.com/thomashoneyman/purescript-halogen-store
module Listasio.Store where

import Prelude

import Data.Maybe (Maybe(..))
import Listasio.Api.Request (BaseURL)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Routing.PushState (PushStateInterface)

data LogLevel = Dev | Prod

derive instance Eq LogLevel
derive instance Ord LogLevel

type Store =
  { nav :: PushStateInterface
  , env :: LogLevel
  , baseUrl :: BaseURL
  , currentUser :: Maybe ProfileWithIdAndEmail
  }

data Action
  = LoginUser ProfileWithIdAndEmail
  | LogoutUser

reduce :: Store -> Action -> Store
reduce store = case _ of
  LoginUser profile ->
    store { currentUser = Just profile }

  LogoutUser ->
    store { currentUser = Nothing }
