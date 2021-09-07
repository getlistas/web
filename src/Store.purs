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

import Data.Lens (over)
import Data.Maybe (Maybe(..))
import Listasio.Api.Request (BaseURL)
import Listasio.Data.Lens (_lists)
import Listasio.Data.List (ListWithIdUserAndMeta)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Network.RemoteData (RemoteData, _Success)
import Routing.PushState (PushStateInterface)

data LogLevel = Dev | Prod

derive instance Eq LogLevel
derive instance Ord LogLevel

type Lists = Array ListWithIdUserAndMeta
type RD_Lists = RemoteData String Lists

type Store =
  { nav :: PushStateInterface
  , env :: LogLevel
  , baseUrl :: BaseURL
  , currentUser :: Maybe ProfileWithIdAndEmail
  , lists :: RD_Lists
  }

data Action
    -- Session
  = LoginUser ProfileWithIdAndEmail
  | LogoutUser
    -- My Lists
  | SetLists RD_Lists
  | OverLists (Lists -> Lists)

reduce :: Store -> Action -> Store
reduce store = case _ of
  LoginUser profile ->
    store {currentUser = Just profile}

  LogoutUser ->
    store {currentUser = Nothing}

  SetLists lists ->
    store {lists = lists}

  OverLists f ->
    over (_lists <<< _Success) f store
