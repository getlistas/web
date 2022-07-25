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

import Data.Lens (_Just, over, set)
import Data.Maybe (Maybe(..))
import Listasio.Api.Request (BaseURL)
import Listasio.Data.Lens (_fullScreen, _lists, _reader)
import Listasio.Data.List (ListWithIdUserAndMeta)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Resource (ListResourceWithConent)
import Network.RemoteData (RemoteData, _Success)
import Routing.PushState (PushStateInterface)

data LogLevel = Dev | Prod

derive instance Eq LogLevel
derive instance Ord LogLevel

type Lists = Array ListWithIdUserAndMeta
type RD_Lists = RemoteData String Lists

type ReaderState =
  { resource :: ListResourceWithConent
  , fullScreen :: Boolean
  }

type Store =
  { nav :: PushStateInterface
  , env :: LogLevel
  , baseUrl :: BaseURL
  , currentUser :: Maybe ProfileWithIdAndEmail
  , lists :: RD_Lists
  , reader :: Maybe ReaderState
  }

data Action
  -- Session
  = LoginUser ProfileWithIdAndEmail
  | LogoutUser
  -- My Lists
  | SetLists RD_Lists
  | OverLists (Lists -> Lists)
  -- Reader
  | Fullscreen Boolean
  | SetReader ListResourceWithConent
  | ClearReader

reduce :: Store -> Action -> Store
reduce store = case _ of
  LoginUser profile ->
    store { currentUser = Just profile }

  LogoutUser ->
    store { currentUser = Nothing }

  SetLists lists ->
    store { lists = lists }

  OverLists f ->
    over (_lists <<< _Success) f store

  Fullscreen isOn ->
    set (_reader <<< _Just <<< _fullScreen) isOn store

  SetReader resource ->
    set _reader (Just {resource, fullScreen: false}) store

  ClearReader ->
    set _reader Nothing store
