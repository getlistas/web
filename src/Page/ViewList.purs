module Listasio.Page.ViewList where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Data.List (ListWithIdUserAndMeta)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Route (Route)
import Listasio.Store as Store
import Network.RemoteData (RemoteData(..))
import Tailwind as T
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)

_slot :: Proxy "viewList"
_slot = Proxy

data Action
  = Receive (Connected (Maybe ProfileWithIdAndEmail) Unit)
  | Navigate Route Event

type State
  =
  { currentUser :: Maybe ProfileWithIdAndEmail
  , list :: RemoteData String ListWithIdUserAndMeta
  }

component
  :: forall q o m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => H.Component q Unit o m
component = connect (selectEq _.currentUser) $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }
  where
  initialState { context: currentUser } =
    { currentUser
    , list: NotAsked
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Receive { context: currentUser } ->
      H.modify_ _ { currentUser = currentUser }

    Navigate route e -> navigate_ e route

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render _ =
    HH.div
      [ HP.classes [ T.container, T.textCenter, T.mt10 ] ]
      [ HH.text "view list" ]
