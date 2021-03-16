module Listasio.Page.ViewList where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Data.List (ListWithIdUserAndMeta)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Route (Route)
import Listasio.Env (UserEnv)
import Network.RemoteData (RemoteData(..))
import Tailwind as T
import Web.Event.Event (Event)

data Action
  = Receive { currentUser :: Maybe ProfileWithIdAndEmail }
  | Navigate Route Event

type State
  = { currentUser :: Maybe ProfileWithIdAndEmail
    , list :: RemoteData String ListWithIdUserAndMeta
    }

component
  :: forall q o m r
   . MonadAff m
  => MonadAsk { userEnv :: UserEnv | r } m
  => Navigate m
  => H.Component HH.HTML q {} o m
component = Connect.component $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }
  where
  initialState { currentUser } =
    { currentUser
    , list: NotAsked
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Receive { currentUser } ->
      H.modify_ _ { currentUser = currentUser }

    Navigate route e -> navigate_ e route

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { currentUser } =
    HH.div
      [ HP.classes [ T.container, T.textCenter, T.mt10 ] ]
      [ HH.text "view list" ]
