module Doneq.Page.Discover where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Maybe (Maybe(..))
import Doneq.Capability.Navigate (class Navigate, navigate_)
import Doneq.Component.HTML.Header (header)
import Doneq.Data.Profile (Profile)
import Doneq.Data.Route (Route(..))
import Doneq.Env (UserEnv)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Tailwind as T
import Web.Event.Event (Event)

data Action
  = Initialize
  | Receive { currentUser :: Maybe Profile }
  | Navigate Route Event

type State = {currentUser :: Maybe Profile}

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
      , initialize = Just Initialize
      }
  }
  where
  initialState { currentUser } = { currentUser }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> pure unit

    Receive { currentUser } ->
      H.modify_ _ { currentUser = currentUser }

    Navigate route e -> navigate_ e route

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { currentUser } =
    HH.div
      [ HP.classes [ T.minHScreen, T.wScreen, T.flex, T.flexCol, T.itemsCenter ] ]
      [ header currentUser Navigate Discover
      , HH.div
          [ HP.classes [ T.container, T.textCenter, T.mt10 ] ]
          [ HH.text "discover" ]
      ]
