module Listasio.Component.Link where

import Prelude

import Listasio.Capability.Navigate (class Navigate, navigate)
import Listasio.Component.HTML.Utils (safeHref)
import Listasio.Data.Route (Route)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
-- import Halogen.HTML.Properties as HP
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type Slot id = forall query. H.Slot query Void id

_link = SProxy :: SProxy "link"

data Action
  = Navigate Route MouseEvent

type State =
  { label :: String
  , route :: Route
  }

type Input =
  { label :: String
  , route :: Route
  }

component
  :: forall q m output
   . MonadEffect m
  => Navigate m
  => H.Component HH.HTML q Input output m
component = H.mkComponent
  { initialState: \input -> input
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    Navigate route e -> do
      H.liftEffect $ preventDefault $ toEvent e
      navigate route

  render :: State -> H.ComponentHTML Action () m
  render { label, route } =
    HH.a
      [ safeHref route, HE.onClick $ Just <<< Navigate route ]
      [ HH.text label ]
