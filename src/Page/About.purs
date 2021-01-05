module Listasio.Page.About where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Component.HTML.Header (header)
import Listasio.Component.HTML.Layout as Layout
import Listasio.Data.Profile (Profile)
import Listasio.Data.Route (Route(..))
import Listasio.Env (UserEnv)
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
    Layout.dashboard
      currentUser
      Navigate
      (Just About)
      $ HH.div
          []
          [ HH.h1
              [ HP.classes [ T.textGray400, T.mb6, T.text4xl, T.fontBold ] ]
              [ HH.text "About" ]
          , HH.div
              [ HP.classes [ T.textGray400 ] ]
              [ HH.div [] [ HH.text "listas.io your favorite digital consumption manager" ]
              , HH.div
                  [ HP.classes [ T.mt4 ] ]
                  [ HH.a [ HP.classes [ T.textManzana ], HP.target "_blank", HP.href "https://github.com/ndelvalle" ] [ HH.text "@ndelvalle" ]
                  , HH.text " <> "
                  , HH.a [ HP.classes [ T.textManzana ], HP.target "_blank", HP.href "https://twitter.com/DvNahuel" ] [ HH.text "@DvNahuel" ]
                  , HH.text " <> "
                  , HH.a [ HP.classes [ T.textManzana ], HP.target "_blank", HP.href "https://gillchristian.xyz" ] [ HH.text "@gillchristian" ]
                  ]
              ]
          ]
