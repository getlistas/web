module Listasio.Page.Pricing where

import Prelude

import Listasio.Component.HTML.Icons as Icons
import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Route (Route)
import Listasio.Env (UserEnv)
import Tailwind as T
import Web.Event.Event (Event)

data Action
  = Receive { currentUser :: Maybe ProfileWithIdAndEmail }
  | Navigate Route Event

type State = {currentUser :: Maybe ProfileWithIdAndEmail}

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
  initialState { currentUser } = { currentUser }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Receive { currentUser } ->
      H.modify_ _ { currentUser = currentUser }

    Navigate route e -> navigate_ e route

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { currentUser } =
    HH.div
      []
      [ HH.div
          [ HP.classes [ T.pt2 ] ]
          [ HH.h1
              [ HP.classes [ T.textGray400, T.mb6, T.text4xl, T.fontBold ] ]
              [ HH.text "Pricing" ]
          ]
      , wip
      ]

    where
    wip =
      HH.div
        [ HP.classes [ T.p2, T.roundedLg, T.bgDurazno, T.smP3, T.mb8 ] ]
        [ HH.div
            [ HP.classes [ T.flex, T.itemsCenter ] ]
            [ HH.span
                [ HP.classes [ T.flex, T.p2, T.roundedLg, T.bgManzana ] ]
                [ Icons.code
                    [ Icons.classes [ T.h6, T.w6, T.textWhite ] ]
                ]
            , HH.p
                [ HP.classes [ T.ml3, T.fontMedium, T.textWhite ] ]
                [ HH.text "Work in progress"
                ]
            ]
        ]
