module Listasio.Page.Login where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Listasio.Capability.Analytics (class Analytics)
import Listasio.Capability.Navigate (class Navigate, navigate, navigate_)
import Listasio.Capability.Resource.User (class ManageUser)
import Listasio.Component.HTML.Login as Login
import Listasio.Component.HTML.Message as Message
import Listasio.Component.HTML.Utils (whenElem)
import Listasio.Data.Route (Route(..))
import Tailwind as T
import Web.Event.Event as Event

type ChildSlots
  = ( login :: Login.Slot )

data Action
  = Navigate Route Event.Event
  | GoToRegister Login.Output

type State
  = { redirect :: Boolean
    , registerSuccess :: Boolean
    }

type Input
  = { redirect :: Boolean
    , registerSuccess :: Boolean
    }

component ::
  forall q o m.
  MonadAff m =>
  Navigate m =>
  ManageUser m =>
  Analytics m =>
  H.Component HH.HTML q Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }
  where
  initialState { redirect, registerSuccess } =
    { redirect, registerSuccess }

  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Navigate route e -> navigate_ e route

    GoToRegister Login.GoToRegister -> navigate Register

  render :: State -> H.ComponentHTML Action ChildSlots m
  render {redirect, registerSuccess} =
    HH.div
      [ HP.classes [ T.mt12, T.flex, T.flexCol, T.itemsCenter ] ]
      [ HH.h1
          [ HP.classes [ T.textGray400, T.text2xl, T.fontBold, T.mb8 ] ]
          [ HH.text "Welcome to Listas" ]
      , whenElem registerSuccess \_ ->
          Message.message $ Message.props
            { classes = [ T.mb6 ]
            , title = Just "Registration succesful!"
            , text = Just "Login to start using the app"
            , icon = Just "🎉"
            }
      , HH.div
          [ HP.classes [ T.w96, T.maxWFull ] ]
          [ HH.slot (SProxy :: _ "login") unit Login.component {redirect} (Just <<< GoToRegister)
          ]
      ]
