module Listasio.Page.Register where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Listasio.Capability.Analytics (class Analytics)
import Listasio.Capability.Navigate (class Navigate, navigate, navigate_)
import Listasio.Capability.Resource.User (class ManageUser)
import Listasio.Component.HTML.Register as Register
import Listasio.Data.Route (Route(..))
import Tailwind as T
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event

_slot :: Proxy "register"
_slot = Proxy

type ChildSlots
  = (register :: Register.Slot)

type State = Unit

data Action
  = Navigate Route Event.Event
  | GoToSignin Register.Output

component
  :: forall q o m
   . MonadAff m
  => ManageUser m
  => Navigate m
  => Analytics m
  => H.Component q Unit o m
component =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Navigate route e -> navigate_ e route

    GoToSignin Register.GoToSignin -> navigate Login

  render :: State -> H.ComponentHTML Action ChildSlots m
  render _ =
    HH.div
      [ HP.classes [ T.mt12, T.flex, T.flexCol, T.itemsCenter ] ]
      [ HH.h1
          [ HP.classes [ T.textGray400, T.text2xl, T.fontBold, T.mb8 ] ]
          [ HH.text "Create your account" ]
      , HH.div
          [ HP.classes [ T.w96, T.maxWFull ] ]
          [ HH.slot Register._slot unit Register.component unit GoToSignin
          ]
      ]
