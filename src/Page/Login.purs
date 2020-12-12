module Doneq.Page.Login where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Doneq.Api.Request (LoginFields)
import Doneq.Capability.Navigate (class Navigate, navigate, navigate_)
import Doneq.Capability.Resource.User (class ManageUser, loginUser)
import Doneq.Component.HTML.Header (header)
import Doneq.Component.HTML.Utils (safeHref, whenElem)
import Doneq.Data.Email (Email)
import Doneq.Data.Route (Route(..))
import Doneq.Form.Field as Field
import Doneq.Form.Validation as V
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Tailwind as T
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent as Mouse

data Action
  = HandleLoginForm LoginFields
  | Navigate Route Event.Event

type State
  = { redirect :: Boolean } -- TODO: Maybe Route instead

type Input
  = { redirect :: Boolean }

type ChildSlots
  = ( formless :: F.Slot LoginForm FormQuery () LoginFields Unit )

component ::
  forall q o m.
  MonadAff m =>
  Navigate m =>
  ManageUser m =>
  H.Component HH.HTML q Input o m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    HandleLoginForm fields -> do
      -- broadcast the user changes to subscribed components so they receive the up-to-date value
      loginUser fields
        >>= case _ of
            Nothing -> void $ H.query F._formless unit $ F.injQuery $ SetLoginError true unit
            Just profile -> do
              void $ H.query F._formless unit $ F.injQuery $ SetLoginError false unit
              st <- H.get
              when st.redirect (navigate Home)
    Navigate route e -> navigate_ e route

  render :: State -> H.ComponentHTML Action ChildSlots m
  render _ =
    container
      [ HH.h1
          []
          [ HH.text "Sign In" ]
      , HH.p
          []
          [ HH.a
              [ safeHref Register, HE.onClick (Just <<< Navigate Register <<< Mouse.toEvent) ]
              [ HH.text "Need an account?" ]
          ]
      , HH.slot F._formless unit formComponent unit (Just <<< HandleLoginForm)
      ]
    where
    container html =
      HH.div
        [ HP.classes [ T.minHScreen, T.wScreen, T.flex, T.flexCol, T.itemsCenter ] ]
        [ header Nothing Navigate Login
        , HH.div [] html
        ]

newtype LoginForm r f
  = LoginForm
  ( r
      ( email :: f V.FormError String Email
      , password :: f V.FormError String String
      )
  )

derive instance newtypeLoginForm :: Newtype (LoginForm r f) _

data FormQuery a
  = SetLoginError Boolean a

derive instance functorFormQuery :: Functor FormQuery

data FormAction
  = Submit Event.Event

formComponent ::
  forall i slots m.
  MonadAff m =>
  F.Component LoginForm FormQuery slots i LoginFields m
formComponent =
  F.component formInput
    $ F.defaultSpec
        { render = renderLogin
        , handleEvent = handleEvent
        , handleQuery = handleQuery
        , handleAction = handleAction
        }
  where
  formInput :: i -> F.Input LoginForm ( loginError :: Boolean ) m
  formInput _ =
    { validators:
        LoginForm
          { email: V.required >>> V.minLength 3 >>> V.emailFormat
          , password: V.required >>> V.minLength 2 >>> V.maxLength 20
          }
    , initialInputs: Nothing
    , loginError: false
    }

  handleEvent = F.raiseResult

  handleAction = case _ of
    Submit event -> do
      H.liftEffect $ Event.preventDefault event
      eval F.submit
    where
    eval act = F.handleAction handleAction handleEvent act

  handleQuery :: forall a. FormQuery a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    SetLoginError bool a -> do
      H.modify_ _ { loginError = bool }
      pure (Just a)

  proxies = F.mkSProxies (F.FormProxy :: _ LoginForm)

  renderLogin { form, loginError } =
    HH.form
      [ HE.onSubmit \ev -> Just $ F.injAction $ Submit ev ]
      [ whenElem loginError \_ ->
          HH.div
            []
            [ HH.text "Email or password is invalid" ]
      , HH.fieldset_
          [ Field.input proxies.email form
              [ HP.placeholder "Email"
              , HP.type_ HP.InputEmail
              ]
          , Field.input proxies.password form
              [ HP.placeholder "Password"
              , HP.type_ HP.InputPassword
              ]
          , Field.submit "Log in"
          ]
      ]
