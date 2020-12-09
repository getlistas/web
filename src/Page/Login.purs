-- | The login page supports a form users can submit to authenticate their session and gain access
-- | to the application.
module Doneq.Page.Login where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Doneq.Api.Request (LoginFields)
import Doneq.Capability.Navigate (class Navigate, navigate, navigate_)
import Doneq.Capability.Resource.User (class ManageUser, loginUser)
import Doneq.Component.HTML.Header (header)
import Doneq.Component.HTML.Utils (css, safeHref, whenElem)
import Doneq.Data.Email (Email)
import Doneq.Data.Route (Route(..))
import Doneq.Form.Field (submit)
import Doneq.Form.Field as Field
import Doneq.Form.Validation as V
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Tailwind as T
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent (toEvent)

data Action
  = HandleLoginForm LoginFields
  | Navigate Route Event

-- Should this component redirect to home after login or not? If the login page is loaded
-- at the login route, then yes; if not, then it is guarding another route and should not.
type State
  = { redirect :: Boolean }

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
      -- loginUser also handles broadcasting the user changes to subscribed components
      -- so they receive the up-to-date value (see AppM and the `authenticate` function.)
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
          [ css "text-xs-center" ]
          [ HH.text "Sign In" ]
      , HH.p
          [ css "text-xs-center" ]
          [ HH.a
              [ safeHref Register, HE.onClick (Just <<< Navigate Register <<< toEvent) ]
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

-- | See the Formless tutorial to learn how to build your own forms:
-- | https://github.com/thomashoneyman/purescript-halogen-formless
newtype LoginForm r f
  = LoginForm
  ( r
      ( email :: f V.FormError String Email
      , password :: f V.FormError String String
      )
  )

derive instance newtypeLoginForm :: Newtype (LoginForm r f) _

-- We can extend our form to receive more queries than it supports by default. Here, we'll
-- set a login error from the parent.
data FormQuery a
  = SetLoginError Boolean a

derive instance functorFormQuery :: Functor (FormQuery)

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

  handleQuery :: forall a. FormQuery a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    SetLoginError bool a -> do
      H.modify_ _ { loginError = bool }
      pure (Just a)

  proxies = F.mkSProxies (F.FormProxy :: _ LoginForm)

  renderLogin { form, loginError } =
    HH.form_
      [ whenElem loginError \_ ->
          HH.div
            [ css "error-messages" ]
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
          , submit "Log in"
          ]
      ]
