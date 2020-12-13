module Doneq.Page.Register where

import Prelude
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Doneq.Api.Request (RegisterFields)
import Doneq.Capability.Navigate (class Navigate, navigate, navigate_)
import Doneq.Capability.Resource.User (class ManageUser, registerUser)
import Doneq.Component.HTML.Header (header)
import Doneq.Component.HTML.Utils (safeHref)
import Doneq.Data.Email (Email)
import Doneq.Data.Route (Route(..))
import Doneq.Data.Username (Username)
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

newtype RegisterForm r f
  = RegisterForm
  ( r
      ( name :: f V.FormError String Username
      , slug :: f V.FormError String Username
      , email :: f V.FormError String Email
      , password :: f V.FormError String String
      )
  )

derive instance newtypeRegisterForm :: Newtype (RegisterForm r f) _

data Action
  = HandleRegisterForm RegisterFields
  | Navigate Route Event.Event

component ::
  forall q o m.
  MonadAff m =>
  ManageUser m =>
  Navigate m =>
  H.Component HH.HTML q Unit o m
component =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  handleAction = case _ of
    -- TODO
    HandleRegisterForm fields -> registerUser fields >>= traverse_ (\_ -> navigate Home)
    Navigate route e -> navigate_ e route

  render _ =
    container
      [ HH.h1
          []
          [ HH.text "Sign Up" ]
      , HH.p
          []
          [ HH.a
              [ safeHref Login, HE.onClick (Just <<< Navigate Login <<< Mouse.toEvent) ]
              [ HH.text "Already have an account?" ]
          ]
      , HH.slot F._formless unit formComponent unit (Just <<< HandleRegisterForm)
      ]
    where
    container html =
      HH.div
        [ HP.classes [ T.minHScreen, T.wScreen, T.flex, T.flexCol, T.itemsCenter ] ]
        [ header Nothing Navigate Register
        , HH.div [] html
        ]

data FormAction
  = Submit Event.Event

formComponent ::
  forall formQuery formSlots formInput m.
  MonadAff m =>
  F.Component RegisterForm formQuery formSlots formInput RegisterFields m
formComponent =
  F.component formInput
    $ F.defaultSpec
        { render = renderForm
        , handleEvent = handleEvent
        -- TODO handleQuery like Login
        , handleAction = handleAction
        }
  where
  formInput :: formInput -> F.Input' RegisterForm m
  formInput _ =
    { validators:
        RegisterForm
          { name: V.required >>> V.usernameFormat
          , slug: V.required >>> V.usernameFormat
          , email: V.required >>> V.minLength 3 >>> V.emailFormat
          , password: V.required >>> V.minLength 8 >>> V.maxLength 20
          }
    , initialInputs: Nothing
    }

  handleEvent = F.raiseResult

  handleAction = case _ of
    Submit event -> do
      H.liftEffect $ Event.preventDefault event
      eval F.submit
    where
    eval act = F.handleAction handleAction handleEvent act

  renderForm { form } =
    HH.form
      [ HE.onSubmit \ev -> Just $ F.injAction $ Submit ev ]
      [ HH.fieldset_
          [ name
          , slug
          , email
          , password
          ]
      , Field.submit "Sign up"
      ]
    where
    proxies = F.mkSProxies (F.FormProxy :: _ RegisterForm)

    name =
      Field.input proxies.name form
        [ HP.placeholder "Username", HP.type_ HP.InputText ]

    slug =
      Field.input proxies.slug form
        [ HP.placeholder "Slug", HP.type_ HP.InputText ]

    email =
      Field.input proxies.email form
        [ HP.placeholder "Email", HP.type_ HP.InputEmail ]

    password =
      Field.input proxies.password form
        [ HP.placeholder "Password", HP.type_ HP.InputPassword ]
