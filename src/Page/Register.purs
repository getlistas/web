-- | The registration form allows new users to sign up to the Doneq service and authenticate
-- | their session.
module Doneq.Page.Register where

import Prelude
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Doneq.Api.Request (RegisterFields)
import Doneq.Capability.Navigate (class Navigate, navigate, navigate_)
import Doneq.Capability.Resource.User (class ManageUser, registerUser)
import Doneq.Component.HTML.Header (header)
import Doneq.Component.HTML.Utils (css, safeHref)
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
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent (toEvent)

-- | See the Formless tutorial to learn how to build your own forms:
-- | https://github.com/thomashoneyman/purescript-halogen-formless
newtype RegisterForm r f
  = RegisterForm
  ( r
      ( username :: f V.FormError String Username
      , email :: f V.FormError String Email
      , password :: f V.FormError String String
      )
  )

derive instance newtypeRegisterForm :: Newtype (RegisterForm r f) _

data Action
  = HandleRegisterForm RegisterFields
  | Navigate Route Event

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
    HandleRegisterForm fields -> registerUser fields >>= traverse_ (\_ -> navigate Home)
    Navigate route e -> navigate_ e route

  render _ =
    container
      [ HH.h1
          [ css "text-xs-center" ]
          [ HH.text "Sign Up" ]
      , HH.p
          [ css "text-xs-center" ]
          [ HH.a
              [ safeHref Login, HE.onClick (Just <<< Navigate Login <<< toEvent) ]
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

    formComponent ::
      forall formQuery formSlots formInput.
      F.Component RegisterForm formQuery formSlots formInput RegisterFields m
    formComponent =
      F.component formInput
        $ F.defaultSpec
            { render = renderForm
            , handleEvent = F.raiseResult
            }
      where
      formInput :: formInput -> F.Input' RegisterForm m
      formInput _ =
        { validators:
            RegisterForm
              { username: V.required >>> V.usernameFormat
              , email: V.required >>> V.minLength 3 >>> V.emailFormat
              , password: V.required >>> V.minLength 8 >>> V.maxLength 20
              }
        , initialInputs: Nothing
        }

      renderForm { form } =
        HH.form_
          [ HH.fieldset_
              [ username
              , email
              , password
              ]
          , Field.submit "Sign up"
          ]
        where
        proxies = F.mkSProxies (F.FormProxy :: _ RegisterForm)

        username =
          Field.input proxies.username form
            [ HP.placeholder "Username", HP.type_ HP.InputText ]

        email =
          Field.input proxies.email form
            [ HP.placeholder "Email", HP.type_ HP.InputEmail ]

        password =
          Field.input proxies.password form
            [ HP.placeholder "Password", HP.type_ HP.InputPassword ]
