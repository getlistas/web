module Listasio.Page.Register where

import Prelude
import Control.Error.Util (note)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Listasio.Api.Request (RegisterFields)
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Capability.Resource.User (class ManageUser, registerUser)
import Listasio.Component.HTML.Layout as Layout
import Listasio.Component.HTML.Utils (safeHref)
import Listasio.Data.Email (Email)
import Listasio.Data.Route (Route(..))
import Listasio.Data.Username (Username)
import Listasio.Form.Field as Field
import Listasio.Form.Validation as V
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), fromEither)
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

type State
  = { registration :: RemoteData String Unit }

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
    { initialState: const { registration: NotAsked }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  handleAction = case _ of
    HandleRegisterForm fields -> do
      result <- fromEither <$> note "Failed to register" <$> registerUser fields
      H.modify_ _ { registration = result }

    Navigate route e -> navigate_ e route

  render { registration } =
    Layout.noheader
      Nothing
      Navigate
      (Just Register)
      $ HH.div
          [ HP.classes [ T.mt12, T.flex, T.flexCol, T.itemsCenter ] ]
          [ HH.h1
              [ HP.classes [ T.textGray400, T.text2xl, T.fontBold, T.mb6 ] ]
              [ HH.text "Create Account" ]
          , case registration of
              Success _ ->
                HH.div
                  [ HP.classes
                      [ T.flex
                      , T.justifyCenter
                      , T.itemsCenter
                      , T.m1
                      , T.fontMedium
                      , T.py1
                      , T.px2
                      , T.bgWhite
                      , T.roundedMd
                      , T.textGreen700
                      , T.bgGreen100
                      , T.border
                      , T.borderGreen300
                      ]
                  ]
                  [ HH.div
                      [ HP.classes [ T.text2xl, T.fontNormal, T.maxWFull, T.flexInitial ] ]
                      [ HH.text "Account created :)"
                      , HH.div [ HP.classes [ T.textSm, T.textBase ] ] [ HH.text "Check your email" ]
                      ]
                  ]

              Failure _ -> HH.div [] [ HH.text "Failed" , form ]

              _ -> form
          , HH.p
              [ HP.classes [ T.mt4 ] ]
              [ HH.span [ HP.classes [ T.textGray400 ] ] [HH.text "Already have an account? " ]
              , HH.a
                  [ HP.classes [ T.textDurazno ]
                  , safeHref Register, HE.onClick (Just <<< Navigate Login <<< Mouse.toEvent)
                  ]
                  [ HH.text "Sign in" ]
              ]
          ]
    where
    form = HH.slot F._formless unit formComponent unit (Just <<< HandleRegisterForm)

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

  renderForm { form, submitting } =
    HH.form
      [ HE.onSubmit \ev -> Just $ F.injAction $ Submit ev ]
      [ HH.fieldset
          [ HP.classes [ T.w96, T.maxWFull ] ]
          [ HH.div
              [ HP.classes [ T.grid, T.gridCols2, T.gap4 ] ]
              [ name, slug ]
          , HH.div [ HP.classes [ T.mt4 ] ] [ email ]
          , HH.div [ HP.classes [ T.mt4 ] ] [ password ]
          ]
      , HH.div
          [ HP.classes [ T.mt4 ] ]
          [ Field.submit "Sign up" submitting ]
      ]
    where
    proxies = F.mkSProxies (F.FormProxy :: _ RegisterForm)

    name =
      Field.input (Just "Username") proxies.name form
        [ HP.placeholder "John Doe", HP.type_ HP.InputText ]

    slug =
      Field.input (Just "Slug") proxies.slug form
        [ HP.placeholder "john-doe", HP.type_ HP.InputText ]

    email =
      Field.input (Just "Email address") proxies.email form
        [ HP.placeholder "john.doe@email.com", HP.type_ HP.InputEmail ]

    password =
      Field.input (Just "Password") proxies.password form
        [ HP.placeholder "********", HP.type_ HP.InputPassword ]
