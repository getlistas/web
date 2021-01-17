module Listasio.Page.Register where

import Prelude

import Control.Error.Util (note)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Api.Request (RegisterFields, initGoogleAuth)
import Listasio.Capability.Navigate (class Navigate, navigate, navigate_)
import Listasio.Capability.Resource.User (class ManageUser, googleLoginUser, registerUser)
import Listasio.Component.HTML.Layout as Layout
import Listasio.Component.HTML.Utils (safeHref, whenElem)
import Listasio.Data.Email (Email)
import Listasio.Data.Route (Route(..))
import Listasio.Data.Username (Username)
import Listasio.Form.Field as Field
import Listasio.Form.Validation as V
import Network.RemoteData (RemoteData(..), fromEither, isSuccess)
import Slug (Slug)
import Tailwind as T
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent as Mouse

newtype RegisterForm r f
  = RegisterForm
  ( r
      ( name :: f V.FormError String Username
      , slug :: f V.FormError String Slug
      , email :: f V.FormError String Email
      , password :: f V.FormError String String
      )
  )

derive instance newtypeRegisterForm :: Newtype (RegisterForm r f) _

type State
  = { registration :: RemoteData String Unit }

data Action
  = Initialize
  | HandleRegisterForm RegisterFields
  | HandleGoogleLogin
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
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }
  where
  handleAction = case _ of
    Initialize ->
      void $ H.liftAff $ initGoogleAuth

    HandleRegisterForm fields -> do
      result <- fromEither <$> note "Failed to register" <$> registerUser fields
      H.modify_ _ { registration = result }

    HandleGoogleLogin -> do
      mbProfile <- googleLoginUser
      case mbProfile of
        Nothing -> -- TODO
          pure unit

        Just _ -> navigate Dashboard

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
                      , T.py2
                      , T.px6
                      , T.roundedMd
                      , T.textWhite
                      , T.bgKiwi
                      , T.flex
                      , T.itemsCenter
                      , T.justifyBetween
                      ]
                  ]
                  [ HH.div
                      []
                      [ HH.div [ HP.classes [ T.textXl, T.fontSemibold ] ] [ HH.text "Account created!" ]
                      , HH.div [ HP.classes [ T.textLg, T.fontNormal ] ] [ HH.text "Check your email" ]
                      ]
                  , HH.div
                      [ HP.classes [ T.ml4, T.text5xl ] ]
                      [ HH.text "🎉" ]
                  ]

              Failure _ -> HH.div [] [ HH.text "Failed" , form ]

              _ -> form
          , whenElem (not $ isSuccess registration) \_ ->
              HH.div
                [ HP.classes [ T.w96, T.maxWFull ] ]
                [ HH.div
                    [ HP.classes [ T.flex, T.itemsCenter, T.justifyBetween, T.my4 ] ]
                    [ HH.div [ HP.classes [ T.h0, T.wFull, T.border, T.borderGray200 ] ] []
                    , HH.div [ HP.classes [ T.textGray300, T.mx4, T.leadingNone ] ] [ HH.text "or" ]
                    , HH.div [ HP.classes [ T.h0, T.wFull, T.border, T.borderGray200 ] ] []
                    ]
                , HH.button
                    [ HP.type_ HP.ButtonButton
                    , HE.onClick \_ -> Just HandleGoogleLogin
                    , HP.classes
                        [ T.flex1
                        , T.wFull
                        , T.cursorPointer
                        , T.disabledCursorNotAllowed
                        , T.disabledOpacity50
                        , T.py2
                        , T.px4
                        , T.bgKiwi
                        , T.textWhite
                        , T.roundedMd
                        , T.shadowMd
                        , T.hoverBgOpacity75
                        , T.focusOutlineNone
                        , T.focusRing2
                        , T.focusRingOffset2
                        , T.focusRingOffsetGray10
                        , T.focusRingKiwi
                        ]
                    ]
                    [ HH.text "🇬 Register with Google" ]
                ]
          , whenElem (not $ isSuccess registration) \_ ->
              HH.p
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
          , slug: V.required >>> V.slugFormat
          , email: V.required >>> V.minLength 3 >>> V.emailFormat
          , password: V.required >>> V.minLength 10 >>> V.maxLength 100
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
      Field.input (Just "Name") proxies.name form
        [ HP.placeholder "John Doe", HP.type_ HP.InputText ]

    slug =
      Field.input (Just "Username") proxies.slug form
        [ HP.placeholder "john-doe", HP.type_ HP.InputText ]

    email =
      Field.input (Just "Email address") proxies.email form
        [ HP.placeholder "john.doe@email.com", HP.type_ HP.InputEmail ]

    password =
      Field.input (Just "Password") proxies.password form
        [ HP.placeholder "********", HP.type_ HP.InputPassword ]
