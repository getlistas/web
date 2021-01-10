module Listasio.Page.Login where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Api.Request (LoginFields)
import Listasio.Capability.Navigate (class Navigate, navigate, navigate_)
import Listasio.Capability.Resource.User (class ManageUser, loginUser)
import Listasio.Component.HTML.Layout as Layout
import Listasio.Component.HTML.Utils (safeHref, whenElem)
import Listasio.Data.Email (Email)
import Listasio.Data.Route (Route(..))
import Listasio.Form.Field as Field
import Listasio.Form.Validation as V
import Tailwind as T
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent as Mouse

data Action
  = HandleLoginForm LoginFields
  | Navigate Route Event.Event

type State
  = { redirect :: Boolean
    , success :: Boolean
    }

type Input
  = { redirect :: Boolean
    , success :: Boolean
    }

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
      mbProfile <- loginUser fields
      case mbProfile of
        Nothing -> void $ H.query F._formless unit $ F.injQuery $ SetLoginError true unit

        Just profile -> do
          void $ H.query F._formless unit $ F.injQuery $ SetLoginError false unit
          st <- H.get
          when st.redirect (navigate Dashboard)

    Navigate route e -> navigate_ e route

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { success } =
    Layout.noheader
      Nothing
      Navigate
      (Just Login)
      $ HH.div
          [ HP.classes [ T.mt12, T.flex, T.flexCol, T.itemsCenter ] ]
          [ HH.h1
              [ HP.classes [ T.textGray400, T.text2xl, T.fontBold, T.mb6 ] ]
              [ HH.text "Welcome to Listas" ]
          , whenElem success \_ ->
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
                    [ HH.text "Registration succesful :)"
                    , HH.div [ HP.classes [ T.textSm, T.textBase ] ] [ HH.text "Login to start using the app" ]
                    ]
                ]
          , HH.slot F._formless unit formComponent unit (Just <<< HandleLoginForm)
          , HH.p
              [ HP.classes [ T.mt4 ] ]
              [ HH.span [ HP.classes [ T.textGray400 ] ] [HH.text "Don't have an account? " ]
              , HH.a
                  [ HP.classes [ T.textDurazno ]
                  , safeHref Register, HE.onClick (Just <<< Navigate Register <<< Mouse.toEvent)
                  ]
                  [ HH.text "Sign up for free" ]
              ]
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
          , password: V.required >>> V.minLength 1
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

  renderLogin { form, loginError, submitting } =
    HH.form
      [ HE.onSubmit \ev -> Just $ F.injAction $ Submit ev ]
      [ whenElem loginError \_ ->
          HH.div
            [ HP.classes [ T.textRed500 ] ]
            [ HH.text "Email or password is invalid" ]
      , HH.fieldset
          [ HP.classes [ T.w96, T.maxWFull ] ]
          [ Field.input (Just "Email address") proxies.email form
              [ HP.placeholder "jonh.doe@email.com"
              , HP.type_ HP.InputEmail
              ]
          , HH.div
              [ HP.classes [ T.mt4 ] ]
              [ Field.input (Just "Password") proxies.password form
                  [ HP.placeholder "********"
                  , HP.type_ HP.InputPassword
                  ]
                ]
          , HH.div
              [ HP.classes [ T.mt4 ] ]
              [ Field.submit "Sign in" submitting ]
          ]
      ]
