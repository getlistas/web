module Listasio.Component.HTML.Login where

import Prelude

import Listasio.Component.HTML.Icons as Icons
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Api.Request (LoginFields, initGoogleAuth)
import Listasio.Capability.Navigate (class Navigate, navigate, navigate_)
import Listasio.Capability.Resource.User (class ManageUser, googleLoginUser, loginUser)
import Listasio.Component.HTML.Utils (safeHref, whenElem)
import Listasio.Data.Email (Email)
import Listasio.Data.Route (Route(..))
import Listasio.Form.Field as Field
import Listasio.Form.Validation as V
import Network.RemoteData (RemoteData(..), isFailure, isLoading)
import Tailwind as T
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent as Mouse

type Slot = forall query. H.Slot query Output Unit

type ChildSlots
  = ( formless :: F.Slot LoginForm FormQuery () LoginFields Unit )

data Output
  = GoToRegister

data Action
  = Initialize
  | HandleLoginForm LoginFields
  | HandleGoogleLogin
  | Navigate Route Event.Event
  | SwitchToRegister Event.Event

type State
  = { redirect :: Boolean
    , status :: RemoteData String Unit
    }

type Input
  = { redirect :: Boolean
    }

component ::
  forall q m.
  MonadAff m =>
  Navigate m =>
  ManageUser m =>
  H.Component HH.HTML q Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }
  where
  initialState {redirect} = {redirect, status: NotAsked}

  handleAction :: Action -> H.HalogenM State Action ChildSlots Output m Unit
  handleAction = case _ of
    Initialize ->
      void $ H.liftAff $ initGoogleAuth

    HandleLoginForm fields -> do
      { status } <- H.get
      when (not $ isLoading status) $ do
        void $ H.query F._formless unit $ F.injQuery $ SetLoginStatus Loading unit
        H.modify_ _ { status = Loading }

        mbProfile <- loginUser fields

        case mbProfile of
          Nothing -> do
             void $ H.query F._formless unit $ F.injQuery $ SetLoginStatus (Failure "Could not login") unit
             H.modify_ _ { status = Failure "Could not login" }

          Just profile -> do
            void $ H.query F._formless unit $ F.injQuery $ SetLoginStatus (Success unit) unit
            H.modify_ _ { status = Success unit }
            st <- H.get
            when st.redirect (navigate Dashboard)

    HandleGoogleLogin -> do
      { status } <- H.get
      when (not $ isLoading status) do
        void $ H.query F._formless unit $ F.injQuery $ SetLoginStatus Loading unit
        H.modify_ _ { status = Loading }

        mbProfile <- googleLoginUser

        case mbProfile of
          Nothing -> do
            void $ H.query F._formless unit $ F.injQuery $ SetLoginStatus NotAsked unit
            H.modify_ _ { status = NotAsked }

          Just profile -> do
            void $ H.query F._formless unit $ F.injQuery $ SetLoginStatus (Success unit) unit
            H.modify_ _ { status = Success unit }
            st <- H.get
            when st.redirect (navigate Dashboard)

    Navigate route e -> navigate_ e route

    SwitchToRegister e -> do
      H.liftEffect $ Event.preventDefault e
      H.raise GoToRegister

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { status } =
    HH.div
      [ HP.classes [ T.flex, T.flexCol, T.itemsCenter ] ]
      [ HH.button
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
              , T.flex
              , T.justifyCenter
              , T.itemsCenter
              ]
          , HP.disabled $ isLoading status
          ]
          [ Icons.google [ Icons.classes [ T.h5, T.w5, T.mr2, T.flexShrink0 ] ]
          , HH.span [] [ HH.text "Login with Google" ]
          ]
      , HH.div
          [ HP.classes [ T.wFull, T.flex, T.itemsCenter, T.justifyBetween, T.my4 ] ]
          [ HH.div [ HP.classes [ T.h0, T.wFull, T.borderT, T.borderGray200 ] ] []
          , HH.div [ HP.classes [ T.textGray300, T.mx4, T.leadingNone ] ] [ HH.text "Or" ]
          , HH.div [ HP.classes [ T.h0, T.wFull, T.borderT, T.borderGray200 ] ] []
          ]
      , HH.slot F._formless unit formComponent unit (Just <<< HandleLoginForm)
      , HH.p
          [ HP.classes [ T.mt4 ] ]
          [ HH.span [ HP.classes [ T.textGray400 ] ] [HH.text "Don't have an account? " ]
          , HH.a
              [ HP.classes [ T.textDurazno ]
              , safeHref Register, HE.onClick (Just <<< SwitchToRegister <<< Mouse.toEvent)
              ]
              [ HH.text "Sign up" ]
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
  = SetLoginStatus (RemoteData String Unit) a

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
  formInput :: i -> F.Input LoginForm ( status :: RemoteData String Unit ) m
  formInput _ =
    { validators:
        LoginForm
          { email: V.required >>> V.minLength 3 >>> V.emailFormat
          , password: V.required >>> V.maxLength 100
          }
    , initialInputs: Nothing
    , status: NotAsked
    }

  handleEvent = F.raiseResult

  handleAction = case _ of
    Submit event -> do
      { status } <- H.get
      when (not $ isLoading status) do
        H.liftEffect $ Event.preventDefault event
        eval F.submit
    where
    eval act = F.handleAction handleAction handleEvent act

  handleQuery :: forall a. FormQuery a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    SetLoginStatus status a -> do
      H.modify_ _ { status = status }
      pure (Just a)

  proxies = F.mkSProxies (F.FormProxy :: _ LoginForm)

  renderLogin { form, status, submitting } =
    HH.form
      [ HE.onSubmit \ev -> Just $ F.injAction $ Submit ev
      , HP.classes [ T.wFull ]
      ]
      [ HH.fieldset
          []
          [ email
          , HH.div
              [ HP.classes [ T.mt4 ] ]
              [ password ]
          , whenElem (isFailure status) \_ ->
              HH.div
                [ HP.classes [ T.textRed500, T.my4 ] ]
                [ HH.text "Email or password is invalid" ]
          , HH.div
              [ HP.classes [ T.mt4 ] ]
              [ Field.submit "Sign in" (submitting || isLoading status) ]
          ]
      ]

    where
    email =
      Field.input proxies.email form $ Field.defaultProps
        { label = Just "Email address"
        , id = Just "email"
        , placeholder = Just "john.doe@email.com"
        , required = true
        , type_ = HP.InputEmail
        }

    password =
      Field.input proxies.password form $ Field.defaultProps
        { label = Just "Password"
        , id = Just "password"
        , placeholder = Just "********"
        , required = true
        , type_ = HP.InputPassword
        }
