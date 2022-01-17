module Listasio.Component.HTML.Login where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Api.Request (LoginFields, initGoogleAuth)
import Listasio.Capability.Analytics (class Analytics, userSet)
import Listasio.Capability.Navigate (class Navigate, navigate, navigate_)
import Listasio.Capability.Resource.User (class ManageUser, googleLoginUser, loginUser)
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Utils (safeHref, whenElem)
import Listasio.Data.Email (Email)
import Listasio.Data.ID as ID
import Listasio.Data.Route (Route(..))
import Listasio.Form.Field as Field
import Listasio.Form.Validation as V
import Network.RemoteData (RemoteData(..), isFailure, isLoading)
import Tailwind as T
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent as Mouse

_slot :: Proxy "login"
_slot = Proxy

type Slot = forall query. H.Slot query Output Unit

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( email :: f String V.FormError Email
  , password :: f String V.FormError String
  )

type FormInputs = { | Form F.FieldInput }

type Query :: Type -> Type
type Query = Const Void

type Input = { redirect :: Boolean }

data Output = GoToRegister

type FormContext
  = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Input Action

type FormlessAction = F.FormlessAction (Form F.FieldState)

data Action
  = Initialize
  | HandleLoginForm LoginFields
  | HandleGoogleLogin
  | Navigate Route Event.Event
  | SwitchToRegister Event.Event
  -- Formless actions
  | Receive FormContext
  | Eval FormlessAction

type State
  =
  { context :: FormContext
  , status :: RemoteData String Unit
  }

component
  :: forall q m
   . MonadAff m
  => Navigate m
  => ManageUser m
  => Analytics m
  => H.Component q Input Output m
component = F.formless { liftAction: Eval } mempty $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { initialize = Just Initialize
      , receive = Just <<< Receive
      , handleAction = handleAction
      , handleQuery = handleQuery
      }
  }
  where
  initialState context = { context, status: NotAsked }

  handleAction :: Action -> H.HalogenM _ _ _ _ m Unit
  handleAction = case _ of
    Initialize ->
      void $ H.liftAff $ initGoogleAuth

    HandleLoginForm fields -> do
      { status } <- H.get
      when (not $ isLoading status) $ do
        H.modify_ _ { status = Loading }

        mbProfile <- loginUser fields

        case mbProfile of
          Nothing -> do
            H.modify_ _ { status = Failure "Could not login" }

          Just { email, id } -> do
            userSet { email: unwrap email, userId: ID.toString id }
            H.modify_ _ { status = Success unit }
            st <- H.get
            when st.context.input.redirect (navigate Dashboard)

    HandleGoogleLogin -> do
      { status } <- H.get
      when (not $ isLoading status) do
        H.modify_ _ { status = Loading }

        mbProfile <- googleLoginUser

        case mbProfile of
          Nothing -> do
            H.modify_ _ { status = NotAsked }

          Just { email, id } -> do
            userSet { email: unwrap email, userId: ID.toString id }
            H.modify_ _ { status = Success unit }
            st <- H.get
            when st.context.input.redirect (navigate Dashboard)

    Navigate route e -> navigate_ e route

    SwitchToRegister e -> do
      H.liftEffect $ Event.preventDefault e
      F.raise GoToRegister

    -- Formless actions

    Receive context -> H.modify_ _ { context = context }

    Eval action -> F.eval action

  handleQuery :: forall a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ m (Maybe a)
  handleQuery =
    F.handleSubmitValidate (handleAction <<< HandleLoginForm) F.validate
      { email: V.emailFormat <=< V.required <=< V.minLength 3
      , password: V.required <=< V.maxLength 100
      }

  render :: State -> H.ComponentHTML Action () m
  render { status, context: { formActions, fields, actions } } =
    HH.div
      [ HP.classes [ T.flex, T.flexCol, T.itemsCenter ] ]
      [ HH.button
          [ HP.type_ HP.ButtonButton
          , HE.onClick $ const HandleGoogleLogin
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
      , form
      , HH.p
          [ HP.classes [ T.mt4 ] ]
          [ HH.span [ HP.classes [ T.textGray400 ] ] [ HH.text "Don't have an account? " ]
          , HH.a
              [ HP.classes [ T.textDurazno ]
              , safeHref Register
              , HE.onClick $ SwitchToRegister <<< Mouse.toEvent
              ]
              [ HH.text "Sign up" ]
          ]
      ]
    where
    form =
      HH.form
        [ HE.onSubmit formActions.handleSubmit
        , HP.noValidate true
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
                [ Field.submit "Sign in" (isLoading status) ]
            ]
        ]

    email =
      Field.input fields.email actions.email $ Field.defaultProps
        { label = Just "Email address"
        , id = Just "email"
        , placeholder = Just "john.doe@email.com"
        , required = true
        , type_ = HP.InputEmail
        }

    password =
      Field.input fields.password actions.password $ Field.defaultProps
        { label = Just "Password"
        , id = Just "password"
        , placeholder = Just "********"
        , required = true
        , type_ = HP.InputPassword
        }
