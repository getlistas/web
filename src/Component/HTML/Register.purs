module Listasio.Component.HTML.Register where

import Prelude

import Data.Either (note)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Api.Request (RegisterFields, initGoogleAuth)
import Listasio.Capability.Analytics (class Analytics, userSet)
import Listasio.Capability.Navigate (class Navigate, navigate, navigate_)
import Listasio.Capability.Resource.User (class ManageUser, googleLoginUser, registerUser)
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Message as Message
import Listasio.Component.HTML.Utils (safeHref, whenElem)
import Listasio.Data.Email (Email)
import Listasio.Data.ID as ID
import Listasio.Data.Route (Route(..))
import Listasio.Data.Username (Username)
import Listasio.Form.Field as Field
import Listasio.Form.Validation as V
import Network.RemoteData (RemoteData(..), fromEither, isFailure, isLoading)
import Tailwind as T
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent as Mouse

_slot :: Proxy "register"
_slot = Proxy

type Slot = forall query. H.Slot query Output Unit

type ChildSlots
  = ( registerForm :: F.Slot RegisterForm FormQuery () RegisterFields Unit )

data Output
  = GoToSignin

type State
  = {status :: RemoteData String Unit}

data Action
  = Initialize
  | HandleRegisterForm RegisterFields
  | HandleGoogleLogin
  | Navigate Route Event.Event
  | SwitchToSignin Event.Event

component ::
  forall q m.
  MonadAff m =>
  ManageUser m =>
  Navigate m =>
  Analytics m =>
  H.Component q Unit Output m
component =
  H.mkComponent
    { initialState: const {status: NotAsked}
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots Output m Unit
  handleAction = case _ of
    Initialize ->
      void $ H.liftAff $ initGoogleAuth

    HandleRegisterForm fields -> do
      {status} <- H.get
      when (not $ isLoading status) do
        void $ H.query _form unit $ F.injQuery $ SetRegisterStatus Loading unit
        H.modify_ _ {status = Loading}

        result <- fromEither <$> note "Failed to register" <$> registerUser fields

        H.modify_ _ {status = unit <$ result}
        void $ H.query _form unit $ F.injQuery $ SetRegisterStatus (unit <$ result) unit

    HandleGoogleLogin -> do
      {status} <- H.get
      when (not $ isLoading status) do
        void $ H.query _form unit $ F.injQuery $ SetRegisterStatus Loading unit
        H.modify_ _ {status = Loading}

        mbProfile <- googleLoginUser

        case mbProfile of
          Nothing -> do
            void $ H.query _form unit $ F.injQuery $ SetRegisterStatus NotAsked unit
            H.modify_ _ {status = NotAsked}

          Just {email, id} -> do
            userSet {email: unwrap email, userId: ID.toString id}
            navigate Dashboard

    Navigate route e -> navigate_ e route

    SwitchToSignin e -> do
      H.liftEffect $ Event.preventDefault e
      H.raise GoToSignin

  render :: State -> H.ComponentHTML Action ChildSlots m
  render {status} =
    case status of
      Success _ ->
        HH.div
          [ HP.classes [ T.flex, T.flexCol, T.itemsCenter ] ]
          [ Message.message $ Message.props
              { title = Just "Account created!"
              , text = Just "Check your email"
              , icon = Just "🎉"
              }
          ]

      _ ->
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
                  , T.hoverBgKiwiDark
                  , T.focusOutlineNone
                  , T.focusRing2
                  , T.focusRingOffset2
                  , T.focusRingKiwi
                  , T.flex
                  , T.justifyCenter
                  , T.itemsCenter
                  ]
              , HP.disabled $ isLoading status
              ]
              [ Icons.google [ Icons.classes [ T.h5, T.w5, T.mr2, T.flexShrink0 ] ]
              , HH.span [] [ HH.text "Register with Google" ]
              ]
          , HH.div
              [ HP.classes [ T.wFull, T.flex, T.itemsCenter, T.justifyBetween, T.my4 ] ]
              [ HH.div [ HP.classes [ T.h0, T.wFull, T.borderT, T.borderGray200 ] ] []
              , HH.div [ HP.classes [ T.textGray300, T.mx4, T.leadingNone ] ] [ HH.text "Or" ]
              , HH.div [ HP.classes [ T.h0, T.wFull, T.borderT, T.borderGray200 ] ] []
              ]
          , HH.slot _form unit formComponent unit HandleRegisterForm
          , HH.p
              [ HP.classes [ T.mt4 ] ]
              [ HH.span [ HP.classes [ T.textGray400 ] ] [HH.text "Already have an account? " ]
              , HH.a
                  [ HP.classes [ T.textDurazno ]
                  , safeHref Register
                  , HE.onClick $ SwitchToSignin <<< Mouse.toEvent
                  ]
                  [ HH.text "Sign in" ]
              ]
          ]

_form = Proxy :: Proxy "registerForm"

newtype RegisterForm (r :: Row Type -> Type) f = RegisterForm (r (FormRow f))
derive instance Newtype (RegisterForm r f) _

type FormRow :: (Type -> Type -> Type -> Type) -> Row Type
type FormRow f =
  ( name :: f V.FormError String Username
  , email :: f V.FormError String Email
  , password :: f V.FormError String String
  )

data FormQuery a
  = SetRegisterStatus (RemoteData String Unit) a

derive instance functorFormQuery :: Functor FormQuery

data FormAction
  = Submit Event.Event

formComponent ::
  forall formSlots formInput m.
  MonadAff m =>
  F.Component RegisterForm FormQuery formSlots formInput RegisterFields m
formComponent =
  F.component formInput $ F.defaultSpec
    { render = renderForm
    , handleQuery = handleQuery
    , handleEvent = handleEvent
    , handleAction = handleAction
    }
  where
  formInput :: formInput -> F.Input RegisterForm ( status :: RemoteData String Unit ) m
  formInput _ =
    { validators:
        RegisterForm
          { name: V.required >>> V.usernameFormat
          , email: V.required >>> V.minLength 3 >>> V.emailFormat
          , password: V.required >>> V.minLength 10 >>> V.maxLength 100
          }
    , initialInputs: Nothing
    , status: NotAsked
    }

  handleEvent = F.raiseResult

  handleAction = case _ of
    Submit event -> do
      H.liftEffect $ Event.preventDefault event
      {status} <- H.get
      when (not $ isLoading status) do eval F.submit

    where
    eval act = F.handleAction handleAction handleEvent act

  handleQuery :: forall a. FormQuery a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    SetRegisterStatus status a -> do
      H.modify_ _ {status = status}
      pure (Just a)

  renderForm {form, status, submitting} =
    HH.form
      [ HE.onSubmit $ F.injAction <<< Submit
      , HP.noValidate true
      , HP.classes [ T.wFull ]
      ]
      [ HH.fieldset
          []
          [ HH.div [ HP.classes [] ] [ name ]
          , HH.div [ HP.classes [ T.mt4 ] ] [ email ]
          , HH.div [ HP.classes [ T.mt4 ] ] [ password ]
          ]
      , whenElem (isFailure status) \_ ->
          HH.div
            [ HP.classes [ T.textRed500, T.my4 ] ]
            -- TODO better error based on what went wrong
            [ HH.text "Could not register :(" ]
      , HH.div
          [ HP.classes [ T.mt4 ] ]
          [ Field.submit "Sign up" (submitting || isLoading status) ]
      ]
    where
    proxies = F.mkSProxies (Proxy :: Proxy RegisterForm)

    name =
      Field.input proxies.name form $ Field.defaultProps
        { label = Just "Name"
        , id = Just "name"
        , placeholder = Just "John Doe"
        , required = true
        }

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
