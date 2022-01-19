module Listasio.Page.Settings where

import Prelude

import Data.Lens (_Just, set)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Listasio.Capability.Navigate (class Navigate, logout, navigate_)
import Listasio.Capability.Resource.User (class ManageUser, updateUser)
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Utils (whenElem)
import Listasio.Component.HTML.Wip as Wip
import Listasio.Data.Lens (_currentUser, _name, _slug)
import Listasio.Data.Profile (Profile, ProfileWithIdAndEmail)
import Listasio.Data.Route (Route)
import Listasio.Data.Username (Username)
import Listasio.Data.Username as Username
import Listasio.Form.Field as Field
import Listasio.Form.Validation as V
import Listasio.Store as Store
import Slug (Slug)
import Slug as Slug
import Tailwind as T
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)

_slot :: Proxy "settings"
_slot = Proxy

type Slot = forall query. H.Slot query Output Input

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( name :: f String V.FormError Username
  , slug :: f String V.FormError Slug
  )

type FormInputs = { | Form F.FieldInput }

type Input = Unit

type ConnectedInput = Connected (Maybe ProfileWithIdAndEmail) Input

type Output = Void

type FormContext
  = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) ConnectedInput Action

type FormlessAction = F.FormlessAction (Form F.FieldState)

data Action
  = HandleForm Profile
  | LogUserOut
  | Navigate Route Event
  -- Formless actions
  | Receive FormContext
  | Eval FormlessAction

type State
  = { context :: FormContext
    , currentUser :: Maybe ProfileWithIdAndEmail }

component
  :: forall q o m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => ManageUser m
  => H.Component q Unit o m
component = connect (selectEq _.currentUser)
    $ F.formless { liftAction: Eval } mempty
    $ H.mkComponent
        { initialState
        , render
        , eval:
            H.mkEval
              $ H.defaultEval
                  { receive = Just <<< Receive
                  , handleAction = handleAction
                  , handleQuery = handleQuery
                  }
        }
  where
  initialState context = { context, currentUser: context.input.context }

  handleAction :: Action -> H.HalogenM _ _ _ _ m Unit
  handleAction = case _ of
    HandleForm { name, slug } -> do
      -- TODO: check response !!!
      updateUser { name, slug }
      H.modify_ $ set (_currentUser <<< _Just <<< _slug) slug
        <<< set (_currentUser <<< _Just <<< _name) name

    LogUserOut -> logout

    Navigate route e -> navigate_ e route

    -- Formless actions
    Receive context -> do
      let newUser = context.input.context
      {currentUser} <- H.get
      H.modify_ _ { context = context, currentUser = newUser }
      case currentUser, newUser of
        -- if we dont' have a profile something went completely wrong
        _, Nothing -> logout
        Just current, Just new | current /= new ->
          handleAction $ context.formActions.setFields $ F.mkFieldStates
            { name: Username.toString new.name
            , slug: Slug.toString new.slug
            }
        _, _ -> pure unit

    Eval action -> F.eval action

  handleQuery :: forall a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ m (Maybe a)
  handleQuery =
    F.handleSubmitValidate (handleAction <<< HandleForm) F.validate
      { name: V.usernameFormat <=< V.required <=< V.minLength 2 <=< V.maxLength 20
      , slug: V.slugFormat <=< V.required <=< V.minLength 2 <=< V.maxLength 20
      }

  render { context: { formActions, fields, actions } } =
    HH.div
      []
      [ HH.div
          [ HP.classes [ T.pt2 ] ]
          [ HH.h1
              [ HP.classes [ T.textGray400, T.mb6, T.text4xl, T.fontBold ] ]
              [ HH.text "Settings" ]
          ]
      , Wip.elem
      , HH.div
          []
          [ -- TODO: show the form
            whenElem false \_ -> form
          , HH.div
              [ HP.classes [ T.mt8 ] ]
              [ HH.button
                  [ HE.onClick $ const LogUserOut
                  , HP.classes
                      [ T.cursorPointer
                      , T.py2
                      , T.px4
                      , T.bgGray300
                      , T.textWhite
                      , T.fontSemibold
                      , T.roundedLg
                      , T.shadowMd
                      , T.hoverBgKiwi
                      , T.focusOutlineNone
                      , T.focusRing2
                      , T.focusRingOffset2
                      , T.focusRingKiwiDark
                      , T.flex
                      , T.itemsCenter
                      ]
                  ]
                  [ HH.span [] [ HH.text "Log out" ]
                  , Icons.logout [ Icons.classes [ T.h5, T.w5, T.ml2 ] ]
                  ]
              ]
          ]
      ]
    where
    form =
      HH.form
        [ HE.onSubmit formActions.handleSubmit
        , HP.noValidate true
        ]
        [ HH.fieldset_
            [ name
            , HH.div [ HP.classes [ T.mt4 ] ] [ slug ]
            , HH.div
                [ HP.classes [ T.mt4 ] ]
                [ -- TODO: disable while submitting or not changed
                  Field.submit "Update settings" true
                ]
            ]
        ]

    name =
      Field.input fields.name actions.name $ Field.defaultProps
        { label = Just "Your name"
        , id = Just "name"
        , placeholder = Just "John Doe"
        }

    slug =
      Field.input fields.slug actions.slug $ Field.defaultProps
        { label = Just "Your slug"
        , id = Just "slug"
        , placeholder = Just "john-doe"
        }
