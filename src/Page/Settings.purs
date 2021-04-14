module Listasio.Page.Settings where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Lens (_Just, set)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Capability.Navigate (class Navigate, logout, navigate_)
import Listasio.Capability.Resource.User (class ManageUser, updateUser)
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Utils (whenElem)
import Listasio.Data.Lens (_currentUser, _name, _slug)
import Listasio.Data.Profile (Profile, ProfileWithIdAndEmail)
import Listasio.Data.Route (Route)
import Listasio.Data.Username (Username)
import Listasio.Data.Username as Username
import Listasio.Env (UserEnv)
import Listasio.Form.Field as Field
import Listasio.Form.Validation as V
import Slug (Slug)
import Slug as Slug
import Tailwind as T
import Web.Event.Event (Event)
import Web.Event.Event as Event

newtype SettingsForm r f
  = SettingsForm
  ( r
      ( name :: f V.FormError String Username
      , slug :: f V.FormError String Slug
      )
  )

derive instance newtypeSettingsForm :: Newtype (SettingsForm r f) _

data Action
  = Receive { currentUser :: Maybe ProfileWithIdAndEmail }
  | HandleForm Profile
  | LogUserOut
  | Navigate Route Event

type State
  = { currentUser :: Maybe ProfileWithIdAndEmail }

component ::
  forall q o r m.
  MonadAff m =>
  MonadAsk { userEnv :: UserEnv | r } m =>
  Navigate m =>
  ManageUser m =>
  H.Component HH.HTML q {} o m
component = Connect.component $ H.mkComponent
  { initialState
  , render
  , eval:
      H.mkEval
        $ H.defaultEval
            { handleAction = handleAction
            , receive = Just <<< Receive
            }
  }
  where
  initialState { currentUser } = { currentUser }

  handleAction = case _ of
    Receive { currentUser } -> do
      H.modify_ _ { currentUser = currentUser }
      case currentUser of
       -- if we dont' have a profile something went completely wrong
       Nothing -> logout
       Just profile -> do
         let
           newInputs =
             F.wrapInputFields
               { name: Username.toString profile.name
               , slug: Slug.toString profile.slug
               }
         void $ H.query F._formless unit $ F.asQuery $ F.loadForm newInputs

    HandleForm {name, slug} -> do
      -- TODO: check response !!!
      updateUser {name, slug}
      H.modify_ $ set (_currentUser <<< _Just <<< _slug) slug
                    <<< set (_currentUser <<< _Just <<< _name) name

    LogUserOut -> logout

    Navigate route e -> navigate_ e route

  render { currentUser } =
    HH.div
      []
      [ HH.div
          [ HP.classes [ T.pt2 ] ]
          [ HH.h1
              [ HP.classes [ T.textGray400, T.mb6, T.text4xl, T.fontBold ] ]
              [ HH.text "Settings" ]
          ]
      , HH.div
          []
          [ whenElem false \_ -> HH.slot F._formless unit formComponent unit (Just <<< HandleForm)
          , HH.div
              [ HP.classes [ T.mt8 ] ]
              [ HH.button
                  [ HE.onClick \_ -> Just LogUserOut
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

data FormAction
  = Submit Event.Event

formComponent :: forall m query slots.
  MonadAff m =>
  F.Component SettingsForm query slots Unit Profile m
formComponent =
  F.component formInput
    $ F.defaultSpec
        { render = renderForm
        , handleEvent = handleEvent
        , handleAction = handleAction
        }
  where
  formInput :: Unit -> F.Input' SettingsForm m
  formInput _ =
    { validators:
        SettingsForm
          { name: V.required >>> V.minLength 3 >>> V.maxLength 20 >>> V.usernameFormat
          , slug: V.required >>> V.minLength 3 >>> V.maxLength 20 >>> V.slugFormat
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
      [ HE.onSubmit $ Just <<< F.injAction <<< Submit
      , HP.noValidate true
      ]
      [ HH.fieldset_
          [ name
          , HH.div [ HP.classes [ T.mt4 ] ] [ slug ]
          , HH.div
              [ HP.classes [ T.mt4 ] ]
              [ Field.submit "Update settings" submitting ]
          ]
      ]
    where
    proxies = F.mkSProxies (F.FormProxy :: _ SettingsForm)

    name =
      Field.input proxies.name form $ Field.defaultProps
        { label = Just "Your name"
        , id = Just "name"
        , placeholder = Just "John Doe"
        }

    slug =
      Field.input proxies.slug form $ Field.defaultProps
        { label = Just "Your slug"
        , id = Just "slug"
        , placeholder = Just "john-doe"
        }
