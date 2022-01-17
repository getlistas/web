module Listasio.Page.Settings where

import Prelude

import Data.Lens (_Just, set)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
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
import Listasio.Store as Store
import Tailwind as T
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)

_slot :: Proxy "settings"
_slot = Proxy


data Action
  = Receive (Connected (Maybe ProfileWithIdAndEmail) Unit)
  | HandleForm Profile
  | LogUserOut
  | Navigate Route Event

type State
  = { currentUser :: Maybe ProfileWithIdAndEmail }

component
  :: forall q o m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => ManageUser m
  => H.Component q Unit o m
component = connect (selectEq _.currentUser) $ H.mkComponent
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
  initialState { context: currentUser } = { currentUser }

  handleAction = case _ of
    Receive { context: currentUser } -> do
      H.modify_ _ { currentUser = currentUser }
      case currentUser of
        -- if we dont' have a profile something went completely wrong
        Nothing -> logout
        Just _ -> pure unit
        -- Just profile ->
        --   void $ H.query _form unit $ F.asQuery $ F.loadForm $ F.wrapInputFields
        --     { name: Username.toString profile.name
        --     , slug: Slug.toString profile.slug
        --     }

    HandleForm { name, slug } -> do
      -- TODO: check response !!!
      updateUser { name, slug }
      H.modify_ $ set (_currentUser <<< _Just <<< _slug) slug
        <<< set (_currentUser <<< _Just <<< _name) name

    LogUserOut -> logout

    Navigate route e -> navigate_ e route

  render _ =
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
          [ whenElem false \_ -> HH.text "form goes here xD (HH.slot _form unit formComponent unit HandleForm)"
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

-- type FormRow :: (Type -> Type -> Type -> Type) -> Row Type
-- type FormRow f =
--   ( name :: f V.FormError String Username
--   , slug :: f V.FormError String Slug
--   )

-- _form = Proxy :: Proxy "settingsForm"

-- data FormAction
--   = Submit Event.Event

-- formComponent
--   :: forall m query slots
--    . MonadAff m
--   => F.Component Form query slots Unit Profile m
-- formComponent =
--   F.component formInput
--     $ F.defaultSpec
--         { render = renderForm
--         , handleEvent = handleEvent
--         , handleAction = handleAction
--         }
--   where
--   formInput :: Unit -> F.Input' Form m
--   formInput _ =
--     { validators:
--         Form
--           { name: V.required >>> V.minLength 3 >>> V.maxLength 20 >>> V.usernameFormat
--           , slug: V.required >>> V.minLength 3 >>> V.maxLength 20 >>> V.slugFormat
--           }
--     , initialInputs: Nothing
--     }

--   handleEvent = F.raiseResult

--   handleAction = case _ of
--     Submit event -> do
--       H.liftEffect $ Event.preventDefault event
--       eval F.submit

--     where
--     eval act = F.handleAction handleAction handleEvent act

--   renderForm { form, submitting } =
--     HH.form
--       [ HE.onSubmit $ F.injAction <<< Submit
--       , HP.noValidate true
--       ]
--       [ HH.fieldset_
--           [ name
--           , HH.div [ HP.classes [ T.mt4 ] ] [ slug ]
--           , HH.div
--               [ HP.classes [ T.mt4 ] ]
--               [ Field.submit "Update settings" submitting ]
--           ]
--       ]
--     where
--     proxies = F.mkSProxies (Proxy :: Proxy Form)

--     name =
--       Field.input proxies.name form $ Field.defaultProps
--         { label = Just "Your name"
--         , id = Just "name"
--         , placeholder = Just "John Doe"
--         }

--     slug =
--       Field.input proxies.slug form $ Field.defaultProps
--         { label = Just "Your slug"
--         , id = Just "slug"
--         , placeholder = Just "john-doe"
--         }
