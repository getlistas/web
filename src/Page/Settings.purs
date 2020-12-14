module Doneq.Page.Settings where

import Prelude

import Data.Lens (preview)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Doneq.Capability.Navigate (class Navigate, logout, navigate_)
import Doneq.Capability.Resource.User (class ManageUser, getCurrentUser, updateUser)
import Doneq.Component.HTML.Header (header)
import Doneq.Data.Profile (Profile)
import Doneq.Data.Route (Route(..))
import Doneq.Data.Username (Username)
import Doneq.Data.Username as Username
import Doneq.Form.Field as Field
import Doneq.Form.Validation as V
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), _Success, fromMaybe)
import Tailwind as T
import Web.Event.Event (Event)

newtype SettingsForm r f
  = SettingsForm
  ( r
      ( name :: f V.FormError String Username
      , slug :: f V.FormError String Username
      )
  )

derive instance newtypeSettingsForm :: Newtype (SettingsForm r f) _

data Action
  = Initialize
  | HandleForm Profile
  | LogUserOut
  | Navigate Route Event

type State
  = { profile :: RemoteData String Profile }

component ::
  forall q o m.
  MonadAff m =>
  Navigate m =>
  ManageUser m =>
  H.Component HH.HTML q Unit o m
component =
  H.mkComponent
    { initialState: \_ -> { profile: NotAsked }
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              }
    }
  where
  handleAction = case _ of
    Initialize -> do
      H.modify_ _ { profile = Loading }
      mbProfile <- getCurrentUser
      H.modify_ _ { profile = fromMaybe mbProfile }
      -- if profile cannot be located then something horrible went wrong
      case mbProfile of
        Nothing -> logout
        Just profile -> do
          let
            newInputs =
              F.wrapInputFields
                { name: Username.toString profile.name
                , slug: Username.toString profile.slug
                }
          void $ H.query F._formless unit $ F.asQuery $ F.loadForm newInputs
    HandleForm fields -> do
      updateUser fields
      mbProfile <- getCurrentUser
      H.modify_ _ { profile = fromMaybe mbProfile }
    LogUserOut -> logout
    Navigate route e -> navigate_ e route

  render { profile } =
    container
      [ HH.h1
          []
          [ HH.text "Your Settings" ]
      , HH.slot F._formless unit formComponent unit (Just <<< HandleForm)
      , HH.hr_
      , HH.button
          [ HE.onClick \_ -> Just LogUserOut ]
          [ HH.text "Log out" ]
      ]
    where
    container html =
      HH.div_
        [ header (preview _Success profile) Navigate Settings
        , HH.div
            []
            [ HH.div
                [ HP.classes [ T.container ] ]
                [ HH.div
                    []
                    [ HH.div
                        []
                        html
                    ]
                ]
            ]
        ]

    formComponent :: forall query slots. F.Component SettingsForm query slots Unit Profile m
    formComponent =
      F.component formInput
        $ F.defaultSpec
            { render = renderForm
            , handleEvent = F.raiseResult
            }
      where
      formInput :: Unit -> F.Input' SettingsForm m
      formInput _ =
        { validators:
            SettingsForm
              { name: V.required >>> V.minLength 3 >>> V.maxLength 20 >>> V.usernameFormat
              , slug: V.required >>> V.minLength 3 >>> V.maxLength 20 >>> V.usernameFormat
              }
        , initialInputs: Nothing
        }

      renderForm { form, submitting } =
        HH.form_
          [ HH.fieldset_
              [ name
              , slug
              , Field.submit "Update settings" submitting
              ]
          ]
        where
        proxies = F.mkSProxies (F.FormProxy :: _ SettingsForm)

        name =
          Field.input proxies.name form
            [ HP.placeholder "Your name", HP.type_ HP.InputText ]

        slug =
          Field.input proxies.slug form
            [ HP.placeholder "Your slug", HP.type_ HP.InputText ]
