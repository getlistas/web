module Listasio.Page.CreateResource where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Either (Either, note)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Listasio.Capability.Navigate (class Navigate, navigate, navigate_)
import Listasio.Capability.Resource.List (class ManageList, getLists)
import Listasio.Capability.Resource.Resource (class ManageResource)
import Listasio.Component.HTML.CreateResource as CreateResource
import Listasio.Component.HTML.Layout as Layout
import Listasio.Data.List (ListWithIdAndUser)
import Listasio.Data.Profile (Profile)
import Listasio.Data.Route (Route(..))
import Listasio.Env (UserEnv)
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RemoteData
import Tailwind as T
import Web.Event.Event (Event)

data Action
  = Initialize
  | Receive { currentUser :: Maybe Profile, url :: Maybe String }
  | Navigate Route Event
  | LoadLists
  | HandleCreateResource CreateResource.Output

type Input
  = { url :: Maybe String }

type State
  = { currentUser :: Maybe Profile
    , lists :: RemoteData String (Array ListWithIdAndUser)
    , url :: Maybe String
    }

type ChildSlots
  = ( createResource :: CreateResource.Slot
    )

noteError :: forall a. Maybe a -> Either String a
noteError = note "Could not fetch your lists"

component
  :: forall q o m r
   . MonadAff m
  => MonadAsk { userEnv :: UserEnv | r } m
  => ManageList m
  => ManageResource m
  => Navigate m
  => H.Component HH.HTML q Input o m
component = Connect.component $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      , initialize = Just Initialize
      }
  }
  where
  initialState { currentUser, url } =
    { currentUser
    , lists: NotAsked
    , url
    }

  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> void $ H.fork $ handleAction LoadLists

    Receive { currentUser } -> H.modify_ _ { currentUser = currentUser }

    Navigate route e -> navigate_ e route

    LoadLists -> do
      H.modify_ _ { lists = Loading }
      lists <- RemoteData.fromEither <$> noteError <$> getLists
      H.modify_ _ { lists = lists }

    HandleCreateResource (CreateResource.Created resource) ->
      navigate Dashboard

    HandleCreateResource (CreateResource.Cancel) ->
      navigate Dashboard

  render :: State -> H.ComponentHTML Action ChildSlots m
  render st =
    Layout.dashboard
      st.currentUser
      Navigate
      Nothing
      $ HH.div
          []
          [ header
          , HH.div [ HP.classes [ T.container, T.flex, T.justifyCenter ] ] [ form ]
          ]
    where
    header =
      HH.div
        [ HP.classes [ T.flex, T.justifyBetween, T.wFull ] ]
        [ HH.h1
            [ HP.classes [ T.textGray400, T.mb6, T.text4xl, T.fontBold ] ]
            [ HH.text "CreateResource" ]
        ]

    form = case st.lists of
      Success lists ->
        HH.div
          [ HP.classes [ T.bgWhite, T.wFull, T.maxWLg ] ]
          [ HH.slot CreateResource._createResource unit CreateResource.component { lists, url: st.url } (Just <<< HandleCreateResource) ]

      Failure msg ->
        HH.div
          [ HP.classes [ T.p4, T.border4, T.borderRed600, T.bgRed200, T.textRed900 ] ]
          [ HH.p [ HP.classes [ T.fontBold, T.textLg ] ] [ HH.text "Error =(" ]
          , HH.p_ [ HH.text msg ]
          ]

      _ -> HH.div [ HP.classes [ T.textCenter ] ] [ HH.text "Loading ..." ]