module Listasio.Page.Dashboard where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Array (null)
import Data.Either (Either, note)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Capability.Resource.List (class ManageList, getLists)
import Listasio.Component.HTML.Header (header)
import Listasio.Component.HTML.Utils (maybeElem, whenElem)
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
  | Receive { currentUser :: Maybe Profile }
  | Navigate Route Event
  | LoadLists

type State
  = { currentUser :: Maybe Profile
    , lists :: RemoteData String (Array ListWithIdAndUser)
    }

noteError :: forall a. Maybe a -> Either String a
noteError = note "Could not fetch your lists"

component
  :: forall q o m r
   . MonadAff m
  => MonadAsk { userEnv :: UserEnv | r } m
  => ManageList m
  => Navigate m
  => H.Component HH.HTML q {} o m
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
  initialState { currentUser } = { currentUser, lists: NotAsked }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> void $ H.fork $ handleAction LoadLists

    Receive { currentUser } -> H.modify_ _ { currentUser = currentUser }

    Navigate route e -> navigate_ e route

    LoadLists -> do
      H.modify_ _ { lists = Loading }
      lists <- RemoteData.fromEither <$> noteError <$> getLists
      H.modify_ _ { lists = lists }

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { currentUser, lists } =
    HH.div
      [ HP.classes [ T.minHScreen, T.wScreen, T.flex, T.flexCol, T.itemsCenter ] ]
      [ header currentUser Navigate Dashboard
      , HH.div [ HP.classes [ T.mt10 ] ] [ feed ]
      ]
    where
    feed = case lists of
      Success items ->
        HH.div
          [ HP.classes [ T.flex ] ]
          $ map listInfo items
      Failure msg ->
        HH.div
          [ HP.classes [ T.p4, T.border4, T.borderRed600, T.bgRed200, T.textRed900 ] ]
          [ HH.p [ HP.classes [ T.fontBold, T.textLg ] ] [ HH.text "Error =(" ]
          , HH.p_ [ HH.text msg ]
          ]
      _ -> HH.div [ HP.classes [ T.textCenter ] ] [ HH.text "Loading ..." ]

    listInfo :: ListWithIdAndUser -> H.ComponentHTML Action slots m
    listInfo { title, description, tags } =
      HH.div
        [ HP.classes [ T.m4, T.p2, T.border4, T.borderIndigo400 ] ]
        [ HH.div [ HP.classes [ T.textLg, T.borderB2, T.borderGray200, T.mb4 ] ] [ HH.text title ]
        , maybeElem description \des -> HH.div [ HP.classes [ T.textSm, T.mb4 ] ] [ HH.text des ]
        , whenElem (not $ null tags) \_ ->
            HH.div
              [ HP.classes [ T.flex, T.textSm ] ]
              $ map ((\t -> HH.div [ HP.classes [ T.mr2 ] ] [ HH.text t ]) <<< ("#" <> _)) tags
        ]
