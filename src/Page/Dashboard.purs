module Listasio.Page.Dashboard where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Array (snoc)
import Data.Either (Either, note)
import Data.Maybe (Maybe(..), isJust)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Capability.Resource.List (class ManageList, getLists)
import Listasio.Capability.Resource.Resource (class ManageResource)
import Listasio.Component.HTML.CreateResource as CreateResource
import Listasio.Component.HTML.Layout as Layout
import Listasio.Component.HTML.List as List
import Listasio.Component.HTML.Utils (safeHref, whenElem)
import Listasio.Data.List (ListWithIdAndUser)
import Listasio.Data.Profile (Profile)
import Listasio.Data.Route (Route(..))
import Listasio.Env (UserEnv)
import Network.RemoteData (RemoteData(..), toMaybe)
import Network.RemoteData as RemoteData
import Tailwind as T
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent as Mouse

data Action
  = Initialize
  | Receive { currentUser :: Maybe Profile }
  | Navigate Route Event
  | LoadLists
  | ToggleCreateResource
  | HandleCreateResource CreateResource.Output

type State
  = { currentUser :: Maybe Profile
    , lists :: RemoteData String (Array ListWithIdAndUser)
    , showCreateResource :: Boolean
    }

type ChildSlots
  = ( createResource :: CreateResource.Slot
    , list :: List.Slot
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
  initialState { currentUser } =
    { currentUser, lists: NotAsked, showCreateResource: false }

  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> void $ H.fork $ handleAction LoadLists

    Receive { currentUser } -> H.modify_ _ { currentUser = currentUser }

    Navigate route e -> navigate_ e route

    LoadLists -> do
      H.modify_ _ { lists = Loading }
      lists <- RemoteData.fromEither <$> noteError <$> getLists
      H.modify_ _ { lists = lists }

    HandleCreateResource (CreateResource.Created resource) -> do
      void $ H.query List._list resource.list."$oid" $ H.tell $ List.ResourceAdded resource
      H.modify_ _ { showCreateResource = false }

    HandleCreateResource (CreateResource.Cancel) ->
      H.modify_ _ { showCreateResource = false }

    ToggleCreateResource ->
      H.modify_ \s -> s { showCreateResource = not s.showCreateResource }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render st =
    Layout.dashboard
      st.currentUser
      Navigate
      (Just Dashboard)
      $ HH.div
          []
          [ header
          , HH.div [ HP.classes [ T.container ] ] [ feed ]
          ]
    where
    header =
      HH.div
        [ HP.classes [ T.flex, T.justifyBetween, T.wFull ] ]
        [ HH.h1
            [ HP.classes [ T.textGray400, T.mb6, T.text4xl, T.fontBold ] ]
            [ HH.text "Read Next" ]
        , whenElem (not st.showCreateResource && isJust (toMaybe st.lists)) \_ ->
            HH.div
              []
              [  HH.button
                [ HE.onClick \_ -> Just $ ToggleCreateResource
                , HP.classes
                    [ T.flexNone
                    , T.cursorPointer
                    , T.leadingNormal
                    , T.py2
                    , T.px4
                    , T.bgDuraznoLight
                    , T.textWhite
                    , T.fontSemibold
                    , T.roundedMd
                    , T.shadowMd
                    , T.hoverBgDurazno
                    , T.focusOutlineNone
                    , T.focusRing2
                    , T.focusRingDurazno
                    ]
                ]
                [ HH.text "Create Resource" ]
              ]
        ]
    feed = case st.lists of
      Success lists ->
        HH.div
          []
          [ HH.div
              [ HP.classes [ T.grid, T.gridCols1, T.smGridCols2, T.lgGridCols3, T.gap4 ] ]
              $ snoc
                (map (\list -> HH.slot List._list list._id."$oid" List.component { list } absurd) lists)
                listCreate
          , whenElem st.showCreateResource \_ ->
              HH.div
                [ HP.classes [ T.fixed, T.top4, T.right4, T.bgWhite ] ]
                [ HH.slot CreateResource._createResource unit CreateResource.component { lists } (Just <<< HandleCreateResource) ]
          ]

      Failure msg ->
        HH.div
          [ HP.classes [ T.p4, T.border4, T.borderRed600, T.bgRed200, T.textRed900 ] ]
          [ HH.p [ HP.classes [ T.fontBold, T.textLg ] ] [ HH.text "Error =(" ]
          , HH.p_ [ HH.text msg ]
          ]

      _ -> HH.div [ HP.classes [ T.textCenter ] ] [ HH.text "Loading ..." ]

    listCreate =
      HH.a
        [ safeHref CreateList
        , HE.onClick (Just <<< Navigate CreateList <<< Mouse.toEvent)
        , HP.classes [ T.border2, T.borderKiwi, T.roundedMd, T.flex, T.itemsCenter, T.justifyCenter, T.p8, T.bgWhite ]
        ]
        [ HH.div
            [ HP.classes
                [ T.cursorPointer
                , T.flex
                , T.flexCol
                , T.justifyCenter
                , T.itemsCenter
                ]
            ]
            [ HH.span [ HP.classes [ T.text7xl, T.textKiwi, T.leadingNone ] ] [ HH.text "+" ]
            , HH.span [ HP.classes [ T.textGray400 ] ] [ HH.text "Create new list" ]
            ]
        ]
