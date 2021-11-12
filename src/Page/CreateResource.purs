module Listasio.Page.CreateResource where

import Prelude

import Data.Either (Either, note)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Listasio.Capability.Navigate (class Navigate, navigate, navigate_)
import Listasio.Capability.Resource.List (class ManageList, getLists)
import Listasio.Capability.Resource.Resource (class ManageResource)
import Listasio.Component.HTML.CreateResource as CreateResource
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Utils (safeHref)
import Listasio.Data.List (ListWithIdUserAndMeta)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Route (Route(..))
import Listasio.Store as Store
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RemoteData
import Tailwind as T
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent as Mouse

_slot :: Proxy "createResource"
_slot = Proxy

type Input
  =
  { url :: Maybe String
  , title :: Maybe String
  , text :: Maybe String
  }

data Action
  = Initialize
  | Receive (Connected (Maybe ProfileWithIdAndEmail) Input)
  | Navigate Route Event
  | LoadLists
  | HandleCreateResource CreateResource.Output

type State
  =
  { currentUser :: Maybe ProfileWithIdAndEmail
  , lists :: RemoteData String (Array ListWithIdUserAndMeta)
  , url :: Maybe String
  , title :: Maybe String
  , text :: Maybe String
  }

type ChildSlots
  =
  ( createResource :: CreateResource.Slot
  )

noteError :: forall a. Maybe a -> Either String a
noteError = note "Could not fetch your lists"

component
  :: forall q o m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => ManageList m
  => ManageResource m
  => Navigate m
  => H.Component q Input o m
component = connect (selectEq _.currentUser) $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      , initialize = Just Initialize
      }
  }
  where
  initialState { context: currentUser, input: { url, title, text } } =
    { currentUser, lists: NotAsked, url, title, text }

  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> void $ H.fork $ handleAction LoadLists

    Receive { context: currentUser } -> H.modify_ _ { currentUser = currentUser }

    Navigate route e -> navigate_ e route

    LoadLists -> do
      H.modify_ _ { lists = Loading }
      lists <- RemoteData.fromEither <$> noteError <$> getLists
      H.modify_ _ { lists = lists }

    HandleCreateResource (CreateResource.Created _) ->
      navigate Dashboard

  render :: State -> H.ComponentHTML Action ChildSlots m
  render st =
    HH.div
      []
      [ header
      , HH.div [ HP.classes [ T.container ] ] [ form ]
      ]

    where
    header =
      HH.div
        [ HP.classes [ T.flex, T.itemsCenter, T.textGray400, T.mb6, T.text4xl, T.fontBold, T.pt2 ] ]
        [ HH.a
            [ safeHref Dashboard
            , HE.onClick $ Navigate Dashboard <<< Mouse.toEvent
            , HP.classes [ T.textGray200, T.mr8, T.flexShrink0 ]
            ]
            [ Icons.chevronLeft [ Icons.classes [ T.h10, T.w10 ] ] ]
        , HH.text "Add resource"
        ]

    form = case st.lists of
      Success lists ->
        HH.div
          [ HP.classes [ T.wFull, T.maxWLg ] ]
          [ let
              input = { lists, url: st.url, title: st.title, text: st.text, selectedList: Nothing }
            in
              HH.slot CreateResource._slot unit CreateResource.component input HandleCreateResource
          ]

      Failure msg ->
        HH.div
          [ HP.classes [ T.p4, T.border4, T.borderRed600, T.bgRed200, T.textRed900 ] ]
          [ HH.p [ HP.classes [ T.fontBold, T.textLg ] ] [ HH.text "Error =(" ]
          , HH.p_ [ HH.text msg ]
          ]

      _ -> HH.div [ HP.classes [ T.textCenter ] ] [ HH.text "Loading ..." ]
