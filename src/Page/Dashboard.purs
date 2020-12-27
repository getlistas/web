module Listasio.Page.Dashboard where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Array (null, snoc, zipWith)
import Data.Either (Either, note)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Capability.Resource.List (class ManageList, getLists)
import Listasio.Capability.Resource.Resource (class ManageResource, createResource, getListResources)
import Listasio.Component.HTML.CreateResource as CreateResource
import Listasio.Component.HTML.Header (header)
import Listasio.Component.HTML.Utils (maybeElem, safeHref, whenElem)
import Listasio.Data.List (ListWithIdAndUser)
import Listasio.Data.Profile (Profile)
import Listasio.Data.Resource (ListResource)
import Listasio.Data.Route (Route(..))
import Listasio.Env (UserEnv)
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RemoteData
import Tailwind as T
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent as Mouse

data Action
  = Initialize
  | Receive { currentUser :: Maybe Profile }
  | Navigate Route Event
  | LoadLists
  | HandleCreateForm CreateResource.ResourceWithList

type Item
  = { list :: ListWithIdAndUser
    , resources :: Array ListResource
    }

type State
  = { currentUser :: Maybe Profile
    , lists :: RemoteData String (Array Item)
    }

type ChildSlots
  = ( formless :: CreateResource.Slot )

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
  initialState { currentUser } = { currentUser, lists: NotAsked }

  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> void $ H.fork $ handleAction LoadLists

    Receive { currentUser } -> H.modify_ _ { currentUser = currentUser }

    Navigate route e -> navigate_ e route

    LoadLists -> do
      H.modify_ _ { lists = Loading }
      lists <- RemoteData.fromEither <$> noteError <$> getLists
      H.modify_ _ { lists = map { list: _, resources: mempty } <$> lists }
      resourcesByList <- traverse (getListResources <<< _._id."$oid") $ fromMaybe mempty $ RemoteData.toMaybe lists
      let resources = map (fromMaybe mempty) resourcesByList
      H.modify_ _ { lists = (\ls -> zipWith { list: _, resources: _} ls resources) <$> lists }

    HandleCreateForm { description, title, url, list: listId } -> do
      { lists } <- H.get
      mbNewResource <- createResource { description, title, url } listId
      case mbNewResource of
        Just resource -> do
          H.modify_ \s -> s { lists = map (\i -> if i.list._id."$oid" == listId then i { resources = snoc i.resources resource  } else i) <$> s.lists }
          void $ H.query F._formless unit $ F.injQuery $ CreateResource.SetCreateError false unit

        Nothing -> void $ H.query F._formless unit $ F.injQuery $ CreateResource.SetCreateError true unit

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { currentUser, lists } =
    HH.div
      [ HP.classes [ T.minHScreen, T.wScreen, T.flex, T.flexCol, T.itemsCenter ] ]
      [ header currentUser Navigate Dashboard
      , HH.div
          [ HP.classes [ T.mt10 ] ]
          [ HH.a
              [ safeHref CreateList
              , HE.onClick (Just <<< Navigate CreateList <<< Mouse.toEvent)
              , HP.classes
                  [ T.cursorPointer
                  , T.py2
                  , T.px4
                  , T.bgPink700
                  , T.textWhite
                  , T.fontSemibold
                  , T.roundedLg
                  , T.shadowMd
                  , T.focusOutlineNone
                  ]
              ]
            [ HH.text "Create new list" ]
          ]
      , HH.div [ HP.classes [ T.mt4 ] ] [ feed ]
      ]
    where
    feed = case lists of
      Success items | null items -> HH.text "Create a list :)"
      Success items ->
        HH.div
          []
          [ HH.div
              [ HP.classes [ T.flex, T.flexWrap ] ]
              $ map listInfo items
          , HH.div
              [ HP.classes [ T.w1d2 ] ]
              [ HH.slot F._formless unit CreateResource.formComponent ({ lists: _ } $ map _.list $ items) (Just <<< HandleCreateForm) ]
          ]

      Failure msg ->
        HH.div
          [ HP.classes [ T.p4, T.border4, T.borderRed600, T.bgRed200, T.textRed900 ] ]
          [ HH.p [ HP.classes [ T.fontBold, T.textLg ] ] [ HH.text "Error =(" ]
          , HH.p_ [ HH.text msg ]
          ]

      _ -> HH.div [ HP.classes [ T.textCenter ] ] [ HH.text "Loading ..." ]

    listInfo :: forall slots. Item -> H.ComponentHTML Action slots m
    listInfo { list: { title, description, tags }, resources } =
      HH.div
        [ HP.classes [ T.m4, T.p2, T.border4, T.borderIndigo400 ] ]
        [ HH.div [ HP.classes [ T.textLg, T.borderB2, T.borderGray200, T.mb4 ] ] [ HH.text title ]
        , maybeElem description \des -> HH.div [ HP.classes [ T.textSm, T.mb4 ] ] [ HH.text des ]
        , whenElem (not $ null tags) \_ ->
            HH.div
              [ HP.classes [ T.flex, T.textSm ] ]
              $ map ((\t -> HH.div [ HP.classes [ T.mr2 ] ] [ HH.text t ]) <<< ("#" <> _)) tags
        , whenElem (not $ null resources) \_ ->
            HH.div
              [ HP.classes [ T.mt4, T.flex, T.flexCol ] ]
              $ map (\i -> HH.a [ HP.href i.url ] [ HH.text i.title ]) resources

        ]
