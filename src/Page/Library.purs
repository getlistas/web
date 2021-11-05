module Listasio.Page.Library where

import Prelude

import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either, note)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Ord.Down (Down(..))
import Data.String as S
import Data.String.NonEmpty as NES
import Data.Tuple (Tuple(..), snd)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Store.Select (selectEq)
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Capability.Resource.List (class ManageList, getLists)
import Listasio.Capability.Resource.Resource (class ManageResource, searchResources)
import Listasio.Component.HTML.Dropdown as DD
import Listasio.Component.HTML.Input as Input
import Listasio.Component.HTML.Message as Message
import Listasio.Component.HTML.Resource (resource)
import Listasio.Component.HTML.ToggleGroup as ToggleGroup
import Listasio.Data.ID (ID)
import Listasio.Data.List (ListWithIdUserAndMeta)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Resource (FilterByDone(..), ListResource)
import Listasio.Data.Route (Route)
import Listasio.Data.YearMonth (YearMonth)
import Listasio.Data.YearMonth as YearMonth
import Listasio.Form.Validation (class ToText)
import Listasio.Store as Store
import Network.RemoteData (RemoteData(..), fromEither, isLoading, isNotAsked, withDefault)
import Select as Select
import Tailwind as T
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)

_slot :: Proxy "library"
_slot = Proxy

type ChildSlots
  = ( dropdown :: DD.Slot DDItem Unit )

type Lists = RemoteData String (Array ListWithIdUserAndMeta)

type StoreState
  = { currentUser :: Maybe ProfileWithIdAndEmail
    , lists :: Lists
    }

data Action
  = Initialize
  | Receive (Connected StoreState Unit)
  | LoadResources
  | LoadLists
  | Navigate Route Event
    -- query
  | ToggleFilterByDone FilterByDone
  | SearchChange String
  | HandleListSelection (DD.Message DDItem)
    -- Meta
  | NoOp

filterByMsg :: FilterByDone -> String
filterByMsg = case _ of
  ShowAll -> "You haven't added any resource yet 😅"
  ShowDone -> "No completed resources yet 😅"
  ShowPending -> "No pending resources 👏"

type GroupedResources
  = Map (Down YearMonth) (NonEmptyArray ListResource)

byCompletedAt :: ListResource -> ListResource -> Ordering
byCompletedAt {completed_at: Nothing} {completed_at: Just _} = LT
byCompletedAt {completed_at: Just _} {completed_at: Nothing} = GT
byCompletedAt {created_at: a} {created_at: b} = compare a b

byCreatedAt :: ListResource -> ListResource -> Ordering
byCreatedAt {created_at: a} {created_at: b} = compare a b

groupResources :: FilterByDone -> Array ListResource -> GroupedResources
groupResources ShowDone items =
  items
    # A.sortBy byCompletedAt
    # A.groupBy createdOnSameMonth
    # map monthItemsPair
    # Map.fromFoldable
    # map (NEA.sortBy byCompletedAt)
groupResources _ items =
  items
    # A.sortWith _.created_at
    # A.groupBy createdOnSameMonth
    # map monthItemsPair
    # Map.fromFoldable
    # map (NEA.sortWith _.created_at)

createdOnSameMonth :: ListResource -> ListResource -> Boolean
createdOnSameMonth {created_at: a} {created_at: b} =
  YearMonth.fromDateTime a == YearMonth.fromDateTime b

monthItemsPair :: NonEmptyArray ListResource -> Tuple (Down YearMonth) (NonEmptyArray ListResource)
monthItemsPair is = Tuple (Down $ YearMonth.fromDateTime $ _.created_at $ NEA.head is) is

type State
  = { currentUser :: Maybe ProfileWithIdAndEmail
    , resources :: RemoteData String GroupedResources
    , lists :: Lists
    , filterBy :: FilterByDone
    , searchQuery :: String
    , list :: Maybe ID
    }

noteError :: forall a. Maybe a -> Either String a
noteError = note "Failed to load your library contents"

select :: Store.Store -> StoreState
select {lists, currentUser} = {lists, currentUser}

queryParamFilterBy :: FilterByDone -> Maybe Boolean
queryParamFilterBy ShowAll = Nothing
queryParamFilterBy ShowDone = Just true
queryParamFilterBy ShowPending = Just false

newtype DDItem = DDItem {label :: String, value :: ID}

derive instance eqDDItem :: Eq DDItem
derive instance newtypeDDItem :: Newtype DDItem _

instance toTextDDItem :: ToText DDItem where
  toText = _.label <<< unwrap

component
  :: forall q o m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => ManageResource m
  => ManageList m
  => Navigate m
  => H.Component q Unit o m
component = connect (selectEq select) $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      , initialize = Just Initialize
      }
  }
  where
  initialState {context: {lists, currentUser}} =
    { currentUser
    , resources: NotAsked
    , lists
    , filterBy: ShowDone
    , searchQuery: ""
    , list: Nothing
    }

  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      void $ H.fork $ handleAction LoadResources
      void $ H.fork $ handleAction LoadLists

    LoadResources -> do
      H.modify_ _ {resources = Loading}
      {filterBy, searchQuery, list} <- H.get
      let completed = queryParamFilterBy filterBy
          search_text = NES.fromString $ S.trim searchQuery
          search = {list, completed, sort: Nothing, search_text, limit: Nothing, skip: Nothing}
      resources <- map (groupResources filterBy) <$> fromEither <$> noteError <$> searchResources search
      H.modify_ _ {resources = resources}

    LoadLists -> do
      {lists} <- H.get
      when (isNotAsked lists) do updateStore $ Store.SetLists Loading
      result <- fromEither <$> noteError <$> getLists
      updateStore $ Store.SetLists result

    Receive {context: {currentUser, lists}} ->
      H.modify_ _ {currentUser = currentUser, lists = lists}

    Navigate route e -> navigate_ e route

    ToggleFilterByDone filterBy -> do
      H.modify_ _ {filterBy = filterBy}
      handleAction LoadResources

    SearchChange newQuery -> do
      {searchQuery: oldQuery} <- H.get

      when (newQuery /= oldQuery) do
        H.modify_ _ {searchQuery = newQuery}
        handleAction LoadResources

    HandleListSelection (DD.Selected (DDItem {value: list})) -> do
      {list: prevList} <- H.get

      when (prevList /= Just list) do
        H.modify_ _ {list = Just list}
        handleAction LoadResources

    HandleListSelection DD.Cleared -> do
      {list: prevList} <- H.get

      when (isJust prevList) do
        H.modify_ _ {list = Nothing}
        handleAction LoadResources

    NoOp -> pure unit

  render :: State -> H.ComponentHTML Action ChildSlots m
  render {currentUser, resources, lists: mbLists, filterBy, searchQuery} =
    HH.div
      []
      [ HH.div
          [ HP.classes [ T.pt2 ] ]
          [ HH.h1
              [ HP.classes [ T.textGray400, T.mb6, T.text4xl, T.fontBold ] ]
              [ HH.text "Library" ]
          ]
      , settings
      , feed
      ]

    where
    lists = withDefault [] mbLists

    settings =
      HH.div
        [ HP.classes [ T.flex, T.itemsStart, T.flexWrap, T.my6 ] ]
        [ HH.div
            [ HP.classes [ T.flex, T.flexCol, T.smFlexRow, T.justifyBetween, T.itemsCenter, T.gap4, T.mb4 ] ]
            [ ToggleGroup.toggleGroup
                (isLoading resources)
                [ {label: "All", action: ToggleFilterByDone ShowAll, active: filterBy == ShowAll}
                , {label: "Done", action: ToggleFilterByDone ShowDone, active: filterBy == ShowDone}
                , {label: "Pending", action: ToggleFilterByDone ShowPending, active: filterBy == ShowPending}
                ]
            , let ddInput = {placeholder: "Choose a list", items: map listToItem lists}
                  listToItem {id, title} = DDItem {value: id, label: title}
               in HH.div
                    [ HP.classes [ T.wFull ] ]
                    [ HH.slot DD._dropdown unit (Select.component DD.input DD.spec) ddInput HandleListSelection ]
            , HH.div
                [ HP.classes [ T.wFull ] ]
                [ Input.search
                    { value: searchQuery
                    , placeholder: Just "Search resources"
                    , onValueInput: SearchChange
                    , onEscape: SearchChange ""
                    , noOp: NoOp
                    }
                ]
            ]
        ]

    feed =
      case resources of
        Success grouped | Map.isEmpty grouped ->
          HH.div
            [ HP.classes [ T.textGray300, T.text2xl, T.mt10 ] ]
            [ HH.text $ case filterBy, S.null $ S.trim searchQuery of
                _, false -> "No results found 😅"
                ShowDone, _ -> "No completed resources yet 😅"
                _, _ -> "You haven't added any resource yet 😅"
            ]

        Success grouped ->
          HH.div []
            $ map snd
            $ Map.toUnfoldable
            $ mapWithIndex dateFeed grouped

        Failure msg ->
          Message.message $ Message.props
            { classes = [ T.mt10, T.py2, T.w96 ]
            , style = Message.Alert
            , title = Just msg
            , icon = Just "😓"
            }

        _ ->
          HH.div
            [ HP.classes [ T.textGray300, T.textLg, T.mt10 ] ]
            [ HH.text "Loading ..." ]

    dateFeed (Down yearMonth) items =
      HH.div
        []
        [ HH.div
            [ HP.classes [ T.textXl, T.textGray400, T.mt6, T.mb4 ] ]
            [ HH.text $ show yearMonth ]
        , HH.div
            [ HP.classes [ T.grid, T.gridCols1, T.mdGridCols2, T.lgGridCols3, T.xlGridCols4, T.gap4 ] ]
            $ map (resource currentUser (Just Navigate) lists)
            $ NEA.toArray items
        ]
