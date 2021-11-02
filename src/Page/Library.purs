module Listasio.Page.Library where

import Prelude

import Data.Array (groupBy, sortBy, sortWith)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.String.NonEmpty as NES
import Data.Either (Either, note)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.Ord.Down (Down(..))
import Data.Tuple (Tuple(..), snd)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Store.Select (selectEq)
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Capability.Resource.List (class ManageList, getLists)
import Listasio.Capability.Resource.Resource (class ManageResource, searchResources)
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Message as Message
import Listasio.Component.HTML.Resource (resource)
import Listasio.Component.HTML.ToggleGroup as ToggleGroup
import Listasio.Data.List (ListWithIdUserAndMeta)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Resource (FilterByDone(..), ListResource)
import Listasio.Data.Route (Route)
import Listasio.Data.YearMonth (YearMonth)
import Listasio.Data.YearMonth as YearMonth
import Listasio.Store as Store
import Network.RemoteData (RemoteData(..), fromEither, isLoading, isNotAsked, withDefault)
import Tailwind as T
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

_slot :: Proxy "library"
_slot = Proxy

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
  | ToggleFilterByDone FilterByDone
  | SearchChange String
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

-- TODO: group by completed_at on Done
groupResources :: FilterByDone -> Array ListResource -> GroupedResources
groupResources ShowDone items =
  items
    # sortBy byCompletedAt
    # groupBy createdOnSameMonth
    # map monthItemsPair
    # Map.fromFoldable
    # map (NEA.sortBy byCompletedAt)
groupResources _ items =
  items
    # sortWith _.created_at
    # groupBy createdOnSameMonth
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
    }

noteError :: forall a. Maybe a -> Either String a
noteError = note "Failed to load your library contents"

select :: Store.Store -> StoreState
select {lists, currentUser} = {lists, currentUser}

queryParamFilterBy :: FilterByDone -> Maybe Boolean
queryParamFilterBy ShowAll = Nothing
queryParamFilterBy ShowDone = Just true
queryParamFilterBy ShowPending = Just false

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
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> do
      void $ H.fork $ handleAction LoadResources
      void $ H.fork $ handleAction LoadLists

    LoadResources -> do
      H.modify_ _ {resources = Loading}
      {filterBy, searchQuery} <- H.get
      let completed = queryParamFilterBy filterBy
          search_text = NES.fromString $ S.trim searchQuery
          search = {list: Nothing, completed, sort: Nothing, search_text, limit: Nothing, skip: Nothing}
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

    NoOp -> pure unit

  render :: forall slots. State -> H.ComponentHTML Action slots m
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
            [ HP.classes [ T.flex, T.flexCol, T.smFlexRow, T.justifyBetween, T.gap4, T.mb4 ] ]
            [ ToggleGroup.toggleGroup
                (isLoading resources)
                [ {label: "All", action: ToggleFilterByDone ShowAll, active: filterBy == ShowAll}
                , {label: "Done", action: ToggleFilterByDone ShowDone, active: filterBy == ShowDone}
                , {label: "Pending", action: ToggleFilterByDone ShowPending, active: filterBy == ShowPending}
                ]
            , HH.div
                [ HP.classes [ T.wFull, T.flex, T.roundedLg ] ]
                [ HH.input
                    [ HP.placeholder "Search resources"
                    , HP.classes
                        [ T.wFull
                        , T.roundedNone
                        , T.roundedLLg
                        , T.smTextSm
                        , T.border
                        , T.borderGray300
                        , T.px4
                        , T.focusRing2
                        , T.focusRingKiwi
                        , T.focusOutlineNone
                        , T.focusBorderKiwi
                        , T.focusZ10
                        ]
                    , HP.value searchQuery
                    , HE.onValueInput $ SearchChange
                    , HE.onKeyDown \e ->
                        case KeyboardEvent.code e of
                          "Escape" -> SearchChange ""
                          _ -> NoOp
                    ]
                , HH.button
                    [ HP.classes
                        [ T.negMlPx
                        , T.flex
                        , T.itemsCenter
                        , T.p2
                        , T.border
                        , T.borderGray300
                        , T.roundedRLg
                        , T.textGray300
                        , T.hoverTextKiwi
                        , T.bgGray100
                        , T.focusOutlineNone
                        , T.focusRing2
                        , T.focusRingKiwi
                        , T.focusBorderKiwi
                        ]
                    , HE.onClick $ const $ SearchChange ""
                    , HP.disabled $ S.null searchQuery
                    ]
                    [ Icons.x
                        [ Icons.classes [ T.h5, T.w5 ] ]
                    ]
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
