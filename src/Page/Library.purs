module Listasio.Page.Library where

import Prelude

import Data.Array (find, groupBy, mapMaybe, null, sortBy, sortWith)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either, note)
import Data.Filterable (filter)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Ord.Down (Down(..))
import Data.Tuple (Tuple(..), snd)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Store.Select (selectEq)
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Capability.Resource.List (class ManageList, getLists)
import Listasio.Capability.Resource.Resource (class ManageResource, getResources)
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Message as Message
import Listasio.Component.HTML.Resource (resource)
import Listasio.Component.HTML.ToggleGroup as ToggleGroup
import Listasio.Component.HTML.Utils (safeHref)
import Listasio.Data.ID (ID)
import Listasio.Data.List (ListWithIdUserAndMeta)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Resource (FilterByDone(..), ListResource)
import Listasio.Data.Route (Route(..))
import Listasio.Data.YearMonth (YearMonth)
import Listasio.Data.YearMonth as YearMonth
import Listasio.Store as Store
import Network.RemoteData (RemoteData(..), fromEither, isNotAsked, withDefault)
import Tailwind as T
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent as Mouse

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
  | ToggleGroupBy GroupBy
  | ToggleFilterByDone FilterByDone

filterByDoneMsg :: FilterByDone -> String
filterByDoneMsg = case _ of
  ShowAll -> "You haven't added any resource yet 😅"
  ShowDone -> "No completed resources yet 😅"
  ShowPending -> "No pending resources 👏"

data GroupBy
  = GroupNone
  | GroupList
  | GroupDate

derive instance eqGroupBy :: Eq GroupBy

type GroupedResources
  = { byList :: Map ID (NonEmptyArray ListResource)
    , byDate :: Map (Down YearMonth) (NonEmptyArray ListResource)
    , all :: Array ListResource
    }

byCompletedAt :: ListResource -> ListResource -> Ordering
byCompletedAt {completed_at: Nothing} {completed_at: Just _} = LT
byCompletedAt {completed_at: Just _} {completed_at: Nothing} = GT
byCompletedAt {completed_at: Just a} {completed_at: Just b} = compare a b
byCompletedAt {created_at: a} {created_at: b} = compare a b

groupResources :: Array ListResource -> GroupedResources
groupResources items = {byList, byDate, all}
  where
  all = sortBy byCompletedAt items
  byList =
    items
      # sortWith _.list
      # groupBy (\a b -> a.list == b.list)
      # map (Tuple <$> (_.list <<< NEA.head) <*> identity)
      # Map.fromFoldable
      # map (NEA.sortBy byCompletedAt)
  byDate =
    items
      # filter (isJust <<< _.completed_at)
      # sortWith _.completed_at
      # groupBy completedOnSameMonth
      # mapMaybe monthItemsPair
      # Map.fromFoldable
      # map (NEA.sortBy byCompletedAt)

completedOnSameMonth :: ListResource -> ListResource -> Boolean
completedOnSameMonth {completed_at: a} {completed_at: b} =
  map YearMonth.fromDateTime a == map YearMonth.fromDateTime b

monthItemsPair :: NonEmptyArray ListResource -> Maybe (Tuple (Down YearMonth) (NonEmptyArray ListResource))
monthItemsPair is =
  flip Tuple is <$> (map Down $ map YearMonth.fromDateTime $ _.completed_at $ NEA.head is)

type State
  = { currentUser :: Maybe ProfileWithIdAndEmail
    , resources :: RemoteData String GroupedResources
    , lists :: Lists
    , groupBy :: GroupBy
    , filterByDone :: FilterByDone
    }

noteError :: forall a. Maybe a -> Either String a
noteError = note "Failed to load your library contents"

select :: Store.Store -> StoreState
select {lists, currentUser} = {lists, currentUser}

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
    , groupBy: GroupDate
    , filterByDone: ShowDone
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> do
      void $ H.fork $ handleAction LoadResources
      void $ H.fork $ handleAction LoadLists

    LoadResources -> do
      H.modify_ _ {resources = Loading}
      resources <- map groupResources <$> fromEither <$> noteError <$> getResources
      H.modify_ _ {resources = resources}

    LoadLists -> do
      {lists} <- H.get
      when (isNotAsked lists) do updateStore $ Store.SetLists Loading
      result <- fromEither <$> noteError <$> getLists
      updateStore $ Store.SetLists result

    Receive {context: {currentUser, lists}} ->
      H.modify_ _ {currentUser = currentUser, lists = lists}

    Navigate route e -> navigate_ e route

    ToggleGroupBy groupBy ->
      H.modify_ _ {groupBy = groupBy}

    ToggleFilterByDone filterByDone ->
      H.modify_ _ {filterByDone = filterByDone}

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render {currentUser, resources, lists: mbLists, groupBy, filterByDone} =
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
            [ HP.classes [ T.mb4, T.mr0, T.smMr4, T.wFull, T.smWAuto ] ]
            [ ToggleGroup.toggleGroup
                false
                [ {label: "None", action: ToggleGroupBy GroupNone, active: groupBy == GroupNone}
                , {label: "By Date", action: ToggleGroupBy GroupDate, active: groupBy == GroupDate}
                , {label: "By List", action: ToggleGroupBy GroupList, active: groupBy == GroupList}
                ]
            ]
        , HH.div
            [ HP.classes [ T.wFull , T.smWAuto ] ]
            [ ToggleGroup.toggleGroup
                (groupBy == GroupDate)
                [ {label: "All", action: ToggleFilterByDone ShowAll, active: filterByDone == ShowAll && groupBy /= GroupDate}
                , {label: "Done", action: ToggleFilterByDone ShowDone, active: filterByDone == ShowDone || groupBy == GroupDate}
                , {label: "Pending", action: ToggleFilterByDone ShowPending, active: filterByDone == ShowPending && groupBy /= GroupDate}
                ]
            ]
        ]

    filterByDoneFn =
      case filterByDone, groupBy of
        ShowAll, _ -> identity
        ShowDone, _ -> filter (isJust <<< _.completed_at)
        ShowPending, GroupDate -> identity
        ShowPending, _ -> filter (isNothing <<< _.completed_at)

    feed =
      case resources of
        Success grouped | null grouped.all ->
          HH.div
            [ HP.classes [ T.textGray300, T.text2xl, T.mt10 ] ]
            [ HH.text "You haven't added any resource yet 😅" ]

        Success grouped ->
          case groupBy of
            GroupNone ->
              case filterByDoneFn grouped.all, filterByDone of
                [], fbd ->
                  HH.div
                    [ HP.classes [ T.textGray300, T.text2xl, T.mt10 ] ]
                    [ HH.text $ filterByDoneMsg fbd ]
                rs, _ ->
                  HH.div
                    [ HP.classes [ T.grid, T.gridCols1, T.mdGridCols2, T.lgGridCols3, T.xlGridCols4, T.gap4 ] ]
                    $ map (resource lists) rs

            GroupList ->
              case map snd $ Map.toUnfoldable $ Map.mapMaybeWithKey listFeed grouped.byList of
                [] ->
                  HH.div
                    [ HP.classes [ T.textGray300, T.text2xl, T.mt10 ] ]
                    [ HH.text $ filterByDoneMsg filterByDone ]

                items -> HH.div [] items

            GroupDate ->
              case Map.isEmpty grouped.byDate of
                true ->
                  HH.div
                    [ HP.classes [ T.textGray300, T.text2xl, T.mt10 ] ]
                    [ HH.text "No completed resources yet 😅" ]
                false ->
                  HH.div []
                    $ map snd
                    $ Map.toUnfoldable
                    $ mapWithIndex dateFeed grouped.byDate

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

    listFeed listId items =
      lists
        # find ((listId == _) <<< _.id)
        # filter (const $ not $ null filteredItems)
        # map (\l ->
                HH.div
                  []
                  [ HH.a
                      ( editLinkProps
                          l
                          [ HP.classes
                              [ T.textXl
                              , T.textGray400
                              , T.mt6
                              , T.mb4
                              , T.block
                              , T.flex
                              , T.itemsCenter
                              , T.hoverTextKiwi
                              , T.hoverUnderline
                              ]
                          ]
                      )
                      [ HH.span [] [ HH.text l.title ]
                      , Icons.cog [ Icons.classes [ T.ml2, T.h5, T.w5 ] ]
                      ]
                  , HH.div
                      [ HP.classes [ T.grid, T.gridCols1, T.mdGridCols2, T.lgGridCols3, T.xlGridCols4, T.gap4 ] ]
                      $ map (resource lists)
                      $ filteredItems
                  ]
              )
      where
        filteredItems = filterByDoneFn $ NEA.toArray items

        editLinkProps l rest =
          case currentUser of
            Just {slug} ->
              [ safeHref $ EditList slug l.slug
              , HE.onClick (Navigate (EditList slug l.slug) <<< Mouse.toEvent)
              ]
                <> rest
            Nothing -> rest

    dateFeed (Down yearMonth) items =
      HH.div
        []
        [ HH.div
            [ HP.classes [ T.textXl, T.textGray400, T.mt6, T.mb4 ] ]
            [ HH.text $ show yearMonth ]
        , HH.div
            [ HP.classes [ T.grid, T.gridCols1, T.mdGridCols2, T.lgGridCols3, T.xlGridCols4, T.gap4 ] ]
            $ map (resource lists)
            $ filterByDoneFn
            $ NEA.toArray items
        ]
