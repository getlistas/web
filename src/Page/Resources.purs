module Listasio.Page.Resources where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Array (find, groupBy, mapMaybe, null, sortBy, sortWith)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.DateTime (DateTime)
import Data.Either (Either, note)
import Data.Filterable (filter)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map, fromFoldable, toUnfoldable)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Tuple (Tuple(..), snd)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Capability.Resource.List (class ManageList, getLists)
import Listasio.Capability.Resource.Resource (class ManageResource, getResources)
import Listasio.Component.HTML.Layout as Layout
import Listasio.Component.HTML.Utils (cx, maybeElem)
import Listasio.Data.ID (ID)
import Listasio.Data.List (ListWithIdAndUser)
import Listasio.Data.Ordering (comparingOn)
import Listasio.Data.Profile (Profile)
import Listasio.Data.Resource (ListResource)
import Listasio.Data.Route (Route(..))
import Listasio.Data.YearMonth (YearMonth)
import Listasio.Data.YearMonth as YearMonth
import Listasio.Env (UserEnv)
import Network.RemoteData (RemoteData(..), fromEither)
import Tailwind as T
import Util (takeDomain)
import Web.Event.Event (Event)

data Action
  = Initialize
  | Receive { currentUser :: Maybe Profile }
  | LoadResources
  | LoadLists
  | Navigate Route Event
  | ToggleGroupByList
  | ToggleGroupByDate
  | ToggleShowDone
  | ToggleShowPending

data FilterByDone
  = ShowAll
  | ShowDone
  | ShowPending

derive instance eqFilterByDone :: Eq FilterByDone

toggleFilterByDone :: FilterByDone -> FilterByDone
toggleFilterByDone ShowDone = ShowAll
toggleFilterByDone _ = ShowDone

toggleFilterByPending :: FilterByDone -> FilterByDone
toggleFilterByPending ShowPending = ShowAll
toggleFilterByPending _ = ShowPending

data GroupBy
  = GroupNone
  | GroupList
  | GroupDate

derive instance eqGroupBy :: Eq GroupBy

toggleGroupByList :: GroupBy -> GroupBy
toggleGroupByList GroupList = GroupNone
toggleGroupByList _ = GroupList

toggleGroupByDate :: GroupBy -> GroupBy
toggleGroupByDate GroupDate = GroupNone
toggleGroupByDate _ = GroupDate

type GroupedResources
  = { byList :: Map ID (NonEmptyArray ListResource)
    , byDate :: Map YearMonth (NonEmptyArray ListResource)
    , all :: Array ListResource
    }

-- TODO: on both Nothing default to _.created_at
byCompletedAt :: Maybe DateTime -> Maybe DateTime -> Ordering
byCompletedAt Nothing (Just _) = LT
byCompletedAt (Just _) Nothing = GT
byCompletedAt Nothing Nothing = EQ
byCompletedAt (Just a) (Just b) = compare a b

groupResources :: Array ListResource -> GroupedResources
groupResources items = { byList, byDate, all }
  where
  all = sortBy (comparingOn _.completed_at byCompletedAt) items
  byList =
    items
      # sortWith _.list
      # groupBy (\a b -> a.list == b.list)
      # map (Tuple <$> (_.list <<< NEA.head) <*> identity)
      # fromFoldable
      # map (NEA.sortBy $ comparingOn _.completed_at byCompletedAt)
  byDate =
    items
      # filter (isJust <<< _.completed_at)
      # sortWith _.completed_at
      # groupBy completedOnSameMonth
      # mapMaybe monthItemsPair
      # fromFoldable
      # map (NEA.sortWith _.completed_at)

completedOnSameMonth :: ListResource -> ListResource -> Boolean
completedOnSameMonth { completed_at: a } { completed_at: b } =
  map YearMonth.fromDateTime a == map YearMonth.fromDateTime b

monthItemsPair :: NonEmptyArray ListResource -> Maybe (Tuple YearMonth (NonEmptyArray ListResource))
monthItemsPair is =
  flip Tuple is <$> (map YearMonth.fromDateTime $ _.completed_at $ NEA.head is)

type State
  = { currentUser :: Maybe Profile
    , resources :: RemoteData String GroupedResources
    , lists :: Maybe (Array ListWithIdAndUser)
    , groupBy :: GroupBy
    , filterByDone :: FilterByDone
    }

noteError :: forall a. Maybe a -> Either String a
noteError = note "Could not fetch your lists"

component
  :: forall q o m r
   . MonadAff m
  => MonadAsk { userEnv :: UserEnv | r } m
  => ManageResource m
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
  initialState { currentUser } =
    { currentUser
    , resources: NotAsked
    , lists: Nothing
    , groupBy: GroupNone
    , filterByDone: ShowDone
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> do
      void $ H.fork $ handleAction LoadLists
      void $ H.fork $ handleAction LoadResources

    LoadResources -> do
      H.modify_ _ { resources = Loading }
      resources <- map groupResources <$> fromEither <$> noteError <$> getResources
      H.modify_ _ { resources = resources }

    LoadLists -> do
      H.modify_ _ { lists = Nothing }
      lists <- getLists
      H.modify_ _ { lists = lists }

    Receive { currentUser } ->
      H.modify_ _ { currentUser = currentUser }

    Navigate route e -> navigate_ e route

    ToggleGroupByList ->
      H.modify_ \s -> s { groupBy = toggleGroupByList s.groupBy }

    ToggleGroupByDate ->
      H.modify_ \s -> s { groupBy = toggleGroupByDate s.groupBy }

    ToggleShowDone ->
      H.modify_ \s -> s { filterByDone = toggleFilterByDone s.filterByDone }

    ToggleShowPending ->
      H.modify_ \s -> s { filterByDone = toggleFilterByPending s.filterByDone }

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { currentUser, resources, lists: mbLists, groupBy, filterByDone } =
    Layout.dashboard
      currentUser
      Navigate
      (Just Resources)
      $ HH.div
          []
          [ HH.h1
              [ HP.classes [ T.textGray400, T.mb6, T.text4xl, T.fontBold ] ]
              [ HH.text "Resources" ]
          , settings
          , feed
          ]
    where
    settings =
      HH.div
        [ HP.classes [ T.flex, T.itemsStart, T.my6, T.spaceX2 ] ]
        [ filterBtnGroup
            false
            (filterBtn "By Month" (Just ToggleGroupByDate) $ groupBy == GroupDate)
            (filterBtn "By List" (Just ToggleGroupByList) $ groupBy == GroupList)
        , filterBtnGroup
            (groupBy == GroupDate)
            (filterBtn "Done" (Just ToggleShowDone) $ filterByDone == ShowDone || groupBy == GroupDate)
            (filterBtn "Pending" (Just ToggleShowPending) $ filterByDone == ShowPending && groupBy /= GroupDate)
        ]

    filterBtnGroup disabled l r =
      HH.div
        [ HP.classes
            [ T.roundedMd
            , T.bgGray100
            , T.py1
            , T.grid
            , T.gridCols2
            , T.divideX2
            , T.divideWhite
            , T.textSm
            , cx T.cursorNotAllowed disabled
            ]
        ]
        [ l disabled, r disabled ]

    filterBtn label action isSelected disabled =
      HH.div
        [ HP.classes [ T.px2 ] ]
        [ HH.button
            [ HP.type_ HP.ButtonButton
            , HE.onClick \_ -> action
            , HP.disabled disabled
            , HP.classes
                [ T.roundedMd
                , T.px4
                , T.py1
                , T.wFull
                , cx T.bgKiwi isSelected
                , cx T.textWhite isSelected
                , cx T.bgTransparent $ not isSelected
                , cx T.textGray300 $ not isSelected
                , T.focusOutlineNone
                , T.focusRing2
                , T.focusRingGray300
                , T.focusRingInset
                , T.disabledCursorNotAllowed
                , cx T.disabledOpacity50 $ isSelected
                ]
            ]
            [ HH.text label ]
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
          HH.div [] [ HH.text "No resources added yet :)" ]

        Success grouped ->
          case groupBy of
            GroupNone ->
              HH.div
                [ HP.classes [ T.grid, T.gridCols1, T.mdGridCols2, T.lgGridCols3, T.xlGridCols4, T.gap4 ] ]
                $ map item
                $ filterByDoneFn grouped.all

            GroupList ->
              HH.div []
                $ map snd
                $ toUnfoldable
                $ mapWithIndex listFeed grouped.byList

            GroupDate ->
              HH.div []
                $ map snd
                $ toUnfoldable
                $ mapWithIndex dateFeed grouped.byDate

        Failure msg ->
          HH.div
            []
            [ HH.text msg ]
        _ ->
          HH.div [] [ HH.text "Loading ..." ]

    listFeed listId items =
      maybeElem (find ((listId == _) <<< _.id) lists) \l ->
        HH.div
          []
          [ HH.div [ HP.classes [ T.textXl, T.textGray400, T.mt6, T.mb4 ] ] [ HH.text l.title ]
          , HH.div
              [ HP.classes [ T.grid, T.gridCols1, T.mdGridCols2, T.lgGridCols3, T.xlGridCols4, T.gap4 ] ]
              $ map item
              $ filterByDoneFn
              $ NEA.toArray items
          ]

    dateFeed yearMonth items =
      HH.div
        []
        [ HH.div
            [ HP.classes [ T.textXl, T.textGray400, T.mt6, T.mb4 ] ]
            [ HH.text $ show yearMonth ]
        , HH.div
            [ HP.classes [ T.grid, T.gridCols1, T.mdGridCols2, T.lgGridCols3, T.xlGridCols4, T.gap4 ] ]
            $ map item
            $ filterByDoneFn
            $ NEA.toArray items
        ]

    lists = fromMaybe [] mbLists

    shortUrl url =
      maybeElem (takeDomain url) \short ->
        HH.div
          [ HP.classes [ T.textGray300, T.textXs, T.mt2, T.ml6 ] ]
          [ HH.text short ]

    tag t =
      HH.span
        [ HP.classes
            [ T.textXs
            , T.px1
            , T.roundedSm
            , T.bgDurazno
            , T.textWhite
            , T.mr1
            , T.leadingNormal
            ]
        ]
        [ HH.text t ]

    item { url, title, list, completed_at } =
      HH.div
        [ HP.classes
            [ T.roundedMd
            , T.bgWhite
            , T.border2
            , T.borderKiwi
            , T.p2
            , T.flex
            , T.flexCol
            , T.justifyBetween
            ]
        ]
        [ HH.a
            [ HP.classes [ T.flex, T.itemsCenter ]
            , HP.href url
            , HP.target "_blank"
            , HP.rel "noreferrer noopener nofollow"
            ]
            [ HH.img [ HP.classes [ T.w4, T.h4, T.mr2 ], HP.src $ "https://s2.googleusercontent.com/s2/favicons?domain_url=" <> url ]
            , HH.div [ HP.classes [ T.textGray400, T.textSm, T.fontMedium, T.truncate ] ] [ HH.text title ]
            ]
        , shortUrl url
        , HH.div
            [ HP.classes [ T.flex, T.justifyBetween, T.itemsCenter, T.mt2 ] ]
            [ HH.div
                [ HP.classes [ T.flex ] ]
                [ HH.div
                    [ HP.classes
                        [ T.textSm
                        , cx T.textKiwi $ isJust completed_at
                        , cx T.textGray300 $ isNothing completed_at
                        , T.fontBold
                        , T.mr2
                        , T.w4
                        , T.textCenter
                        ]
                    ]
                    [ HH.text $ if isJust completed_at then "D" else "P" ]
                , maybeElem (filter (not <<< null) $ map _.tags $ find ((list == _) <<< _.id) lists) \tags ->
                    HH.div
                      [ HP.classes [ T.flex ] ]
                      $ map tag tags
                ]
            , maybeElem (find ((list == _) <<< _.id) lists) \l ->
                HH.div
                  [ HP.classes [ T.textXs, T.mr2 ] ]
                  [ HH.span [ HP.classes [ T.textGray200 ] ] [ HH.text "List: " ]
                  , HH.span [ HP.classes [ T.textGray300, T.fontMedium ] ] [ HH.text l.title ]
                  ]
            ]
        ]