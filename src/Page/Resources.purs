module Listasio.Page.Resources where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Array (find, groupBy, null, sortWith)
import Data.Array.NonEmpty (NonEmptyArray, head, toArray)
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
import Listasio.Component.HTML.Utils (cx, maybeElem, whenElem)
import Listasio.Data.List (ListWithIdAndUser)
import Listasio.Data.Profile (Profile)
import Listasio.Data.Resource (ListResource)
import Listasio.Data.Route (Route(..))
import Listasio.Env (UserEnv)
import Network.RemoteData (RemoteData(..), fromEither)
import Tailwind as T
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

-- grouped -- :: Map String (NonEmptyArray ListResource)
--   # map toArray
--   # fold
--   # GroupNone

groupResources :: Array ListResource -> GroupedResources
groupResources all = { byList, byDate, all }
  where
  byList =
    all
      # sortWith _.list
      # groupBy (\a b -> a.list == b.list)
      # map (\is -> Tuple (_.list $ head is) is)
      # fromFoldable
  byDate =
    all
      # filter (isJust <<< _.completed_at)
      # sortWith _.list
      # groupBy (\a b -> a.list == b.list)
      # map (\is -> Tuple (_.list $ head is) is)
      # fromFoldable

type GroupedResources
  = { byList :: Map String (NonEmptyArray ListResource)
    , byDate :: Map String (NonEmptyArray ListResource) -- TODO: key :: { year :: Int, month :: Month }
    , all :: Array ListResource
    }

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
        [ HP.classes [ T.flex, T.my6, T.spaceX4 ] ]
        [ HH.div
            []
            [ HH.div
                [ HP.classes [ T.w40, T.flex, T.itemsCenter, T.justifyBetween, T.my4 ] ]
                [ HH.span
                    [ HP.classes [ T.textSm, T.fontMedium, T.textGray400] ]
                    [ HH.text "Group by list" ]
                , toggle (Just ToggleGroupByList) false $ groupBy == GroupList
                ]
            , HH.div
                [ HP.classes [ T.w40, T.flex, T.itemsCenter, T.justifyBetween, T.my4 ] ]
                [ HH.span
                    [ HP.classes [ T.textSm, T.fontMedium, T.textGray400] ]
                    [ HH.text "Group by date" ]
                , toggle (Just ToggleGroupByDate) false $ groupBy == GroupDate
                ]
            ]
        , HH.div
            []
            [ HH.div
                [ HP.classes [ T.w40, T.flex, T.itemsCenter, T.justifyBetween, T.my4 ] ]
                [ HH.span
                    [ HP.classes [ T.textSm, T.fontMedium, T.textGray400] ]
                    [ HH.text "Only Done" ]
                , toggle (Just ToggleShowDone) (groupBy == GroupDate) $ filterByDone == ShowDone || groupBy == GroupDate
                ]
            , HH.div
                [ HP.classes [ T.w40, T.flex, T.itemsCenter, T.justifyBetween, T.my4 ] ]
                [ HH.span
                    [ HP.classes [ T.textSm, T.fontMedium, T.textGray400] ]
                    [ HH.text "Only Pending" ]
                , toggle (Just ToggleShowPending) (groupBy == GroupDate) $ filterByDone == ShowPending && groupBy /= GroupDate
                ]
            ]
        ]

    toggle action disabled on =
      HH.button
        [ HP.type_ HP.ButtonButton
        , HE.onClick \_ -> action
        , HP.disabled disabled
        , HP.classes
            [ cx T.bgGray200 $ not on
            , cx T.bgKiwi on
            , T.relative
            , T.inlineFlex
            , T.flexShrink0
            , T.h6
            , T.w11
            , T.border2
            , T.borderTransparent
            , T.roundedFull
            , T.cursorPointer
            , T.transitionColors
            , T.easeInOut
            , T.duration200
            , T.focusOutlineNone
            , T.focusRing2
            , T.focusRingOffset2
            , T.focusRingKiwi
            , T.disabledCursorNotAllowed
            , T.disabledOpacity50
            ]
        ]
        [ HH.span [ HP.classes [ T.srOnly ] ] [ HH.text "Use setting" ]
        , HH.span
            [ HP.classes
                [ cx T.translateX0 $ not on
                , cx T.translateX5 on
                , T.inlineBlock
                , T.h5
                , T.w5
                , T.roundedFull
                , T.bgWhite
                , T.shadow
                , T.transform
                , T.ring0
                , T.transition
                , T.easeInOut
                , T.duration200
                ]
            ]
            []
        ]

    listFeed listId items =
      maybeElem (find ((listId == _) <<< _.id) lists) \l ->
        HH.div
          []
          [ HH.div [ HP.classes [ T.textXl, T.textGray400, T.mt6, T.mb4 ] ] [ HH.text l.title ]
          , HH.div
              [ HP.classes [ T.grid, T.gridCols3, T.gap4 ] ]
              $ map item
              $ filterByDoneFn
              $ toArray items
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
                [ HP.classes [ T.grid, T.gridCols3, T.gap4 ] ]
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
                $ mapWithIndex listFeed grouped.byDate

        Failure msg ->
          HH.div
            []
            [ HH.text msg ]
        _ ->
          HH.div [] [ HH.text "Loading ..." ]

    lists = fromMaybe [] mbLists

    item { url, title, list, completed_at } =
      HH.div
        [ HP.classes [ T.mb4 ] ]
        [ HH.a
            [ HP.classes
                [ T.inlineBlock
                , T.textGray300
                , T.textSm, T.mb1
                , T.mr2
                , T.flex
                , T.py1
                , T.px2
                , T.hoverTextWhite
                , T.hoverBgDurazno
                , T.roundedMd
                ]
            , HP.href url
            , HP.target "_blank"
            , HP.rel "noreferrer noopener nofollow"
            ]
            [ HH.img [ HP.classes [ T.inlineBlock, T.mr1, T.w4, T.h4 ], HP.src $ "https://s2.googleusercontent.com/s2/favicons?domain_url=" <> url ]
            , HH.text title
            ]
        , HH.div
            []
            [ whenElem (groupBy /= GroupList) \_ ->
                maybeElem (find ((list == _) <<< _.id) lists) \l ->
                  HH.span
                    [ HP.classes [ T.textXs, T.mr2 ] ]
                    [ HH.span [ HP.classes [ T.textGray200 ] ] [ HH.text "From: " ]
                    , HH.span [ HP.classes [ T.textGray300 ] ] [ HH.text l.title ]
                    ]
            , whenElem (filterByDone == ShowAll && groupBy /= GroupDate) \_ ->
                HH.span
                  [ HP.classes
                      [ T.textXs
                      , T.textWhite
                      , cx T.bgKiwi $ isJust completed_at
                      , cx T.bgManzana $ isNothing completed_at
                      , T.bgOpacity75
                      , T.roundedSm
                      , T.px2
                      , T.py1
                      ]
                  ]
                  [ HH.text $ if isJust completed_at then "Done" else "Pending" ]
            ]
        ]
