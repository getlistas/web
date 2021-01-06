module Listasio.Page.Resources where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Array (find, groupBy, null, sortWith)
import Data.Array.NonEmpty (NonEmptyArray, head, toArray)
import Data.Either (Either, note)
import Data.Foldable (fold)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map, fromFoldable, toUnfoldable)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
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
import Listasio.Data.List (ListWithIdAndUser)
import Listasio.Data.Profile (Profile)
import Listasio.Data.Resource (ListResource)
import Listasio.Data.Route (Route(..))
import Listasio.Env (UserEnv)
import Network.RemoteData (RemoteData(..), fromEither, toMaybe)
import Tailwind as T
import Web.Event.Event (Event)

data Action
  = Initialize
  | Receive { currentUser :: Maybe Profile }
  | LoadResources
  | LoadLists
  | Navigate Route Event
  | ToggleGroupByList

data GroupedResources
  = GroupNone (Array ListResource)
  | GroupList (Map String (NonEmptyArray ListResource))

isGroupList :: GroupedResources -> Boolean
isGroupList (GroupList _) = true
isGroupList _ = false

toggleGroupByList :: GroupedResources -> GroupedResources
toggleGroupByList (GroupList grouped) =
  grouped
    # map toArray
    # fold
    # GroupNone
toggleGroupByList (GroupNone items) =
  items
    # sortWith _.list
    # groupBy (\a b -> a.list == b.list)
    # map (\is -> Tuple (_.list $ head is) is)
    # fromFoldable
    # GroupList

type State
  = { currentUser :: Maybe Profile
    , resources :: RemoteData String GroupedResources
    , lists :: Maybe (Array ListWithIdAndUser)
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
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> do
      void $ H.fork $ handleAction LoadLists
      void $ H.fork $ handleAction LoadResources

    LoadResources -> do
      H.modify_ _ { resources = Loading }
      resources <- map GroupNone <$> fromEither <$> noteError <$> getResources
      H.modify_ _ { resources = resources }

    LoadLists -> do
      H.modify_ _ { lists = Nothing }
      lists <- getLists
      H.modify_ _ { lists = lists }

    Receive { currentUser } ->
      H.modify_ _ { currentUser = currentUser }

    Navigate route e -> navigate_ e route

    ToggleGroupByList ->
      H.modify_ \s -> s { resources = toggleGroupByList <$> s.resources }

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { currentUser, resources, lists: mbLists } =
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
        [ HP.classes [ T.flex, T.my6 ] ]
        [ HH.div
            [ HP.classes [ T.w40, T.flex, T.itemsCenter, T.justifyBetween] ]
            [ HH.span
                [ HP.classes [ T.textSm, T.fontMedium, T.textGray400] ]
                [ HH.text "Group by list" ]
            , toggle $ maybe false isGroupList $ toMaybe resources
            ]
        ]

    toggle on =
      HH.button
        [ HP.type_ HP.ButtonButton
        , HE.onClick \_ -> Just ToggleGroupByList
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
              $ toArray $ map itemSimple items
          ]

    feed =
      case resources of
        Success (GroupNone items) | null items ->
          HH.div [] [ HH.text "No resources added yet :)" ]
        Success (GroupNone items) ->
          HH.div
            [ HP.classes [ T.grid, T.gridCols3, T.gap4 ] ]
            $ map item items
        Failure msg ->
          HH.div
            []
            [ HH.text msg ]
        Success (GroupList grouped) ->
          HH.div []
            $ map snd
            $ toUnfoldable
            $ mapWithIndex listFeed grouped
        _ ->
          HH.div [] [ HH.text "Loading ..." ]

    lists = fromMaybe [] mbLists

    item { url, title, list } =
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
        , maybeElem (find ((list == _) <<< _.id) lists) \l ->
            HH.div
              []
              [ HH.span
                  [ HP.classes [ T.textXs, T.textWhite, T.bgDurazno, T.bgOpacity75, T.roundedSm, T.px2, T.py1 ] ]
                  [ HH.text l.title ]
              ]
        ]

    itemSimple { url, title, list } =
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
        ]


