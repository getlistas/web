module Listasio.Page.Discover where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Array (length, null, snoc)
import Data.Either (Either, note)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Capability.Resource.List (class ManageList, discoverLists)
import Listasio.Component.HTML.Header (header)
import Listasio.Component.HTML.Utils (cx, maybeElem, whenElem)
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
  | LoadMore

type Items
  = { refreshing :: Boolean
    , items :: Array ListWithIdAndUser
    }

type State
  = { currentUser :: Maybe Profile
    , lists :: RemoteData String Items
    , page :: Int
    , isLast :: Boolean
    }

noteError :: forall a. Maybe a -> Either String a
noteError = note "Could not fetch top lists"

perPage :: Int
perPage = 20

limit :: Maybe Int
limit = Just perPage

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
  initialState { currentUser } = { currentUser, lists: NotAsked, page: 1, isLast: false }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> void $ H.fork $ handleAction LoadLists

    Receive { currentUser } -> H.modify_ _ { currentUser = currentUser }

    Navigate route e -> navigate_ e route

    LoadLists -> do
      H.modify_ _ { lists = Loading }
      mbLists <- discoverLists { limit, skip: Nothing }

      let
        lists = { refreshing: false, items: _ } <$> noteError mbLists
        isLast = maybe false ((perPage > _) <<< length) mbLists

      H.modify_ _ { lists = RemoteData.fromEither lists, isLast = isLast }

    LoadMore -> do
      state <- H.get
      H.modify_ _ { lists = map (_ { refreshing = true }) state.lists }

      let
        prev = fromMaybe [] $ _.items <$> RemoteData.toMaybe state.lists
        pagination = { limit, skip: Just $ perPage * state.page }

      mbLists <- discoverLists pagination

      let
        lists = noteError $ { refreshing: false, items: _ } <$> (prev <> _) <$> mbLists
        isLast = maybe false ((perPage > _) <<< length) mbLists
        newPage = maybe state.page (const (state.page + 1)) mbLists

      H.modify_ _ { lists = RemoteData.fromEither lists, page = newPage, isLast = isLast }

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { currentUser, lists, isLast } =
    HH.div
      [ HP.classes [ T.minHScreen, T.wScreen, T.flex, T.flexCol, T.itemsCenter ] ]
      [ header currentUser Navigate $ Just Discover
      , HH.div [ HP.classes [ T.mt10 ] ] [ feed ]
      ]
    where
    feed = case lists of
      Success { refreshing, items } ->
        HH.div
          [ HP.classes [ T.flex, T.flexCol ] ]
          $ snoc (map listInfo items)
          $ whenElem (not isLast) \_ -> HH.div [ HP.classes [ T.mt8, T.flex, T.justifyCenter ] ] [ loadMore refreshing ]
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

    loadMore :: Boolean -> H.ComponentHTML Action slots m
    loadMore disabled =
      HH.input
        [ HP.type_ HP.InputButton
        , HP.value "Load more"
        , HP.classes
            [ T.cursorPointer
            , T.py2
            , T.px4
            , T.bgPink300
            , T.textWhite
            , T.fontSemibold
            , T.roundedLg
            , T.shadowMd
            , T.hoverBgPink700
            , T.focusOutlineNone
            , cx T.cursorNotAllowed disabled
            , cx T.opacity50 disabled
            ]
        , HP.disabled disabled
        , HE.onClick \_ -> Just LoadMore
        ]
