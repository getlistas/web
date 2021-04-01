module Listasio.Page.Discover where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Array (cons, find, length, null)
import Data.Either (Either, note)
import Data.Filterable (filter)
import Data.Foldable (elem)
import Data.Lens (over, view)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Capability.Resource.List (class ManageList, discoverLists, forkList, getLists)
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Tag as Tag
import Listasio.Component.HTML.Utils (maybeElem, safeHref, whenElem)
import Listasio.Data.Avatar (Avatar)
import Listasio.Data.Avatar as Avatar
import Listasio.Data.ID (ID)
import Listasio.Data.Lens (_forkInProgress)
import Listasio.Data.List (Author(..), ListWithIdUserAndMeta, PublicList)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Route (Route(..))
import Listasio.Data.Username (Username)
import Listasio.Data.Username as Username
import Listasio.Env (UserEnv)
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RemoteData
import Slug (Slug)
import Tailwind as T
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent as Mouse

data Action
  = Initialize
  | Receive {currentUser :: Maybe ProfileWithIdAndEmail}
  | Navigate Route Event
  | LoadPublicLists
  | LoadMore
  | LoadOwnLists
  | ForkList PublicList

type Items
  = { refreshing :: Boolean
    , items :: Array PublicList
    }

type State
  = { currentUser :: Maybe ProfileWithIdAndEmail
    , lists :: RemoteData String Items
    , ownLists :: RemoteData String (Array ListWithIdUserAndMeta)
    , forkInProgress :: Array ID
    , page :: Int
    , isLast :: Boolean
    }

noteError :: forall a. Maybe a -> Either String a
noteError = note "Could not fetch top lists"

perPage :: Int
perPage = 25

limit :: Maybe Int
limit = Just perPage

isForkingThisList :: PublicList -> State -> Boolean
isForkingThisList {id} =
  isJust <<< find (_ == id) <<< view _forkInProgress

component
  :: forall q o m r
   . MonadAff m
  => MonadAsk {userEnv :: UserEnv | r} m
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
  initialState {currentUser} =
    { currentUser
    , lists: NotAsked
    , page: 1
    , isLast: false
    , ownLists: NotAsked
    , forkInProgress: []
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> do
      void $ H.fork $ handleAction LoadPublicLists
      void $ H.fork $ handleAction LoadOwnLists

    Receive {currentUser} -> H.modify_ _ {currentUser = currentUser}

    Navigate route e -> navigate_ e route

    LoadPublicLists -> do
      H.modify_ _ {lists = Loading}
      mbLists <- discoverLists {limit, skip: Nothing}

      let
        lists = {refreshing: false, items: _} <$> noteError mbLists
        isLast = maybe false ((perPage > _) <<< length) mbLists

      H.modify_ _ {lists = RemoteData.fromEither lists, isLast = isLast}

    LoadMore -> do
      state <- H.get
      H.modify_ _ {lists = map (_ {refreshing = true}) state.lists}

      let
        prev = fromMaybe [] $ _.items <$> RemoteData.toMaybe state.lists
        pagination = {limit, skip: Just $ perPage * state.page}

      mbLists <- discoverLists pagination

      let
        lists = noteError $ {refreshing: false, items: _} <$> (prev <> _) <$> mbLists
        isLast = maybe false ((perPage > _) <<< length) mbLists
        newPage = maybe state.page (const (state.page + 1)) mbLists

      H.modify_ _ {lists = RemoteData.fromEither lists, page = newPage, isLast = isLast}

    LoadOwnLists -> do
      H.modify_ _ {ownLists = Loading}
      lists <- RemoteData.fromEither <$> noteError <$> getLists
      H.modify_ _ {ownLists = lists}

    ForkList list -> do
      isForkingAlready <- H.gets $ elem list.id <<< view _forkInProgress

      unless isForkingAlready do
        H.modify_ $ over _forkInProgress $ cons list.id
        mbForkedList <- forkList list.id
        H.modify_ $ over _forkInProgress $ filter (_ /= list.id)
        case mbForkedList of
          Nothing -> pure unit
          Just _forked -> pure unit

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render state@{currentUser, lists, isLast} =
    HH.div
      []
      [ HH.div
          [ HP.classes [ T.pt2 ] ]
          [ HH.h1
              [ HP.classes [ T.textGray400, T.mb6, T.text4xl, T.fontBold ] ]
              [ HH.text "Discover" ]
          ]
      , wip
      , feed
      ]

    where
    wip =
      HH.div
        [ HP.classes [ T.p2, T.roundedLg, T.bgDurazno, T.smP3, T.mb8 ] ]
        [ HH.div
            [ HP.classes [ T.flex, T.itemsCenter ] ]
            [ HH.span
                [ HP.classes [ T.flex, T.p2, T.roundedLg, T.bgManzana ] ]
                [ Icons.code
                    [ Icons.classes [ T.h6, T.w6, T.textWhite ] ]
                ]
            , HH.p
                [ HP.classes [ T.ml3, T.fontMedium, T.textWhite ] ]
                [ HH.text "Work in progress"
                ]
            ]
        ]

    feed = case lists of
      Success {refreshing, items} ->
        HH.div
          [ HP.classes [ T.flex, T.flexCol ] ]
          [ HH.div
              [ HP.classes [ T.grid, T.gridCols1, T.smGridCols2, T.lgGridCols3, T.gap4, T.itemsStart ] ]
              $ map listInfo items
          , whenElem (not isLast) \_ ->
              HH.div
                [ HP.classes [ T.mt8, T.flex, T.justifyCenter ] ]
                [ button "Load More" (Just LoadMore) refreshing ]
          ]
      Failure msg ->
        HH.div
          [ HP.classes [ T.p4, T.border4, T.borderRed600, T.bgRed200, T.textRed900 ] ]
          [ HH.p [ HP.classes [ T.fontBold, T.textLg ] ] [ HH.text "Error =(" ]
          , HH.p_ [ HH.text msg ]
          ]

      _ -> HH.div [ HP.classes [ T.pt4, T.textCenter ] ] [ HH.text "Loading ..." ]

    authorEl :: forall row. {slug :: Slug, avatar :: Maybe Avatar, name :: Username | row} -> _
    authorEl {slug, avatar, name} =
      HH.a
        [ safeHref $ Profile slug
        , HE.onClick $ Just <<< Navigate (Profile slug) <<< Mouse.toEvent
        ]
        [ HH.div
            [ HP.classes [ T.flex, T.itemsCenter ] ]
            [ Avatar.renderWithDefault Avatar.Xs avatar
            , HH.div [ HP.classes [ T.textSm, T.textGray300, T.ml2 ] ] [ HH.text $ Username.toString name ]
            ]
        ]

    listInfo :: PublicList -> H.ComponentHTML Action slots m
    listInfo list@{title, description, tags, author} =
      HH.div
        [ HP.classes [ T.p2, T.border2, T.borderKiwi, T.roundedMd ] ]
        [ HH.div [ HP.classes [ T.textLg, T.borderB2, T.borderGray200, T.mb4 ] ] [ HH.text title ]
        , maybeElem description \des -> HH.div [ HP.classes [ T.textSm, T.mb4 ] ] [ HH.text des ]
        , whenElem (not $ null tags) \_ ->
            HH.div
              [ HP.classes [ T.flex, T.textSm ] ]
              $ map Tag.tag tags

        , HH.div
            [ HP.classes [ T.mt4, T.flex, T.justifyBetween ] ]
            case author, currentUser of
              -- TODO: use current user avatar
              You, Just me -> [ authorEl { slug: me.slug, name: me.name, avatar: _.avatar =<< currentUser } ]
              Other user, Just _ ->
                [ authorEl user
                , button "Copy list" (Just $ ForkList list) $ isForkingThisList list state
                ]
              Other user, Nothing -> [ authorEl user ]
              _, _ -> [ HH.text "" ]
        ]

button :: forall i p. String -> Maybe p -> Boolean -> HH.HTML i p
button text action disabled =
  HH.button
    [ HP.type_ HP.ButtonButton
    , HP.classes
        [ T.cursorPointer
        , T.py2
        , T.px4
        , T.bgKiwi
        , T.textWhite
        , T.fontSemibold
        , T.textSm
        , T.roundedLg
        , T.hoverBgKiwiDark
        , T.focusOutlineNone
        , T.focusRing2
        , T.focusRingOffset2
        , T.focusRingKiwiDark
        , T.disabledCursorNotAllowed
        , T.disabledOpacity50
        ]
    , HP.disabled disabled
    , HE.onClick \_ -> action
    ]
    [ HH.text text ]
