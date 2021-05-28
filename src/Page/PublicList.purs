module Listasio.Page.PublicList where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Array.NonEmpty as NEA
import Data.Either (note)
import Data.Filterable (filter)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Capability.Resource.List (class ManageList, getPublicListBySlug)
import Listasio.Capability.Resource.User (class ManageUser, userBySlug)
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Tag as Tag
import Listasio.Component.HTML.Utils (maybeElem, safeHref, whenElem)
import Listasio.Component.HTML.Wip as Wip
import Listasio.Data.Avatar as Avatar
import Listasio.Data.DateTime (toDisplayMonthDayYear)
import Listasio.Data.List (ListWithIdUserAndMeta)
import Listasio.Data.Profile (ProfileWithIdAndEmail, PublicProfile)
import Listasio.Data.Route (Route(..))
import Listasio.Env (UserEnv)
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RemoteData
import Slug (Slug)
import Tailwind as T
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent as Mouse

type Props
  = { user :: Slug
    , list :: Slug
    , currentUser :: Maybe ProfileWithIdAndEmail
    }

data Action
  = Initialize
  | Receive Props
  | LoadList
  | LoadAuthor
  | Navigate Route Event

type State
  = { authorSlug :: Slug
    , listSlug :: Slug
    , currentUser :: Maybe ProfileWithIdAndEmail
    , list :: RemoteData String ListWithIdUserAndMeta
    , author :: RemoteData String PublicProfile
    }

component
  :: forall q o m r
   . MonadAff m
  => MonadAsk {userEnv :: UserEnv | r} m
  => ManageUser m
  => ManageList m
  => Navigate m
  => H.Component HH.HTML q {list :: Slug, user :: Slug} o m
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
  initialState {list, user, currentUser} =
    { listSlug: list
    , authorSlug: user
    , currentUser
    , list: NotAsked
    , author: NotAsked
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> do
      void $ H.fork $ handleAction LoadList
      void $ H.fork $ handleAction LoadAuthor

    LoadList -> do
      {authorSlug, listSlug} <- H.get

      H.modify_ _ {list = Loading}
      list <- RemoteData.fromEither <$> note "Failed to fetch list" <$> getPublicListBySlug {user: authorSlug, list: listSlug}
      H.modify_ _ {list = list}

    LoadAuthor -> do
      {authorSlug} <- H.get

      H.modify_ _ {author = Loading}
      author <- RemoteData.fromEither <$> note "Failed to fetch list author" <$> userBySlug authorSlug
      H.modify_ _ {author = author}

    Receive {currentUser} ->
      H.modify_ _ {currentUser = currentUser}

    Navigate route e -> navigate_ e route

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render {list, author, authorSlug, currentUser} =
    HH.div
      []
      [ Wip.elem
      , case list of
          NotAsked -> HH.text ""

          -- TODO: loading skeleton?
          Loading -> HH.text "Loading ..."

          Success l -> listCols l

          Failure msg -> HH.text msg
      ]

    where

    listCols :: ListWithIdUserAndMeta -> _
    listCols l@{title, is_public} =
      HH.div
        []
        [ HH.div
            [ HP.classes [ T.flex, T.itemsCenter, T.mb8 ] ]
            [ HH.div
                [ HP.classes [ T.flex, T.itemsCenter ] ]
                [ HH.a
                    (case author of
                      Success {slug} ->
                        [ safeHref $ Profile slug
                        , HE.onClick $ Just <<< Navigate (Profile slug) <<< Mouse.toEvent
                        ]
                      _ -> []
                    )
                    [ Avatar.renderWithDefault Avatar.Sm
                      $ _.avatar =<< RemoteData.toMaybe author
                    ]
                , HH.h1
                    [ HP.classes [ T.text3xl, T.fontBold, T.textGray400, T.ml4 ] ]
                    [ HH.text title ]
                ]
            , whenElem (not is_public) \_ ->
                HH.div
                  [ HP.classes [ T.ml4, T.px1, T.border, T.borderGray200, T.roundedSm, T.textSm, T.textGray300 ] ]
                  [ HH.text "Private" ]
            ]
        , HH.div
            [ HP.classes
                [ T.grid
                , T.gridCols1
                , T.smGridCols2
                , T.lgGridCols3
                , T.gap4
                ]
            ]
            [ HH.div
                []
                [ sectionTitle "Details"
                , listDetails l
                ]
            , HH.div
                []
                [ sectionTitle "Resources"
                , HH.div [ HP.classes [ T.textGray200, T.textLg ] ] [ HH.text "..." ]
                ]
            , HH.div
                [ HP.classes [ T.smColSpan2, T.lgColSpan1 ] ]
                [ sectionTitle "More from this author"
                , HH.div [ HP.classes [ T.textGray200, T.textLg ] ] [ HH.text "..." ]
                ]
            ]
        ]

    listDetails :: ListWithIdUserAndMeta -> _
    listDetails {slug, description, resource_metadata, tags, updated_at, forks_count, likes_count, subscriptions_count} =
      HH.div
        [ HP.classes
            [ T.bgWhite
            , T.p4
            , T.roundedLg
            ]
        ]
        [ maybeElem description \d ->
            HH.div [ HP.classes [ T.textSm, T.textGray400, T.mb4 ] ] [ HH.text d ]
        , stat {count: resource_metadata.count, icon: Icons.document, one: "Resource", many: "Resources"}
        , stat {count: likes_count, icon: Icons.star, one: "Like", many: "Likes"}
        , stat {count: forks_count, icon: Icons.duplicate, one: "Copy", many: "Copies"}
        , stat {count: subscriptions_count, icon: Icons.userAdd, one: "Follow", many: "Follows"}
        , HH.div
            [ HP.classes [ T.flex, T.itemsCenter, T.mb4 ] ]
            [ Icons.clock
                [ Icons.classes [ T.h4, T.w4, T.textGray300, T.mr2 ] ]
            , HH.div
                [ HP.classes [ T.textGray400, T.textSm ] ]
                [ HH.span [ HP.classes [ T.fontBold ] ] [ HH.text $ toDisplayMonthDayYear updated_at ]
                , HH.text " last update"
                ]
            ]
        , maybeElem (NEA.fromArray tags) \ts ->
            HH.div
              []
              [ HH.div
                  [ HP.classes [ T.flex, T.itemsCenter, T.mb2 ] ]
                  [ Icons.tag
                      [ Icons.classes [ T.h4, T.w4, T.textGray300, T.mr2 ] ]
                  , HH.div
                      [ HP.classes [ T.textGray400, T.textSm ] ]
                      [ HH.text " Tags:" ]
                  ]
              , HH.div
                  [ HP.classes [ T.flex ] ]
                  $ map Tag.tag
                  $ NEA.toArray ts

              ]
        , maybeElem (filter ((_ == authorSlug) <<< _.slug) currentUser) \_ ->
            HH.a
              [ safeHref $ EditList authorSlug slug
              , HE.onClick $ Just <<< Navigate (EditList authorSlug slug) <<< Mouse.toEvent
              , HP.classes
                  [ T.flex
                  , T.itemsCenter
                  , T.justifyCenter
                  , T.textWhite
                  , T.wFull
                  , T.py2
                  , T.bgKiwi
                  , T.hoverBgKiwiDark
                  , T.focusOutlineNone
                  , T.focusRing2
                  , T.focusRingKiwiDark
                  , T.focusRingOffset2
                  , T.roundedMd
                  , T.mt8
                  ]
              ]
              [ Icons.cog [ Icons.classes [ T.h6, T.w6, T.mr2 ] ]
              , HH.text "Settings"
              ]
        ]


    stat {count, icon, one, many} =
      whenElem (count > 0) \_ ->
        HH.div
          [ HP.classes [ T.flex, T.itemsCenter, T.mb4 ] ]
          [ icon
              [ Icons.classes [ T.h4, T.w4, T.textGray300, T.mr2 ] ]
          , HH.div
              [ HP.classes [ T.textGray400, T.textSm ] ]
              [ HH.span [ HP.classes [ T.fontBold ] ] [ HH.text $ show count ]
              , HH.text $ " " <> if count > 1 then many else one
              ]
          ]

    sectionTitle title =
      HH.h3 [ HP.classes [ T.textXl, T.fontBold, T.textGray400, T.mb4 ] ] [ HH.text title ]
