module Listasio.Component.HTML.PublicResources where

import Prelude

import Data.Either (note)
import Data.Maybe (Maybe(..))
import Data.String (null, trim)
import Type.Proxy (Proxy(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Api.Endpoint (SortingResources(..), defaultSearch)
import Listasio.Capability.Navigate (class Navigate)
import Listasio.Capability.Resource.Resource (class ManageResource, getPublicListResources, searchResources)
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Utils (maybeElem)
import Listasio.Data.ID (ID)
import Listasio.Data.ID as ID
import Listasio.Data.Resource (ListResource)
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RemoteData
import Slug (Slug)
import Tailwind as T
import Util (takeDomain)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

type Slot id = forall query. H.Slot query Void id

_publicResources = Proxy :: Proxy "publicResources"

type Input
  = { listSlug :: Slug
    , listId :: ID
    , authorSlug :: Slug
    }

data Action
  = Initialize
  | SearchChange String
  | LoadResources
    -- meta actions
  | NoOp

type State
  = { authorSlug :: Slug
    , listSlug :: Slug
    , listId :: ID
    , resources :: RemoteData String (Array ListResource)
    , searchQuery :: String
    }

component
  :: forall q o m
   . MonadAff m
  => ManageResource m
  => Navigate m
  => H.Component q Input o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }
  where
  initialState {authorSlug, listSlug, listId} =
    {authorSlug, listSlug, listId, resources: NotAsked, searchQuery: ""}

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize ->
      handleAction LoadResources

    SearchChange newQuery -> do
      {searchQuery: oldQuery} <- H.get
      H.modify_ _ {searchQuery = newQuery}

      when (null newQuery && (oldQuery /= newQuery)) do
        handleAction LoadResources

      when (not $ null newQuery) do
        {listId} <- H.get

        let search = defaultSearch {list = Just listId, search_text = Just newQuery, sort = Just PositionAsc}

        H.modify_ _ {resources = Loading}

        resources <- RemoteData.fromEither <$> note "Failed to load resources" <$> searchResources search

        H.modify_ _ {resources = resources}

    LoadResources -> do
      {authorSlug, listSlug} <- H.get

      H.modify_ _ {resources = Loading}

      resources <- RemoteData.fromEither <$> note "Failed to load resources" <$> getPublicListResources {user: authorSlug, list: listSlug}

      H.modify_ _ {resources = resources}

    NoOp -> pure unit

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render {resources, searchQuery} =
    HH.div
      []
      [ HH.div
        [ HP.classes [ T.mb4 ] ]
        [ HH.div
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
                  , HE.onKeyDown \e -> case KeyboardEvent.code e of
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
                  , HP.disabled $ null searchQuery
                  ]
                  [ Icons.x
                      [ Icons.classes [ T.h5, T.w5 ] ]
                  ]
              ]
        ]
      , case resources of
          NotAsked -> HH.text ""

          -- TODO: skeleton
          Loading -> HH.text "..."

          Success [] | not $ null $ trim searchQuery ->
            HH.div
              [ HP.classes [ T.bgWhite, T.p4, T.roundedLg, T.textGray300, T.textXl ] ]
              [ HH.text "No results found 😅" ]

          Success rs ->
            HK.div
              [ HP.classes [ T.flex, T.flexCol ] ]
              $ listResource <$> rs

          -- TODO: error message element
          Failure msg -> HH.text msg
      ]

    where
    listResource :: ListResource -> Tuple String _
    listResource {id, url, thumbnail, title, description} =
      Tuple (ID.toString id)
        $ HH.div
            [ HP.classes
                [ T.flex
                , T.justifyBetween
                , T.border
                , T.borderKiwi
                , T.roundedLg
                , T.bgWhite
                , T.mb4
                ]
            ]
            [ HH.div
                [ HP.classes [ T.p4, T.truncate ] ]
                [ HH.a
                    [ HP.classes [ T.textGray400, T.hoverTextKiwi, T.hoverUnderline, T.fontMedium, T.truncate ]
                    , HP.href url
                    , HP.target "_blank"
                    , HP.rel "noreferrer noopener nofollow"
                    ]
                    [ HH.text title ]

                , maybeElem (takeDomain url) \short ->
                    HH.div
                      [ HP.classes [ T.flex, T.itemsCenter, T.mt1 ] ]
                      [ HH.img [ HP.classes [ T.w4, T.h4, T.mr2 ], HP.src $ "https://s2.googleusercontent.com/s2/favicons?domain_url=" <> url ]
                      , HH.div
                          [ HP.classes [ T.textGray300, T.textXs ] ]
                          [ HH.text short ]
                      ]

                , maybeElem description \d ->
                    HH.div
                      [ HP.classes [ T.mt2, T.truncate, T.textGray400, T.textSm ] ]
                      [ HH.text d ]
                ]
            , maybeElem thumbnail \u ->
                HH.div
                  [ HP.classes [ T.hidden, T.smBlock, T.w40, T.h36, T.py4, T.pr4, T.flexShrink0 ] ]
                  [ HH.img
                      [ HP.alt title
                      , HP.src u
                      , HP.classes
                          [ T.wFull
                          , T.hFull
                          , T.objectCover
                          , T.objectCenter
                          , T.roundedMd
                          ]
                      ]
                ]
            ]
