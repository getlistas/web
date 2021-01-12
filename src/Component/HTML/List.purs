module Listasio.Component.HTML.List where

import Prelude

import Data.Array (deleteAt, findIndex, head, insertAt, null, snoc, tail)
import Data.DateTime (DateTime)
import Data.Either (note)
import Data.Filterable (class Filterable, filter)
import Data.Foldable (length)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Capability.Resource.Resource (class ManageResource, completeResource, deleteResource, getListResources)
import Listasio.Component.HTML.Utils (cx, maybeElem, whenElem)
import Listasio.Data.DateTime as DateTime
import Listasio.Data.ID (ID)
import Listasio.Data.List (ListWithIdAndUser)
import Listasio.Data.Resource (ListResource)
import Network.RemoteData (RemoteData(..), fromEither, toMaybe)
import Tailwind as T
import Util (takeDomain)

type ListResources
  = { items :: Array ListResource
    , total :: Int
    , read :: Int
    , last_done :: Maybe DateTime
    }

type Slot = H.Slot Query Void ID

_list = SProxy :: SProxy "list"

data Action
  = Initialize
  | ToggleShowMore
  | CompleteResource ListResource
  | DeleteResource ListResource

type Input
  = { list :: ListWithIdAndUser }

data Query a
  = ResourceAdded ListResource a

type State
  = { list :: ListWithIdAndUser
    , resources :: RemoteData String ListResources
    , showMore :: Boolean
    , markingAsDone :: Boolean
    }

component :: forall o m.
     MonadAff m
  => ManageResource m
  => H.Component HH.HTML Query Input o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , initialize = Just Initialize
      }
  }
  where
  initialState { list } =
    { list
    , resources: NotAsked
    , showMore: false
    , markingAsDone: false
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> do
      H.modify_ _ { resources = Loading }
      { list } <- H.get
      resources <- fromEither <$> note "Failed to load list resources" <$> getListResources list.id
      H.modify_ _ { resources = resources }

    ToggleShowMore -> H.modify_ \s -> s { showMore = not s.showMore }

    CompleteResource toComplete@{ id } -> do
      -- TODO: uses lenses for F sake!
      -- TODO: update last_done
      mbItems <- map _.items <$> toMaybe <$>_.resources <$> H.get
      case findIndex ((id == _) <<< _.id) =<< mbItems of
        Just i -> do
          H.modify_ \s ->
            s { resources = (\r -> r { items = fromMaybe r.items $ deleteAt i r.items, read = r.read + 1 }) <$> s.resources
              , markingAsDone = true
              }
          result <- completeResource toComplete
          when (isNothing result)
            $ H.modify_ \s ->
              s { resources = (\r -> r { items = fromMaybe r.items $ insertAt i toComplete r.items, read = r.read - 1 }) <$> s.resources }
          H.modify_ _ { markingAsDone = false }
        _ -> pure unit

    DeleteResource toDelete@{ id } -> do
      -- TODO: uses lenses for F sake!
      mbItems <- map _.items <$> toMaybe <$>_.resources <$> H.get
      case findIndex ((id == _) <<< _.id) =<< mbItems of
        Just i -> do
          H.modify_ \s ->
            s { resources = (\r -> r { items = fromMaybe r.items $ deleteAt i r.items, total = r.total - 1 }) <$> s.resources
              , markingAsDone = true
              }
          result <- deleteResource toDelete
          when (isNothing result)
            $ H.modify_ \s ->
              s { resources = (\r -> r { items = fromMaybe r.items $ insertAt i toDelete r.items, total = r.total + 1 }) <$> s.resources }
          H.modify_ _ { markingAsDone = false }
        _ -> pure unit

  handleQuery :: forall slots a. Query a -> H.HalogenM State Action slots o m (Maybe a)
  handleQuery = case _ of
    ResourceAdded resource a -> do
      H.modify_ \s -> s { resources = (\r -> r { items = snoc r.items resource, total = r.total + 1 }) <$> s.resources }
      pure $ Just a

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { list, resources, showMore, markingAsDone } =
    HH.div
      [ HP.classes
          [ T.border2
          , T.borderKiwi
          , T.roundedMd
          , T.flex
          , T.flexCol
          , T.bgWhite
          ]
      ]
      [ header, toRead, footer ]
    where
    tag text =
      HH.span
        [ HP.classes [ T.leadingNormal, T.mr1, T.mb1, T.px2, T.bgDuraznoLight, T.textWhite, T.textXs, T.roundedSm ] ]
        [ HH.text text ]

    shortUrl url =
      maybeElem (takeDomain url) \short ->
        HH.div
          [ HP.classes [ T.textGray300, T.textSm, T.mb1, T.mr2, T.flex, T.itemsCenter ] ]
          [ HH.img [ HP.classes [ T.inlineBlock, T.w4, T.h4, T.mr1 ], HP.src $ "https://s2.googleusercontent.com/s2/favicons?domain_url=" <> url ]
          , HH.text short
          ]

    toRead = case head <$> _.items <$> resources of
      Success Nothing ->
        HH.div
          [ HP.classes
              [ T.px4
              , T.py2
              , T.wFull
              , T.flex
              , T.flexCol
              , T.itemsCenter
              , T.justifyCenter
              , T.textGray200
              , T.fontSemibold
              , T.h40
              ]
          ]
          [ HH.div [] [ HH.text "This list is empty" ]
          , HH.div [] [ HH.text "Add more items!" ]
          ]

      Success (Just next) ->
        HH.div
          [ HP.classes [ T.px4, T.pb2, T.pt4, T.flex, T.flexCol, T.justifyBetween, T.h40 ] ]
          [ HH.a
              [ HP.href next.url
              , HP.target "_blank"
              , HP.rel "noreferrer noopener nofollow"
              , HP.classes [ T.cursorPointer, T.flex ]
              ]
              [ HH.img [ HP.classes [ T.h20, T.w32, T.mr4, T.objectCover ], HP.src $ fromMaybe "https://via.placeholder.com/87" next.thumbnail ]
              , HH.div
                  [ HP.classes [ T.overflowHidden ] ]
                  [ HH.div
                      [ HP.classes [ T.textBase, T.textGray400, T.leadingRelaxed, T.truncate ] ]
                      [ HH.text next.title ]
                  , maybeElem next.description \des ->
                      HH.div [ HP.classes [ T.mt1, T.textSm, T.textGray400, T.truncate3Lines ] ] [ HH.text des ]
                  ]
              ]
          , HH.div
              [ HP.classes [ T.mt4, T.flex, T.justifyBetween, T.itemsStart ] ]
              [ HH.div
                  [ HP.classes [ T.flex, T.flexWrap, T.itemsCenter ] ]
                  [ shortUrl next.url
                  -- TODO: resource tags
                  , whenElem (not $ null list.tags) \_ ->
                      HH.div [ HP.classes [ T.flex, T.flexWrap ] ] $ map tag list.tags
                  ]
              , HH.button
                  [ HE.onClick \_ -> Just $ CompleteResource next
                  , HP.classes
                      [ T.flexNone
                      , T.cursorPointer
                      , T.leadingNormal
                      , T.w32
                      , T.bgKiwi
                      , T.textWhite
                      , T.textXs
                      , T.roundedSm
                      , T.bgOpacity75
                      , T.hoverBgOpacity100
                      , T.disabledCursorNotAllowed
                      , T.disabledOpacity50
                      , T.focusOutlineNone
                      , T.focusRing2
                      , T.focusRingOffset2
                      , T.focusRingOffsetGray10
                      , T.focusRingKiwi
                      ]
                  , HP.disabled markingAsDone
                  ]
                  [ HH.text "Mark as done" ]
              ]
          ]

      Failure _ ->
        HH.div
          [ HP.classes
              [ T.px4
              , T.py2
              , T.wFull
              , T.flex
              , T.flexCol
              , T.itemsCenter
              , T.justifyCenter
              , T.textManzana
              , T.h40
              ]
          ]
          [ HH.text "Failed to load list resources :(" ]

      _ ->
        HH.div
          [ HP.classes
              [ T.px4
              , T.py2
              , T.wFull
              , T.flex
              , T.flexCol
              , T.itemsCenter
              , T.justifyCenter
              , T.textGray400
              , T.h40
              ]
          ]
          [ HH.text "..." ]

    header =
      HH.div
        [ HP.classes [ T.px4, T.py2, T.borderB2, T.borderGray200, T.h16 ] ]
        [ HH.div
            [ HP.classes [ T.flex, T.justifyBetween, T.itemsCenter ] ]
            [ HH.div
                [ HP.classes [ T.text2xl, T.textGray400, T.fontBold, T.truncate ] ]
                [ HH.text list.title ]
            , maybeElem (toMaybe resources) \{ total, read } ->
                HH.div
                  [ HP.classes [ T.ml6 ] ]
                  [ HH.span [ HP.classes [ T.mr2 ] ] [ HH.text "🔗" ]
                  , HH.span [ HP.classes [ T.textLg, T.textGray400 ] ] [ HH.text $ show read ]
                  , HH.span [ HP.classes [ T.textLg, T.textGray400, T.mx1 ] ] [ HH.text "/" ]
                  , HH.span [ HP.classes [ T.textLg, T.textGray300 ] ] [ HH.text $ show total ]
                  ]
            ]
        , maybeElem (_.last_done =<< toMaybe resources) \last_done ->
            HH.div
              [ HP.classes [ T.textSm, T.textGray200 ] ]
              [ HH.text $ "Last seen " <> DateTime.toDisplayDayMonth last_done ]
        ]

    footer =
      HH.div
        [ HP.classes
            [ T.py2
            , T.borderT2
            , T.borderGray200
            , T.flex
            , T.flexCol
            , T.itemsCenter
            , cx T.h16 $ not showMore
            ]
        ]
        [ HH.div
            [ HP.classes [ T.flex, T.justifyCenter ] ]
            [ HH.button
                [ HE.onClick \_ -> Just ToggleShowMore
                , HP.disabled $ not hasMore
                , HP.classes
                    [ T.focusOutlineNone
                    , T.flex
                    , T.flexCol
                    , T.itemsCenter
                    , T.disabledCursorNotAllowed
                    , T.disabledOpacity50
                    ]
                ]
                [ HH.span
                    [ HP.classes [ T.textSm, T.textGray200 ] ]
                    [ HH.text $ if showMore && hasMore then "Show less" else "Show more" ]
                , HH.span
                    [ HP.classes [ T.textGray400 ] ]
                    [ HH.text $ if showMore && hasMore then "▲" else "▼" ]
                ]
            ]
        , whenElem (showMore && hasMore) \_ ->
            HH.div
              [ HP.classes
                  [ T.wFull
                  , T.pt2
                  , T.px4
                  , T.mt2
                  , T.borderT2
                  , T.borderGray200
                  ]
              ]
              $ map nextItem rest
        ]
      where

      mbRest = filterNotEmpty $ tail =<< (filterNotEmpty $ _.items <$> toMaybe resources)
      hasMore = isJust $ (_ > 1) <$> length <$> mbRest
      rest = fromMaybe [] mbRest

      nextItem resource@{ url, title } =
        HH.div
          [ HP.classes
              [ T.mb1
              , T.mr2
              , T.py1
              , T.px2
              , T.hoverTextWhite
              , T.textGray300
              , T.hoverBgDurazno
              , T.roundedMd
              , T.flex
              , T.itemsCenter
              , T.justifyBetween
              , T.listResource
              ]
          ]
          [ HH.a
              [ HP.classes [ T.flex, T.itemsCenter, T.truncate, T.mr4 ]
              , HP.target "_blank"
              , HP.href url
              ]
              [ HH.img [ HP.classes [ T.inlineBlock, T.w4, T.h4, T.mr1 ], HP.src $ "https://s2.googleusercontent.com/s2/favicons?domain_url=" <> url ]
              , HH.div [ HP.classes [ T.truncate ] ] [ HH.text title ]
              ]
          , HH.div
              [ HP.classes [ T.flex, T.listResourceSettings, T.w28 ] ]
              [ HH.button
                  [ HE.onClick \_ -> Just $ CompleteResource resource
                  , HP.classes [ T.cursorPointer, T.mr4 ]
                  ]
                  [ HH.text "✔️" ]
              , HH.button
                  [ HE.onClick \_ -> Just $ DeleteResource resource
                  , HP.classes [ T.cursorPointer ]
                  ]
                  [ HH.text "❌" ]
              ]
          ]

filterNotEmpty :: forall t a. Filterable t => t (Array a) -> t (Array a)
filterNotEmpty = filter (not <<< null)
