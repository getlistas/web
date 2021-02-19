module Listasio.Component.HTML.List where

import Prelude

import Data.Array (findIndex, insertAt, null, singleton, snoc, tail)
import Data.Array.NonEmpty (cons')
import Data.Either (note)
import Data.Filterable (class Filterable, filter)
import Data.Foldable (length)
import Data.Lens (firstOf, over, preview, set, traversed)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Capability.Clipboard (class Clipboard, writeText)
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Capability.Now (class Now, nowDateTime)
import Listasio.Capability.Resource.Resource (class ManageResource, completeResource, deleteResource, getListResources)
import Listasio.Component.HTML.ButtonGroupMenu as ButtonGroupMenu
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Tag as Tag
import Listasio.Component.HTML.Utils (cx, maybeElem, safeHref, whenElem)
import Listasio.Data.DateTime as DateTime
import Listasio.Data.ID (ID)
import Listasio.Data.Lens (_completed_count, _count, _last_completed_at, _list, _markingAsDone, _resource_metadata, _resources)
import Listasio.Data.List (ListWithIdUserAndMeta)
import Listasio.Data.Resource (ListResource)
import Listasio.Data.Route (Route(..), routeCodec)
import Network.RemoteData (RemoteData(..), _Success)
import Network.RemoteData as RemoteData
import Routing.Duplex (print)
import Tailwind as T
import Util (takeDomain)
import Web.Event.Event (Event)
import Web.HTML (window) as Window
import Web.HTML.Location as Location
import Web.HTML.Window (location) as Window
import Web.UIEvent.MouseEvent as Mouse

type Slot = H.Slot Query Void ID

_listSlot = SProxy :: SProxy "list"

data Action
  = Initialize
  | ToggleShowMore
  | ToggleShowNextMenu
  | AndCloseNextMenu Action
  | CopyToShare ListResource
  | CopyResourceURL ListResource
  | CompleteResource ListResource
  | DeleteResource ListResource
  | Navigate Route Event

type Input
  = { list :: ListWithIdUserAndMeta }

data Query a
  = ResourceAdded ListResource a

insertResourceAt :: Int -> ListResource -> State -> State
insertResourceAt i resource =
  over
    (_resources <<< _Success)
    (\is -> fromMaybe is $ insertAt i resource is)

removeResourceById :: ID -> State -> State
removeResourceById id =
  over (_resources <<< _Success) (filter ((id /= _) <<< _.id))

type State
  = { list :: ListWithIdUserAndMeta
    , resources :: RemoteData String (Array ListResource)
    , showMore :: Boolean
    , markingAsDone :: Boolean
    , showNextMenu :: Boolean
    }

component :: forall o m.
     MonadAff m
  => ManageResource m
  => Clipboard m
  => Now m
  => Navigate m
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
    , resources: singleton <$> RemoteData.fromMaybe list.resource_metadata.next
    , showMore: false
    , markingAsDone: false
    , showNextMenu: false
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> do
      H.modify_ $ over _resources $ \rs -> if RemoteData.isSuccess rs then rs else Loading
      { list } <- H.get
      resources <- RemoteData.fromEither <$> note "Failed to load list resources" <$> getListResources list.id
      H.modify_ _ { resources = resources }

    ToggleShowMore -> H.modify_ \s -> s { showMore = not s.showMore }

    ToggleShowNextMenu -> H.modify_ \s -> s { showNextMenu = not s.showNextMenu }

    CopyToShare { url } -> do
      host <- H.liftEffect $ Location.host =<< Window.location =<< Window.window
      void $ writeText $ host <> print routeCodec (CreateResource { url: Just url })

    CopyResourceURL { url } -> void $ writeText url

    AndCloseNextMenu action -> do
      void $ H.fork $ handleAction action
      H.modify_ _ { showNextMenu = false }

    CompleteResource toComplete@{ id } -> do
      state <- H.get

      case findIndex ((id == _) <<< _.id) =<< preview (_resources <<< _Success) state of
        Just i -> do
          now <- nowDateTime

          H.modify_
            $ removeResourceById id
                <<< set _markingAsDone true
                <<< over (_list <<< _resource_metadata <<< _completed_count) (_ + 1)
                <<< set (_list <<< _resource_metadata <<< _last_completed_at) (Just now)

          result <- completeResource toComplete

          when (isNothing result) $ H.modify_
            $ over (_list <<< _resource_metadata <<< _completed_count) (_ - 1)
                <<< insertResourceAt i toComplete
                <<< set (_list <<< _resource_metadata <<< _last_completed_at) state.list.resource_metadata.last_completed_at

          H.modify_ $ set _markingAsDone true
        Nothing -> pure unit

    DeleteResource toDelete@{ id } -> do
      mbItems <- H.gets $ preview (_resources <<< _Success)

      case findIndex ((id == _) <<< _.id) =<< mbItems of
        Just i -> do
          H.modify_
            $ removeResourceById id
                <<< set _markingAsDone true
                <<< over (_list <<< _resource_metadata <<< _count) (_ - 1)

          result <- deleteResource toDelete

          when (isNothing result) $ H.modify_
            $ over (_list <<< _resource_metadata <<< _count) (_ + 1)
                <<< insertResourceAt i toDelete

          H.modify_ $ set _markingAsDone true
        Nothing -> pure unit

    Navigate route e -> navigate_ e route

  handleQuery :: forall slots a. Query a -> H.HalogenM State Action slots o m (Maybe a)
  handleQuery = case _ of
    ResourceAdded resource a -> do
      H.modify_
        $ over (_resources <<< _Success) (flip snoc resource)
            <<< over (_list <<< _resource_metadata <<< _count) (_ + 1)

      pure $ Just a

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render state@{ list, resources, showMore, showNextMenu, markingAsDone } =
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
    shortUrl url =
      maybeElem (takeDomain url) \short ->
        HH.div
          [ HP.classes [ T.textGray300, T.textSm, T.mb1, T.mr2, T.flex, T.itemsCenter ] ]
          [ HH.img [ HP.classes [ T.inlineBlock, T.w4, T.h4, T.mr1 ], HP.src $ "https://s2.googleusercontent.com/s2/favicons?domain_url=" <> url ]
          , HH.text short
          ]

    toRead = case list.resource_metadata, firstOf (_resources <<< _Success <<< traversed) state of
      _, Just next -> nextEl next

      { count: 0 }, _ ->
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
          , HH.div [] [ HH.text "Add items!" ]
          ]

      { count, completed_count }, _ | count == completed_count ->
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
          [ HH.div [] [ HH.text "All items completed" ]
          , HH.div [] [ HH.text "Add more items!" ]
          ]

      _, _ ->
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
          [ HH.text "Something went wrong" ]

    nextEl :: ListResource -> _
    nextEl next =
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
                    HH.div [ HP.classes [ T.mt1, T.textSm, T.textGray400, T.lineClamp3 ] ] [ HH.text des ]
                ]
            ]
        , HH.div
            [ HP.classes [ T.mt4, T.flex, T.justifyBetween, T.itemsStart ] ]
            [ HH.div
                [ HP.classes [ T.flex, T.flexWrap, T.itemsCenter ] ]
                [ shortUrl next.url
                -- TODO: resource tags
                , whenElem (not $ null list.tags) \_ ->
                    HH.div [ HP.classes [ T.flex, T.flexWrap ] ] $ map Tag.tag list.tags
                ]
            , ButtonGroupMenu.buttonGroupMenu
                { mainAction: Just $ CompleteResource next
                , label: HH.div
                           [ HP.classes [ T.flex, T.itemsCenter ] ]
                           [ Icons.check [ Icons.classes [ T.flexShrink0, T.h5, T.w5, T.textKiwi ] ]
                           , HH.span [ HP.classes [ T.ml2 ] ] [ HH.text "Done" ]
                           ]
                , toggleMenu: Just ToggleShowNextMenu
                , isOpen: showNextMenu
                }
                $ cons'
                    { action: Just $ AndCloseNextMenu $ CopyResourceURL next
                    , label: HH.div
                               [ HP.classes [ T.flex, T.itemsCenter ] ]
                               [ Icons.clipboardCopy [ Icons.classes [ T.flexShrink0, T.h5, T.w5 ] ]
                               , HH.span [ HP.classes [ T.ml2 ] ] [ HH.text "Copy link" ]
                               ]
                    }
                    [ { action: Just $ AndCloseNextMenu $ CopyToShare next
                      , label: HH.div
                                [ HP.classes [ T.flex, T.itemsCenter ] ]
                                [ Icons.share [ Icons.classes [ T.flexShrink0, T.h5, T.w5 ] ]
                                , HH.span [ HP.classes [ T.ml2 ] ] [ HH.text "Copy share link" ]
                                ]
                      }
                    , { action: Just $ AndCloseNextMenu $ DeleteResource next
                      , label: HH.div
                                [ HP.classes [ T.flex, T.itemsCenter ] ]
                                [ Icons.trash [ Icons.classes [ T.flexShrink0, T.h5, T.w5 ] ]
                                , HH.span [ HP.classes [ T.ml2 ] ] [ HH.text "Remove" ]
                                ]
                      }
                    ]
            ]
        ]

    header =
      HH.div
        [ HP.classes [ T.px4, T.py2, T.borderB2, T.borderGray200, T.h16 ] ]
        [ HH.div
            [ HP.classes [ T.flex, T.justifyBetween, T.itemsCenter ] ]
            [ HH.a
                [ HP.classes [ T.text2xl, T.textGray400, T.fontBold, T.truncate ]
                , safeHref $ EditList list.slug
                , HE.onClick (Just <<< Navigate (EditList list.slug) <<< Mouse.toEvent)
                ]
                [ HH.text list.title ]
            , HH.div
                [ HP.classes [ T.ml6 ] ]
                [ HH.span [ HP.classes [ T.textLg, T.textGray400 ] ] [ HH.text $ show list.resource_metadata.completed_count ]
                , HH.span [ HP.classes [ T.textLg, T.textGray300, T.mx1 ] ] [ HH.text "/" ]
                , HH.span [ HP.classes [ T.textLg, T.textGray300 ] ] [ HH.text $ show list.resource_metadata.count ]
                ]
            ]
        , maybeElem list.resource_metadata.last_completed_at \last_done ->
            HH.div
              [ HP.classes [ T.textSm, T.textGray200 ] ]
              [ HH.text $ "Last done " <> DateTime.toDisplayDayMonth last_done ]
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
                    [ HH.text "Next in queue" ]
                , if showMore && hasMore
                    then Icons.chevronUp [ Icons.classes [ T.flexShrink0, T.h5, T.w5, T.textGray400 ] ]
                    else Icons.chevronDown [ Icons.classes [ T.flexShrink0, T.h5, T.w5, T.textGray400 ] ]
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

      mbRest = filterNotEmpty $ tail =<< (filterNotEmpty $ RemoteData.toMaybe resources)
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
              [ HP.classes [ T.flex, T.itemsCenter, T.truncate ]
              , HP.target "_blank"
              , HP.href url
              ]
              [ HH.img [ HP.classes [ T.inlineBlock, T.w4, T.h4, T.mr1 ], HP.src $ "https://s2.googleusercontent.com/s2/favicons?domain_url=" <> url ]
              , HH.div [ HP.classes [ T.truncate, T.textSm ] ] [ HH.text title ]
              ]
          , HH.div
              [ HP.classes [ T.listResourceSettings, T.ml4 ] ]
              [ HH.button
                  [ HE.onClick \_ -> Just $ CompleteResource resource
                  , HP.classes [ T.cursorPointer, T.mr4 ]
                  ]
                  [ Icons.check [ Icons.classes [ T.flexShrink0, T.h5, T.w5, T.textGray400 ] ] ]
              , HH.button
                  [ HE.onClick \_ -> Just $ DeleteResource resource
                  , HP.classes [ T.cursorPointer ]
                  ]
                  [ Icons.trash [ Icons.classes [ T.h5, T.w5, T.textGray400 ] ] ]
              ]
          ]

filterNotEmpty :: forall t a. Filterable t => t (Array a) -> t (Array a)
filterNotEmpty = filter (not <<< null)
