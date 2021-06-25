module Listasio.Component.HTML.List where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Array (findIndex, insertAt, null, singleton, snoc)
import Data.Array.NonEmpty (cons')
import Data.Either (note)
import Data.Filterable (class Filterable, filter)
import Data.Lens (firstOf, lastOf, lengthOf, over, preview, set, traversed)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Symbol (SProxy(..))
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Capability.Clipboard (class Clipboard, writeText)
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Capability.Now (class Now, nowDateTime)
import Listasio.Capability.Resource.Resource (class ManageResource, changePosition, completeResource, deleteResource, getListResources)
import Listasio.Component.HTML.ButtonGroupMenu as ButtonGroupMenu
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Utils (maybeElem, safeHref, whenElem)
import Listasio.Data.DateTime as DateTime
import Listasio.Data.ID (ID)
import Listasio.Data.ID as ID
import Listasio.Data.Lens (_completed_count, _confirmDelete, _count, _isProcessingAction, _last_completed_at, _list, _resource_metadata, _resources, _showNextMenu)
import Listasio.Data.List (ListWithIdUserAndMeta)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Resource (ListResource)
import Listasio.Data.Route (Route(..), routeCodec)
import Listasio.Env (UserEnv)
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

type Slot = H.Slot Query Output ID

_listSlot = SProxy :: SProxy "list"

data Action
  = Initialize
  | ToggleShowNextMenu
  | CopyToShare ListResource
  | CopyResourceURL ListResource
  | CompleteResource ListResource
  | SkipResource Int ListResource
  | DeleteResource Boolean ListResource
  | ConfirmDeleteResource ListResource
  | Navigate Route Event
  | RaiseCreateResource ID
  | Receive {currentUser :: Maybe ProfileWithIdAndEmail, list :: ListWithIdUserAndMeta}
    -- meta actions
  | AndCloseNextMenu Action
  | WhenNotProcessingAction Action

data Query a
  = ResourceAdded ListResource a

data Output
  = CreateResourceForThisList ID

insertResourceAt :: Int -> ListResource -> State -> State
insertResourceAt i resource =
  over
    (_resources <<< _Success)
    (\is -> fromMaybe is $ insertAt i resource is)

removeResourceById :: ID -> State -> State
removeResourceById id =
  over (_resources <<< _Success) (filter ((id /= _) <<< _.id))

-- TODO: clean up actions & state to only operate on the "Next" (ie first) item
type State
  = { list :: ListWithIdUserAndMeta
    , resources :: RemoteData String (Array ListResource)
    , isProcessingAction :: Boolean
    , confirmDelete :: Maybe ID
    , showNextMenu :: Boolean
    , currentUser :: Maybe ProfileWithIdAndEmail
    }

component :: forall m r.
     MonadAff m
  => MonadAsk {userEnv :: UserEnv | r} m
  => ManageResource m
  => Clipboard m
  => Now m
  => Navigate m
  => H.Component HH.HTML Query {list :: ListWithIdUserAndMeta} Output m
component = Connect.component $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , initialize = Just Initialize
      , receive = Just <<< Receive
      }
  }
  where
  initialState {currentUser, list} =
    { list
    , resources: singleton <$> RemoteData.fromMaybe list.resource_metadata.next
    , isProcessingAction: false
    , showNextMenu: false
    , confirmDelete: Nothing
    , currentUser
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots Output m Unit
  handleAction = case _ of
    Initialize -> do
      H.modify_ $ over _resources $ \rs -> if RemoteData.isSuccess rs then rs else Loading
      {list} <- H.get
      resources <- RemoteData.fromEither <$> note "Failed to load list resources" <$> getListResources {list: list.id, completed: Just false}
      H.modify_ _ {resources = resources}

    Receive {currentUser} ->
      H.modify_ _ {currentUser = currentUser}

    ToggleShowNextMenu -> H.modify_ $ over _showNextMenu not <<< set _confirmDelete Nothing

    CopyToShare {url} -> do
      host <- H.liftEffect $ Location.host =<< Window.location =<< Window.window
      void $ writeText $ host <> print routeCodec (CreateResource {url: Just url, text: Nothing, title: Nothing})

    CopyResourceURL {url} -> void $ writeText url

    AndCloseNextMenu action -> do
      void $ H.fork $ handleAction action
      H.modify_ _ {showNextMenu = false, confirmDelete = Nothing}

    WhenNotProcessingAction action -> do
      {isProcessingAction} <- H.get
      when (not isProcessingAction) $ void $ H.fork $ handleAction action

    CompleteResource toComplete@{id} -> do
      state <- H.get

      case findIndex ((id == _) <<< _.id) =<< preview (_resources <<< _Success) state of
        Just i -> do
          now <- nowDateTime

          H.modify_
            $ removeResourceById id
                <<< set _isProcessingAction true
                <<< over (_list <<< _resource_metadata <<< _completed_count) (_ + 1)
                <<< set (_list <<< _resource_metadata <<< _last_completed_at) (Just now)

          result <- completeResource toComplete

          when (isNothing result) $ H.modify_
            $ over (_list <<< _resource_metadata <<< _completed_count) (_ - 1)
                <<< insertResourceAt i toComplete
                <<< set (_list <<< _resource_metadata <<< _last_completed_at) state.list.resource_metadata.last_completed_at

          H.modify_ $ set _isProcessingAction false
        Nothing -> pure unit

    SkipResource i toSkip@{id} -> do
      lastId <- H.gets $ map _.id <<< lastOf (_resources <<< _Success <<< traversed)

      H.modify_
        $ set _isProcessingAction true
            <<< over (_resources <<< _Success) (flip snoc toSkip)
            <<< removeResourceById id

      result <- changePosition toSkip {previus: lastId}

      when (isNothing result) $ H.modify_ $ insertResourceAt i toSkip <<< removeResourceById id

      H.modify_ $ set _isProcessingAction false

    DeleteResource confirmed toDelete@{id} -> do
      H.modify_ _ {confirmDelete = Just id}
      when confirmed $ void $ H.fork $ handleAction $ ConfirmDeleteResource toDelete

    ConfirmDeleteResource toDelete@{id} -> do
      {confirmDelete} <- H.get

      mbItems <- H.gets $ preview (_resources <<< _Success)

      when (Just id == confirmDelete) $
        for_ (findIndex ((id == _) <<< _.id) =<< mbItems) \i -> do
          H.modify_
            $ removeResourceById id
                <<< set _isProcessingAction true
                <<< over (_list <<< _resource_metadata <<< _count) (_ - 1)

          result <- deleteResource toDelete

          when (isNothing result) $ H.modify_
            $ over (_list <<< _resource_metadata <<< _count) (_ + 1)
                <<< insertResourceAt i toDelete

          H.modify_ $ set _isProcessingAction false

      H.modify_ _ {confirmDelete = Nothing}

    RaiseCreateResource id -> H.raise $ CreateResourceForThisList id

    Navigate route e -> navigate_ e route

  handleQuery :: forall slots a. Query a -> H.HalogenM State Action slots Output m (Maybe a)
  handleQuery = case _ of
    ResourceAdded resource a -> do
      H.modify_
        $ over (_resources <<< _Success) (flip snoc resource)
            <<< over (_list <<< _resource_metadata <<< _count) (_ + 1)

      pure $ Just a

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render state@{list, resources, showNextMenu, isProcessingAction, confirmDelete, currentUser} =
    HH.div
      [ HP.classes
          [ T.border2
          , T.borderKiwi
          , T.roundedMd
          , T.flex
          , T.flexCol
          , T.bgWhite
          , T.h56
          ]
      ]
      [ header, toRead ]
    where
    shortUrl url =
      maybeElem (takeDomain url) \short ->
        HH.div
          [ HP.classes [ T.textGray300, T.textSm, T.mb1, T.mr2, T.flex, T.itemsCenter ] ]
          [ HH.img [ HP.classes [ T.inlineBlock, T.w4, T.h4, T.mr1 ], HP.src $ "https://s2.googleusercontent.com/s2/favicons?domain_url=" <> url ]
          , HH.span [ HP.classes [ T.truncate ] ] [ HH.text short ]
          ]

    toRead = case list.resource_metadata, firstOf (_resources <<< _Success <<< traversed) state of
      _, Just next ->
        nextEl next $ lengthOf (_resources <<< _Success <<< traversed) state == 1

      { count: 0 }, _ ->
        HH.div
          [ HP.classes
              [ T.px4
              , T.py2
              , T.wFull
              , T.hFull
              , T.flex
              , T.flexCol
              , T.itemsCenter
              , T.justifyCenter
              ]
          ]
          [ HH.div
              []
              [ HH.span
                  [ HP.classes [ T.textGray300 ] ]
                  [ HH.text "This list is empty," ]
              , HH.button
                  [ HP.classes
                      [ T.px1
                      , T.textKiwi
                      , T.hoverTextKiwiDark
                      , T.underline
                      , T.focusRing1
                      , T.focusRingKiwi
                      , T.focusOutlineNone
                      ]
                  , HP.type_ HP.ButtonButton
                  , HE.onClick $ \_ -> Just $ RaiseCreateResource list.id
                  ]
                  [ HH.text "add resource" ]
              ]
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
              , T.textGray300
              , T.h40
              ]
          ]
          [ HH.div [] [ HH.text "All done" ]
          , Icons.celebrate []
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

    nextLink {url} content =
      HH.a
        [ HP.href url
        , HP.target "_blank"
        , HP.rel "noreferrer noopener nofollow"
        , HP.classes [ T.cursorPointer, T.flex ]
        ]
        [ content ]


    nextEl :: ListResource -> Boolean -> _
    nextEl next isLast =
      HH.div
        [ HP.classes [ T.flex, T.h40 ] ]
        [ HH.div
            [ HP.classes [ T.wFull, T.p4, T.flex, T.flexCol, T.justifyBetween ] ]
            [ HH.div
                []
                [ nextLink next $
                    HH.div
                      [ HP.classes
                          [ T.fontMedium
                          , T.textGray400
                          , T.hoverTextKiwi
                          , T.hoverUnderline
                          , T.leadingRelaxed
                          , T.lineClamp2
                          ]
                      ]
                      [ HH.text next.title ]
                , nextLink next $
                    HH.div [ HP.classes [ T.mt2 ] ] [ shortUrl next.url ]
                ]
            , HK.div
                [ HP.classes [ T.mt2 ] ]
                [ Tuple
                    (ID.toString next.id)
                    $ ButtonGroupMenu.buttonGroupMenu
                        { mainAction: Just $ WhenNotProcessingAction $ CompleteResource next
                        , label: HH.text "Done"
                        , toggleMenu: Just ToggleShowNextMenu
                        , isOpen: showNextMenu
                        , disabled: isProcessingAction
                        }
                        $ cons'
                            { action: Just $ AndCloseNextMenu $ CopyResourceURL next
                            , label: HH.div
                                       [ HP.classes [ T.flex, T.itemsCenter ] ]
                                       [ Icons.clipboardCopy [ Icons.classes [ T.flexShrink0, T.h5, T.w5, T.textGray200 ] ]
                                       , HH.span [ HP.classes [ T.ml2 ] ] [ HH.text "Copy link" ]
                                       ]
                            , disabled: false
                            }
                            [ { action: Just $ AndCloseNextMenu $ CopyToShare next
                              , label: HH.div
                                         [ HP.classes [ T.flex, T.itemsCenter ] ]
                                         [ Icons.share [ Icons.classes [ T.flexShrink0, T.h5, T.w5, T.textGray200 ] ]
                                         , HH.span [ HP.classes [ T.ml2 ] ] [ HH.text "Copy share link" ]
                                         ]
                              , disabled: false
                              }
                            , { action: Just $ WhenNotProcessingAction $ AndCloseNextMenu $ SkipResource 0 next
                              , label: HH.div
                                         [ HP.classes [ T.flex, T.itemsCenter ] ]
                                         [ Icons.sortDescending [ Icons.classes [ T.flexShrink0, T.h5, T.w5, T.textGray200 ] ]
                                         , HH.span [ HP.classes [ T.ml2 ] ] [ HH.text "Move to last" ]
                                         ]
                              , disabled: isLast || isProcessingAction
                              }
                            , { action: Just $ WhenNotProcessingAction $ maybe
                                          (DeleteResource false next)
                                          (const $ AndCloseNextMenu $ ConfirmDeleteResource next)
                                          confirmDelete
                              , label: HH.div
                                         [ HP.classes [ T.flex, T.itemsCenter ] ]
                                         [ Icons.trash [ Icons.classes [ T.flexShrink0, T.h5, T.w5, T.textGray200 ] ]
                                         , HH.span [ HP.classes [ T.ml2 ] ] [ HH.text $ maybe "Remove" (const "Confirm") confirmDelete ]
                                         ]
                              , disabled: isProcessingAction
                              }
                            ]
                ]

            ]
        , maybeElem next.thumbnail \url ->
            HH.div
              [ HP.classes [ T.w32, T.lgW44, T.py4, T.pr4, T.flexShrink0 ] ]
              [ HH.img [ HP.classes [ T.hFull, T.wFull, T.objectCover, T.roundedMd ], HP.src url ]
              ]

        ]

    listLinkProps route rest =
      case currentUser of
        Just {slug} ->
          [ safeHref $ route slug
          , HE.onClick (Just <<< Navigate (route slug) <<< Mouse.toEvent)
          ]
            <> rest
        Nothing -> rest

    header =
      HH.div
        [ HP.classes [ T.px4, T.py2, T.borderB, T.borderGray200, T.flexShrink0, T.h16 ] ]
        [ HH.div
            [ HP.classes [ T.flex, T.justifyBetween, T.itemsCenter ] ]
            [ HH.a
                ( listLinkProps (flip PublicList list.slug)
                  [ HP.classes
                      [ T.textXl
                      , T.textGray400
                      , T.hoverTextKiwi
                      , T.hoverUnderline
                      , T.fontBold
                      , T.truncate
                      ]
                  ]
                )
                [ HH.text list.title ]
            , HH.div
                [ HP.classes [ T.ml6, T.flex, T.itemsCenter ] ]
                [ whenElem (list.resource_metadata.count > 0) \_ ->
                    HH.div
                      []
                      [ HH.span [ HP.classes [ T.textGray400 ] ] [ HH.text $ show list.resource_metadata.completed_count ]
                      , HH.span [ HP.classes [ T.textGray300, T.mx1 ] ] [ HH.text "/" ]
                      , HH.span [ HP.classes [ T.textGray300 ] ] [ HH.text $ show list.resource_metadata.count ]
                      ]
                , HH.button
                    [ HE.onClick $ \_ -> Just $ RaiseCreateResource list.id
                    , HP.classes [ T.ml2 ]
                    ]
                    [ Icons.plus [ Icons.classes [ T.h5, T.w5, T.textGray300, T.hoverTextGray400 ] ] ]
                , HH.a
                    ( listLinkProps (flip EditList list.slug) [ HP.classes [ T.ml2 ] ] )
                    [ Icons.cog [ Icons.classes [ T.h5, T.w5, T.textGray300, T.hoverTextGray400 ] ] ]
                ]
            ]
        , maybeElem list.resource_metadata.last_completed_at \last_done ->
            HH.div
              [ HP.classes [ T.textSm, T.textGray200 ] ]
              [ HH.text $ "Last done " <> DateTime.toDisplayDayMonth last_done ]
        ]

filterNotEmpty :: forall t a. Filterable t => t (Array a) -> t (Array a)
filterNotEmpty = filter (not <<< null)
