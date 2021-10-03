module Listasio.Component.HTML.List where

import Prelude

import Data.Array as A
import Data.Array.NonEmpty (cons')
import Data.Array.NonEmpty as NEA
import Data.DateTime (DateTime)
import Data.Either (note)
import Data.Lens (firstOf, lastOf, lengthOf, over, preview, set, traversed)
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Store.Select (selectEq)
import Listasio.Capability.Clipboard (class Clipboard, writeText)
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Capability.Now (class Now, nowDateTime)
import Listasio.Capability.Resource.Resource (class ManageResource, changePosition, completeResource, deleteResource, getListResources)
import Listasio.Component.HTML.ButtonGroupMenu as ButtonGroupMenu
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Tag as Tag
import Listasio.Component.HTML.Utils (maybeElem, safeHref, whenElem)
import Listasio.Data.DateTime as DateTime
import Listasio.Data.ID (ID)
import Listasio.Data.ID as ID
import Listasio.Data.Lens (_completed_count, _confirmDelete, _count, _isProcessingAction, _last_completed_at, _next, _resource_metadata, _resources, _showMenu)
import Listasio.Data.List (ListWithIdUserAndMeta)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Resource (ListResource)
import Listasio.Data.Route (Route(..), routeCodec)
import Listasio.Store as Store
import Network.RemoteData (RemoteData(..), _Success)
import Network.RemoteData as RemoteData
import Routing.Duplex (print)
import Tailwind as T
import Type.Proxy (Proxy(..))
import Util (takeDomain)
import Web.Event.Event (Event)
import Web.HTML (window) as Window
import Web.HTML.Location as Location
import Web.HTML.Window (location) as Window
import Web.UIEvent.MouseEvent as Mouse

type Slot = H.Slot Query Output ID

_slot = Proxy :: Proxy "list"

type Input
  = {list :: ListWithIdUserAndMeta}

type StoreState
  = Maybe ProfileWithIdAndEmail

data Action
  = Initialize
  | ToggleShowNextMenu
  | CopyToShare ListResource
  | CopyResourceURL ListResource
    -- UpdateResources
  | CompleteResource ListResource
  | SkipResource ListResource
  | DeleteResource
  | ConfirmDeleteResource ListResource
  | RaiseCreateResource ID
    -- meta actions
  | Navigate Route Event
  | Receive (Connected StoreState Input)
  | AndCloseNextMenu Action
  | WhenNotProcessingAction Action

data Query a
  = ResourceAdded ListResource a

data Output
  = CreateResourceForList ID

updateListById :: ID -> (ListWithIdUserAndMeta -> ListWithIdUserAndMeta) -> ListWithIdUserAndMeta -> ListWithIdUserAndMeta
updateListById id f list
  | id == list.id = f list
  | otherwise = list

onComplete :: DateTime -> Maybe ListResource -> ListWithIdUserAndMeta -> ListWithIdUserAndMeta
onComplete now next =
  over (_resource_metadata <<< _completed_count) (_ + 1)
    <<< set (_resource_metadata <<< _last_completed_at) (Just now)
    <<< set (_resource_metadata <<< _next) next

onReverseComplete :: ID -> Maybe DateTime -> Maybe ListResource -> ListWithIdUserAndMeta -> ListWithIdUserAndMeta
onReverseComplete id date next list
  | id == list.id =
    over (_resource_metadata <<< _completed_count) (_ - 1)
      $ set (_resource_metadata <<< _last_completed_at) date
      $ set (_resource_metadata <<< _next) next list
  | otherwise = list

-- TODO: clean up actions & state to only operate on the "Next" (ie first) item
type State
  = { list :: ListWithIdUserAndMeta
    , resources :: RemoteData String (Array ListResource)
    , isProcessingAction :: Boolean
    , confirmDelete :: Maybe Unit
    , showMenu :: Boolean
    , currentUser :: Maybe ProfileWithIdAndEmail
    }

component :: forall m.
     MonadAff m
  => MonadStore Store.Action Store.Store m
  => ManageResource m
  => Clipboard m
  => Now m
  => Navigate m
  => H.Component Query Input Output m
component = connect (selectEq _.currentUser) $ H.mkComponent
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
  initialState {context: currentUser, input} =
    { list: input.list
    , resources: A.singleton <$> RemoteData.fromMaybe input.list.resource_metadata.next
    , isProcessingAction: false
    , showMenu: false
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

    Receive {context: currentUser, input} ->
      H.modify_ _ {currentUser = currentUser, list = input.list}

    ToggleShowNextMenu -> H.modify_ $ over _showMenu not <<< set _confirmDelete Nothing

    CopyToShare {url} -> do
      host <- H.liftEffect $ Location.host =<< Window.location =<< Window.window
      void $ writeText $ host <> print routeCodec (CreateResource {url: Just url, text: Nothing, title: Nothing})

    CopyResourceURL {url} -> void $ writeText url

    AndCloseNextMenu action -> do
      void $ H.fork $ handleAction action
      H.modify_ _ {showMenu = false, confirmDelete = Nothing}

    WhenNotProcessingAction action -> do
      {isProcessingAction} <- H.get
      unless isProcessingAction $ void $ H.fork $ handleAction action

    CompleteResource toComplete -> do
      state <- H.get

      now <- nowDateTime

      H.modify_
        $ over (_resources <<< _Success) (fromMaybe [] <<< A.tail)
            <<< set _isProcessingAction true

      let nextResource = preview (_resources <<< _Success <<< ix 1) state

      updateStore $ Store.OverLists $ map
        $ updateListById state.list.id $ onComplete now nextResource

      result <- completeResource toComplete

      -- Rollback
      when (isNothing result) do
        H.modify_ $ over (_resources <<< _Success) (A.cons toComplete)

        updateStore $ Store.OverLists $ map
          $ onReverseComplete
              state.list.id
              state.list.resource_metadata.last_completed_at
          $ Just toComplete

      H.modify_ $ set _isProcessingAction false

    SkipResource toSkip -> do
      state <- H.get

      H.modify_
        $ set _isProcessingAction true
            <<< over (_resources <<< _Success) (flip A.snoc toSkip)
            <<< over (_resources <<< _Success) (fromMaybe [] <<< A.tail)

      let nextResource = preview (_resources <<< _Success <<< ix 1) state
          lastId = map _.id $ lastOf (_Success <<< traversed) state.resources

      updateStore $ Store.OverLists $ map
        $ updateListById state.list.id $ set (_resource_metadata <<< _next) nextResource

      result <- changePosition toSkip {previus: lastId}

      -- Rollback
      when (isNothing result) do
        H.modify_ $
          over (_resources <<< _Success) (A.cons toSkip)
            <<< over (_resources <<< _Success) (fromMaybe [] <<< A.init)

        updateStore $ Store.OverLists $ map
          $ updateListById state.list.id $ set (_resource_metadata <<< _next) $ Just toSkip

      H.modify_ $ set _isProcessingAction false

    DeleteResource -> do
      H.modify_ _ {confirmDelete = Just unit}

    ConfirmDeleteResource toDelete -> do
      state <- H.get

      for_ state.confirmDelete \_ -> do
        H.modify_
          $ over (_resources <<< _Success) (fromMaybe [] <<< A.init)
              <<< set _isProcessingAction true

        let nextResource = preview (_resources <<< _Success <<< ix 1) state

        updateStore $ Store.OverLists $ map
          $ updateListById state.list.id
          $ over (_resource_metadata <<< _count) (_ - 1)
              <<< set (_resource_metadata <<< _next) nextResource


        result <- deleteResource toDelete

        -- Rollback
        when (isNothing result) do
          H.modify_ $ over (_resources <<< _Success) (A.cons toDelete)

          updateStore $ Store.OverLists $ map
            $ updateListById state.list.id
            $ over (_resource_metadata <<< _count) (_ + 1)
                <<< set (_resource_metadata <<< _next) (Just toDelete)

        H.modify_ $ set _isProcessingAction false

    RaiseCreateResource id -> H.raise $ CreateResourceForList id

    Navigate route e -> navigate_ e route

  handleQuery :: forall slots a. Query a -> H.HalogenM State Action slots Output m (Maybe a)
  handleQuery = case _ of
    ResourceAdded resource a -> do
      H.modify_ $ over (_resources <<< _Success) (flip A.snoc resource)

      pure $ Just a

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render state@{list, showMenu, isProcessingAction, confirmDelete, currentUser} =
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

      {count: 0}, _ ->
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
                  , HE.onClick $ const $ RaiseCreateResource list.id
                  ]
                  [ HH.text "add resource" ]
              ]
          ]

      {count, completed_count}, _ | count == completed_count ->
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
                , maybeElem (NEA.fromArray next.tags) \ts ->
                    HH.div
                      [ HP.classes [ T.mt2, T.flex ] ]
                      $ NEA.toArray
                      $ map Tag.tag ts
                ]
            , HK.div
                [ HP.classes [ T.mt2 ] ]
                [ Tuple
                    (ID.toString next.id)
                    $ ButtonGroupMenu.buttonGroupMenu
                        { mainAction: WhenNotProcessingAction $ CompleteResource next
                        , label: HH.text "Done"
                        , toggleMenu: ToggleShowNextMenu
                        , isOpen: showMenu
                        , disabled: isProcessingAction
                        }
                        $ cons'
                            { action: AndCloseNextMenu $ CopyResourceURL next
                            , label: HH.div
                                       [ HP.classes [ T.flex, T.itemsCenter ] ]
                                       [ Icons.clipboardCopy [ Icons.classes [ T.flexShrink0, T.h5, T.w5, T.textGray200 ] ]
                                       , HH.span [ HP.classes [ T.ml2 ] ] [ HH.text "Copy link" ]
                                       ]
                            , disabled: false
                            }
                            [ { action: AndCloseNextMenu $ CopyToShare next
                              , label: HH.div
                                         [ HP.classes [ T.flex, T.itemsCenter ] ]
                                         [ Icons.share [ Icons.classes [ T.flexShrink0, T.h5, T.w5, T.textGray200 ] ]
                                         , HH.span [ HP.classes [ T.ml2 ] ] [ HH.text "Copy share link" ]
                                         ]
                              , disabled: false
                              }
                            , { action: WhenNotProcessingAction $ AndCloseNextMenu $ SkipResource next
                              , label: HH.div
                                         [ HP.classes [ T.flex, T.itemsCenter ] ]
                                         [ Icons.sortDescending [ Icons.classes [ T.flexShrink0, T.h5, T.w5, T.textGray200 ] ]
                                         , HH.span [ HP.classes [ T.ml2 ] ] [ HH.text "Move to last" ]
                                         ]
                              , disabled: isLast || isProcessingAction
                              }
                            , { action: WhenNotProcessingAction $ maybe
                                          DeleteResource
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
          , HE.onClick (Navigate (route slug) <<< Mouse.toEvent)
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
                    [ HE.onClick $ const $ RaiseCreateResource list.id
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
