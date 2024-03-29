module Listasio.Component.HTML.PersonalResources where

import Prelude

import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Date (Year)
import Data.Date (year) as Date
import Data.Either (note)
import Data.Enum (toEnum)
import Data.Lens (lastOf, over, preview, set, traversed)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.String (null, trim)
import Data.String.NonEmpty as NES
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
import Listasio.Api.Endpoint (SortingResources(..), defaultSearch)
import Listasio.Capability.Clipboard (class Clipboard, writeText)
import Listasio.Capability.Navigate (class Navigate)
import Listasio.Capability.Now (class Now, nowDateTime, nowDate)
import Listasio.Capability.Resource.List (class ManageList, getLists)
import Listasio.Capability.Resource.Resource (class ManageResource, changePosition, completeResource, deleteResource, getListResources, searchResources, uncompleteResource)
import Listasio.Component.HTML.EditResource as EditResource
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Input as Input
import Listasio.Component.HTML.Modal as Modal
import Listasio.Component.HTML.Tag as Tag
import Listasio.Component.HTML.ToggleGroup as ToggleGroup
import Listasio.Component.HTML.Utils (maybeElem)
import Listasio.Data.DateTime as DateTime
import Listasio.Data.ID (ID)
import Listasio.Data.ID as ID
import Listasio.Data.Lens (_isProcessingAction, _resources)
import Listasio.Data.List (ListWithIdUserAndMeta)
import Listasio.Data.Resource (FilterByDone(..), ListResource, titleOrUrl)
import Listasio.Data.Route (Route(..), routeCodec)
import Listasio.Store as Store
import Network.RemoteData (RemoteData(..), _Success, toMaybe)
import Network.RemoteData as RemoteData
import Routing.Duplex (print)
import Tailwind as T
import Type.Proxy (Proxy(..))
import Util (takeDomain)
import Web.HTML (window) as Window
import Web.HTML.Location as Location
import Web.HTML.Window (location) as Window

type Slot id = H.Slot Query Void id

_slot = Proxy :: Proxy "personalResources"

type Input = { list :: ID }

type Lists = RemoteData String (Array ListWithIdUserAndMeta)

type StoreState
  = { lists :: Lists }

data Action
  = Initialize
  | Receive (Connected StoreState Input)
  | SearchChange String
  | LoadResources
  | LoadLists
  -- editing resources
  | Edit ListResource
  | ResourceEdited EditResource.Output
  | CloseEditModal
  -- resources actions
  | CopyToShare ListResource
  | CopyResourceURL ListResource
  | CompleteResource ListResource
  | UncompleteResource ListResource
  | SkipResource Int ListResource
  | DeleteResource ListResource
  | ConfirmDeleteResource ListResource
  | CancelDeleteResource
  -- filtering
  | ToggleStatusFilter FilterByDone
  -- meta actions
  | WhenNotProcessingAction Action
  | NoOp

data Query a
  = ResourceAdded ListResource a

type State
  =
  { list :: ID
  , lists :: Lists
  , resources :: RemoteData String (Array ListResource)
  , isProcessingAction :: Boolean
  , confirmDelete :: Maybe ID
  , year :: Maybe Year
  , filterByStatus :: FilterByDone
  , searchQuery :: String
  , toEdit :: Maybe ListResource
  }

type ChildSlots
  =
  ( editResource :: EditResource.Slot
  )

insertResourceAt :: Int -> ListResource -> State -> State
insertResourceAt i resource =
  over
    (_resources <<< _Success)
    (\is -> fromMaybe is $ A.insertAt i resource is)

removeResourceById :: ID -> State -> State
removeResourceById id =
  over (_resources <<< _Success) (A.filter ((id /= _) <<< _.id))

modifyResourceById :: ID -> (ListResource -> ListResource) -> State -> State
modifyResourceById id f =
  over (_resources <<< _Success) (map (\r -> if r.id == id then f r else r))

select :: Store.Store -> StoreState
select { lists } = { lists }

component
  :: forall o m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => ManageList m
  => ManageResource m
  => Navigate m
  => Clipboard m
  => Now m
  => H.Component Query Input o m
component = connect (selectEq select) $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , receive = Just <<< Receive
      , initialize = Just Initialize
      }
  }
  where
  initialState { context: { lists }, input: { list } } =
    { lists
    , list
    , resources: NotAsked
    , isProcessingAction: false
    , confirmDelete: Nothing
    , year: toEnum 0
    , filterByStatus: ShowAll
    , searchQuery: ""
    , toEdit: Nothing
    }

  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      year <- Date.year <$> nowDate
      H.modify_ _ { year = Just year }

      handleAction LoadResources

    LoadResources -> do
      { list } <- H.get

      H.modify_ _ { resources = Loading }

      resources <- RemoteData.fromEither <$> note "Failed to load resources" <$> getListResources { list, completed: Nothing }

      H.modify_ _ { resources = resources }

    SearchChange newQuery -> do
      { searchQuery: oldQuery } <- H.get
      H.modify_ _ { searchQuery = newQuery }

      when (null newQuery && (oldQuery /= newQuery)) do
        handleAction LoadResources

      when (not $ null newQuery) do
        { list } <- H.get

        let search = defaultSearch { list = Just list, search_text = NES.fromString newQuery, sort = Just PositionAsc }

        H.modify_ _ { resources = Loading }

        resources <- RemoteData.fromEither <$> note "Failed to load resources" <$> searchResources search

        H.modify_ _ { resources = resources }

    Receive { context: { lists } } -> H.modify_ _ { lists = lists }

    LoadLists -> do
      result <- RemoteData.fromEither <$> note "Could not fetch your lists" <$> getLists
      updateStore $ Store.SetLists result

    -- editing resource

    Edit resource -> do
      { toEdit } <- H.get
      when (isNothing toEdit) do H.modify_ _ { toEdit = Just resource }

    ResourceEdited (EditResource.Updated { new: updated@{ id } }) -> do
      handleAction CloseEditModal
      { list } <- H.get
      when (updated.list == list) do H.modify_ $ modifyResourceById id (const updated)
      when (updated.list /= list) do H.modify_ $ removeResourceById id

    CloseEditModal -> H.modify_ _ { toEdit = Nothing }

    -- resources actions

    CopyToShare { url } -> do
      host <- H.liftEffect $ Location.host =<< Window.location =<< Window.window
      void $ writeText $ host <> print routeCodec (CreateResource { url: Just url, text: Nothing, title: Nothing })

    CopyResourceURL { url } -> void $ writeText url

    CompleteResource toComplete@{ id, completed_at } -> do
      when (not $ isJust completed_at) do
        state <- H.get

        case A.findIndex ((id == _) <<< _.id) =<< preview (_resources <<< _Success) state of
          Just i -> do
            now <- nowDateTime

            H.modify_ $
              modifyResourceById id (_ { completed_at = Just now })
                <<< set _isProcessingAction true

            result <- completeResource toComplete

            when (isNothing result) $ H.modify_ $ insertResourceAt i toComplete
            when (isJust result) $ void $ H.fork $ handleAction LoadLists

            H.modify_ $ set _isProcessingAction false
          Nothing -> pure unit

    UncompleteResource toUndo@{ id, completed_at } -> do
      when (isJust completed_at) do
        state <- H.get

        case A.findIndex ((id == _) <<< _.id) =<< preview (_resources <<< _Success) state of
          Just i -> do
            H.modify_ $
              modifyResourceById id (_ { completed_at = Nothing })
                <<< set _isProcessingAction true

            result <- uncompleteResource toUndo

            when (isNothing result) $ H.modify_ $ insertResourceAt i toUndo
            when (isJust result) $ void $ H.fork $ handleAction LoadLists

            H.modify_ $ set _isProcessingAction false
          Nothing -> pure unit

    SkipResource i toSkip@{ id } -> do
      lastId <- H.gets $ map _.id <<< lastOf (_resources <<< _Success <<< traversed)

      H.modify_
        $ set _isProcessingAction true
            <<< over (_resources <<< _Success) (flip A.snoc toSkip)
            <<< removeResourceById id

      result <- changePosition toSkip { previus: lastId }

      when (isNothing result) $ H.modify_ $ insertResourceAt i toSkip <<< removeResourceById id
      when (isJust result) $ void $ H.fork $ handleAction LoadLists

      H.modify_ $ set _isProcessingAction false

    DeleteResource { id } -> do
      H.modify_ _ { confirmDelete = Just id }

    ConfirmDeleteResource toDelete@{ id } -> do
      { confirmDelete } <- H.get

      mbItems <- H.gets $ preview (_resources <<< _Success)

      when (Just id == confirmDelete) $
        for_ (A.findIndex ((id == _) <<< _.id) =<< mbItems) \i -> do
          H.modify_ $ removeResourceById id <<< set _isProcessingAction true

          result <- deleteResource toDelete

          when (isNothing result) $ H.modify_ $ insertResourceAt i toDelete
          when (isJust result) $ void $ H.fork $ handleAction LoadLists

          H.modify_ $ set _isProcessingAction false

      H.modify_ _ { confirmDelete = Nothing }

    CancelDeleteResource ->
      H.modify_ _ { confirmDelete = Nothing }

    ToggleStatusFilter status ->
      H.modify_ _ { filterByStatus = status }

    WhenNotProcessingAction action -> do
      { isProcessingAction } <- H.get
      when (not isProcessingAction) $ void $ H.fork $ handleAction action

    NoOp -> pure unit

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots o m (Maybe a)
  handleQuery = case _ of
    ResourceAdded resource a -> do
      H.modify_ $ over (_resources <<< _Success) (flip A.snoc resource)

      pure $ Just a

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { lists, isProcessingAction, resources, confirmDelete, year, filterByStatus, searchQuery, toEdit } =
    HH.div
      []
      [ HH.div
          [ HP.classes [ T.flex, T.flexCol, T.smFlexRow, T.justifyBetween, T.gap4, T.mb4 ] ]
          [ HH.div
              [ HP.classes [ T.wFull ] ]
              [ Input.search
                  { value: searchQuery
                  , placeholder: Just "Search resources"
                  , onValueInput: SearchChange
                  , onEscape: SearchChange ""
                  , noOp: NoOp
                  }
              ]
          , HH.div
              [ HP.classes [ T.wFull, T.smWAuto ] ]
              [ ToggleGroup.toggleGroup
                  false
                  [ { label: "All", action: ToggleStatusFilter ShowAll, active: filterByStatus == ShowAll }
                  , { label: "Done", action: ToggleStatusFilter ShowDone, active: filterByStatus == ShowDone }
                  , { label: "Pending", action: ToggleStatusFilter ShowPending, active: filterByStatus == ShowPending }
                  ]
              ]
          ]
      , case filterFn <$> resources of
          NotAsked -> HH.text ""

          -- TODO: skeleton
          Loading -> HH.text "..."

          Success [] ->
            HH.div
              [ HP.classes [ T.bgWhite, T.p4, T.roundedLg, T.textGray300, T.textXl ] ]
              [ HH.text $ case filterByStatus, null $ trim searchQuery of
                  _, false -> "No results found 😅"
                  ShowAll, _ -> "This list is empty 😅"
                  ShowDone, _ -> "No completed resources yet 😅"
                  ShowPending, _ -> "No pending resources 👏"
              ]

          Success rs ->
            HK.div
              [ HP.classes [ T.flex, T.flexCol, T.gap4 ] ]
              $ A.mapWithIndex listResource rs

          -- TODO: error message element
          Failure msg -> HH.text msg
      , Modal.modal (isJust toEdit) ({ onClose: CloseEditModal, noOp: NoOp }) $
          HH.div
            []
            [ HH.div
                [ HP.classes [ T.textCenter, T.textGray400, T.text2xl, T.fontBold, T.mb4 ] ]
                [ HH.text "Edit resource" ]
            , maybeElem toEdit \resource ->
                let
                  input = { lists: fromMaybe [] $ toMaybe lists, resource }
                in
                  HH.slot EditResource._slot unit EditResource.component input ResourceEdited
            ]
      ]

    where
    filterFn =
      case filterByStatus of
        ShowAll -> identity
        ShowDone -> A.filter (isJust <<< _.completed_at)
        ShowPending -> A.filter (isNothing <<< _.completed_at)

    displayDate date =
      maybe (DateTime.toDisplayDayMonth date) (DateTime.toDisplayDayMonthRelative date) year

    mbLastId = (map _.id $ lastOf (_Success <<< traversed) resources)

    isLast id = Just id == mbLastId

    iconAction { icon, action, hoverColor, title, disabled } =
      HH.button
        [ HE.onClick $ const action
        , HP.classes
            [ T.cursorPointer
            , T.py2
            , T.mr4
            , T.disabledCursorNotAllowed
            , T.disabledOpacity50
            , T.textGray300
            , hoverColor
            ]
        , HP.disabled (isProcessingAction || disabled)
        , HP.title title
        ]
        [ icon [ Icons.classes [ T.h5, T.w5 ] ] ]

    listResource :: Int -> ListResource -> Tuple String _
    listResource i resource@{ id, url, thumbnail, completed_at, created_at, description, tags } =
      Tuple (ID.toString id)
        $ HH.div
            [ HP.classes
                [ T.flex
                , T.justifyBetween
                , T.border
                , T.borderKiwi
                , T.roundedLg
                , T.bgWhite
                , T.group
                , T.h44
                ]
            , HE.onMouseLeave $ const CancelDeleteResource
            ]
            [ HH.div
                [ HP.classes [ T.pt4, T.px4, T.pb2, T.flex, T.flexCol, T.justifyBetween, T.truncate ] ]
                [ HH.div
                    [ HP.classes [ T.truncate ] ]
                    [ HH.a
                        [ HP.classes [ T.textGray400, T.hoverTextKiwi, T.hoverUnderline, T.fontMedium, T.truncate ]
                        , HP.href url
                        , HP.target "_blank"
                        , HP.rel "noreferrer noopener nofollow"
                        ]
                        [ HH.text $ titleOrUrl resource ]

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
                    , maybeElem (NEA.fromArray tags) \ts ->
                        HH.div
                          [ HP.classes [ T.mt2, T.flex, T.flexWrap, T.gap1 ] ]
                          $ NEA.toArray
                          $ map Tag.tag ts
                    ]
                , HH.div
                    [ HP.classes [ T.flex, T.itemsCenter, T.groupHoverHidden, T.groupFocusHidden, T.groupFocusWithinHidden, T.mt2 ] ]
                    $ case completed_at of
                        Just date ->
                          [ HH.div
                              [ HP.classes [ T.py2, T.mr2 ] ]
                              [ Icons.check [ Icons.classes [ T.textKiwiDark, T.h5, T.w5 ] ] ]
                          , HH.div
                              [ HP.classes [ T.textGray300, T.textXs ] ]
                              [ HH.text $ "Completed " <> displayDate date ]
                          ]
                        Nothing ->
                          [ HH.div
                              [ HP.classes [ T.py1, T.textGray300, T.textXs ] ]
                              [ HH.text $ "Added " <> displayDate created_at ]
                          ]
                , HH.div
                    [ HP.classes [ T.hidden, T.groupHoverFlex, T.groupFocusFlex, T.groupFocusWithinFlex, T.mt1 ] ]
                    [ case completed_at of
                        Just _ ->
                          iconAction
                            { icon: Icons.x
                            , action: WhenNotProcessingAction $ UncompleteResource resource
                            , hoverColor: T.hoverTextRed700
                            , title: "Mark as pending"
                            , disabled: false
                            }
                        Nothing ->
                          iconAction
                            { icon: Icons.check
                            , action: WhenNotProcessingAction $ CompleteResource resource
                            , hoverColor: T.hoverTextKiwi
                            , title: "Mark as done"
                            , disabled: false
                            }
                    , iconAction
                        { icon: Icons.sortDescending
                        , action: WhenNotProcessingAction $ SkipResource i resource
                        , hoverColor: T.hoverTextKiwi
                        , title: "Move to last"
                        , disabled: isLast id
                        }
                    , case confirmDelete of
                        Just toDelete | toDelete == id ->
                          iconAction
                            { icon: Icons.check
                            , action: WhenNotProcessingAction $ ConfirmDeleteResource resource
                            , hoverColor: T.hoverTextRed700
                            , title: "Confirm delete"
                            , disabled: false
                            }
                        _ ->
                          iconAction
                            { icon: Icons.trash
                            , action: WhenNotProcessingAction $ DeleteResource resource
                            , hoverColor: T.hoverTextRed700
                            , title: "Delete"
                            , disabled: false
                            }
                    , iconAction
                        { icon: Icons.clipboardCopy
                        , action: CopyResourceURL resource
                        , hoverColor: T.hoverTextKiwi
                        , title: "Copy link"
                        , disabled: false
                        }
                    , iconAction
                        { icon: Icons.share
                        , action: CopyToShare resource
                        , hoverColor: T.hoverTextKiwi
                        , title: "Copy share link"
                        , disabled: false
                        }
                    , iconAction
                        { icon: Icons.pencil
                        , action: Edit resource
                        , hoverColor: T.hoverTextKiwi
                        , title: "Edit"
                        , disabled: false
                        }
                    ]
                ]
            , maybeElem thumbnail \u ->
                HH.div
                  [ HP.classes [ T.hidden, T.smBlock, T.w44, T.h44, T.py4, T.pr4, T.flexShrink0 ] ]
                  [ HH.img
                      [ HP.alt $ titleOrUrl resource
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
