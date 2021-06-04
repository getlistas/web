module Listasio.Component.HTML.PersonalResources where

import Prelude

import Data.Array as A
import Data.Either (note)
import Data.Filterable (filter)
import Data.Lens (lastOf, over, preview, set, traversed)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
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
import Listasio.Capability.Navigate (class Navigate)
import Listasio.Capability.Now (class Now, nowDateTime)
import Listasio.Capability.Resource.Resource (class ManageResource, changePosition, completeResource, deleteResource, getListResources)
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Utils (maybeElem, whenElem)
import Listasio.Data.ID (ID)
import Listasio.Data.ID as ID
import Listasio.Data.Lens (_isProcessingAction, _resources)
import Listasio.Data.Resource (ListResource)
import Listasio.Data.Route (Route(..), routeCodec)
import Network.RemoteData (RemoteData(..), _Success)
import Network.RemoteData as RemoteData
import Routing.Duplex (print)
import Tailwind as T
import Util (takeDomain)
import Web.HTML (window) as Window
import Web.HTML.Location as Location
import Web.HTML.Window (location) as Window

type Slot id = forall query. H.Slot query Void id

_personalResources = SProxy :: SProxy "personalResources"

type Input = {list :: ID}

data Action
  = Initialize
  -- resources actions
  | CopyToShare ListResource
  | CopyResourceURL ListResource
  | CompleteResource ListResource
  | SkipResource Int ListResource
  | DeleteResource ListResource
  | ConfirmDeleteResource ListResource
  | CancelDeleteResource
  -- meta actions
  | WhenNotProcessingAction Action

type State
  = { list :: ID
    , resources :: RemoteData String (Array ListResource)
    , isProcessingAction :: Boolean
    , confirmDelete :: Maybe ID
    }

insertResourceAt :: Int -> ListResource -> State -> State
insertResourceAt i resource =
  over
    (_resources <<< _Success)
    (\is -> fromMaybe is $ A.insertAt i resource is)

removeResourceById :: ID -> State -> State
removeResourceById id =
  over (_resources <<< _Success) (filter ((id /= _) <<< _.id))

modifyResourceById :: ID -> (ListResource -> ListResource) -> State -> State
modifyResourceById id f =
  over (_resources <<< _Success) (map (\r -> if r.id == id then f r else r))

component
  :: forall q o m
   . MonadAff m
  => ManageResource m
  => Navigate m
  => Clipboard m
  => Now m
  => H.Component HH.HTML q Input o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }
  where
  initialState {list} =
    { list
    , resources: NotAsked
    , isProcessingAction: false
    , confirmDelete: Nothing
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> do
      {list} <- H.get

      H.modify_ _ {resources = Loading}

      resources <- RemoteData.fromEither <$> note "Failed to load resources" <$> getListResources {list, completed: Nothing}

      H.modify_ _ {resources = resources}

    CopyToShare {url} -> do
      host <- H.liftEffect $ Location.host =<< Window.location =<< Window.window
      void $ writeText $ host <> print routeCodec (CreateResource {url: Just url, text: Nothing, title: Nothing})

    CopyResourceURL {url} -> void $ writeText url

    CompleteResource toComplete@{id, completed_at} -> do
      when (not $ isJust completed_at) do
        state <- H.get

        case A.findIndex ((id == _) <<< _.id) =<< preview (_resources <<< _Success) state of
          Just i -> do
            now <- nowDateTime

            H.modify_ $
              modifyResourceById id (_ {completed_at = Just now})
                <<< set _isProcessingAction true

            result <- completeResource toComplete

            when (isNothing result) $ H.modify_ $ insertResourceAt i toComplete

            H.modify_ $ set _isProcessingAction false
          Nothing -> pure unit

    SkipResource i toSkip@{id} -> do
      lastId <- H.gets $ map _.id <<< lastOf (_resources <<< _Success <<< traversed)

      H.modify_
        $ set _isProcessingAction true
            <<< over (_resources <<< _Success) (flip A.snoc toSkip)
            <<< removeResourceById id

      result <- changePosition toSkip {previus: lastId}

      when (isNothing result) $ H.modify_ $ insertResourceAt i toSkip <<< removeResourceById id

      H.modify_ $ set _isProcessingAction false

    DeleteResource {id} -> do
      H.modify_ _ {confirmDelete = Just id}

    ConfirmDeleteResource toDelete@{id} -> do
      {confirmDelete} <- H.get

      mbItems <- H.gets $ preview (_resources <<< _Success)

      when (Just id == confirmDelete) $
        for_ (A.findIndex ((id == _) <<< _.id) =<< mbItems) \i -> do
          H.modify_ $ removeResourceById id <<< set _isProcessingAction true

          result <- deleteResource toDelete

          when (isNothing result) $ H.modify_ $ insertResourceAt i toDelete

          H.modify_ $ set _isProcessingAction false

      H.modify_ _ {confirmDelete = Nothing}

    CancelDeleteResource ->
      H.modify_ _ {confirmDelete = Nothing}

    WhenNotProcessingAction action -> do
      {isProcessingAction} <- H.get
      when (not isProcessingAction) $ void $ H.fork $ handleAction action

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render {isProcessingAction, resources, confirmDelete} =
    case resources of
      NotAsked -> HH.text ""

      -- TODO: skeleton
      Loading -> HH.text "..."

      Success rs ->
        HK.div
          [ HP.classes
              [ T.flex
              , T.flexCol
              , T.gap4
              ]
          ]
          $ A.mapWithIndex listResource rs

      -- TODO: error message element
      Failure msg -> HH.text msg

    where
    mbLastId = (map _.id $ lastOf (_Success <<< traversed) resources)

    isLast id = Just id == mbLastId

    listResource :: Int -> ListResource -> Tuple String _
    listResource i resource@{id, url, thumbnail, title, completed_at, position, description} =
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
                , T.h36
                ]
            ]
            [ HH.div
                [ HP.classes [ T.px4, T.py2, T.flex, T.flexCol, T.justifyBetween ] ]
                [ HH.div
                    []
                    [ HH.a
                        [ HP.classes [ T.textGray400, T.fontMedium, T.truncate ]
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
                          [ HP.classes [ T.mt2, T.lineClamp2, T.textGray400, T.textSm ] ]
                          [ HH.text d ]
                    ]
                , HH.div
                    [ HP.classes [ T.flex, T.mt2 ] ]
                    [ HH.div
                        [ HP.classes [ T.flex ] ]
                        [ HH.div
                            [ HP.classes
                                [ T.textSm
                                , T.fontBold
                                , T.mr2
                                , T.w4
                                , T.py1
                                , T.textCenter
                                ]
                            ]
                            [ whenElem isCompleted \_ ->
                                Icons.check [ Icons.classes [ T.textKiwi, T.h5, T.w5 ] ]
                            ]
                        ]
                    , HH.div
                        [ HP.classes [ T.hidden, T.groupHoverFlex, T.ml4, T.bgWhite, T.roundedMd ] ]
                        [ HH.button
                            [ HE.onClick \_ -> Just $ WhenNotProcessingAction $ CompleteResource resource
                            , HP.classes [ T.cursorPointer, T.mr2, T.py1, T.px2, T.hoverBgKiwi, T.roundedMd, T.disabledCursorNotAllowed, T.disabledOpacity50 ]
                            , HP.disabled (isProcessingAction || isCompleted)
                            ]
                            [ Icons.check [ Icons.classes [ T.flexShrink0, T.h5, T.w5, T.textGray400 ] ] ]
                        , HH.button
                            [ HE.onClick \_ -> Just $ WhenNotProcessingAction $ SkipResource i resource
                            , HP.classes [ T.cursorPointer, T.mr2, T.py1, T.px2, T.hoverBgKiwi, T.roundedMd, T.disabledCursorNotAllowed, T.disabledOpacity50 ]
                            , HP.disabled $ isProcessingAction || isLast id
                            ]
                            [ Icons.sortDescending [ Icons.classes [ T.flexShrink0, T.h5, T.w5, T.textGray400 ] ] ]
                        , if confirmDelete == Just id
                            then
                              HH.button
                                [ HE.onClick \_ -> Just $ WhenNotProcessingAction $ ConfirmDeleteResource resource
                                , HE.onFocusOut \_ -> Just CancelDeleteResource
                                  -- TODO: should it be on this element or on the wrapper?
                                , HE.onMouseLeave \_ -> Just CancelDeleteResource
                                , HP.classes [ T.cursorPointer, T.py1, T.px2, T.hoverBgRed200, T.roundedMd, T.disabledCursorNotAllowed, T.disabledOpacity50 ]
                                , HP.disabled isProcessingAction
                                ]
                                [ Icons.check [ Icons.classes [ T.h5, T.w5, T.textRed700 ] ] ]
                            else
                              HH.button
                                [ HE.onClick \_ -> Just $ WhenNotProcessingAction $ DeleteResource resource
                                , HP.classes [ T.cursorPointer, T.py1, T.px2, T.hoverBgKiwi, T.roundedMd, T.disabledCursorNotAllowed, T.disabledOpacity50 ]
                                , HP.disabled isProcessingAction
                                ]
                                [ Icons.trash [ Icons.classes [ T.h5, T.w5, T.textGray400 ] ] ]
                        ]
                  ]
                ]
            , HH.div
                [ HP.classes [ T.w40, T.flexShrink0 ] ]
                [ maybeElem thumbnail \u ->
                    HH.img
                      [ HP.alt title
                      , HP.src u
                      , HP.classes
                          [ T.wFull
                          , T.hFull
                          , T.objectCover
                          , T.objectCenter
                          , T.roundedRLg
                          ]
                      ]
                ]
            ]
      where
      isCompleted = isJust completed_at
