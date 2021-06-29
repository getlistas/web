module Listasio.Page.Dashboard where

import Prelude

import Data.Array (null, snoc)
import Data.Either (Either, note)
import Data.Filterable (filter)
import Data.Maybe (Maybe(..))
import Data.MediaType.Common as MediaType
import Data.Traversable (for_, traverse)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as HES
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Listasio.Capability.Clipboard (class Clipboard)
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Capability.Now (class Now)
import Listasio.Capability.Resource.List (class ManageList, getLists)
import Listasio.Capability.Resource.Resource (class ManageResource)
import Listasio.Component.HTML.CreateResource as CreateResource
import Listasio.Component.HTML.List as List
import Listasio.Component.HTML.Modal as Modal
import Listasio.Component.HTML.Utils (maybeElem, safeHref, whenElem)
import Listasio.Data.ID (ID)
import Listasio.Data.List (ListWithIdUserAndMeta)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Route (Route(..))
import Listasio.Store as Store
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RemoteData
import Tailwind as T
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Util as Util
import Web.Clipboard.ClipboardEvent (ClipboardEvent, clipboardData) as Clipboard
import Web.Clipboard.ClipboardEvent.EventTypes (paste) as Clipboard
import Web.Event.Event (Event)
import Web.HTML (window) as Web
import Web.HTML.Event.DataTransfer as DataTransfer
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.UIEvent.MouseEvent as Mouse

_slot :: Proxy "dashboard"
_slot = Proxy

data Action
  = Initialize
  | Receive (Connected (Maybe ProfileWithIdAndEmail) Unit)
  | Navigate Route Event
  | LoadLists
  | PasteUrl Clipboard.ClipboardEvent
  | ToggleCreateResource
  | HandleCreateResource CreateResource.Output
  | HandleCreateResourceForList List.Output
    -- meta actions
  | NoOp

type State
  = { currentUser :: Maybe ProfileWithIdAndEmail
    , lists :: RemoteData String (Array ListWithIdUserAndMeta)
    , showCreateResource :: Boolean
    , pastedUrl :: Maybe String
    , createResourceForThisList :: Maybe ID
    }

type ChildSlots
  = ( createResource :: CreateResource.Slot
    , list :: List.Slot
    )

noteError :: forall a. Maybe a -> Either String a
noteError = note "Could not fetch your lists"

component
  :: forall q o m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => ManageList m
  => ManageResource m
  => Navigate m
  => Clipboard m
  => Now m
  => H.Component q Unit o m
component = connect (selectEq _.currentUser) $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      , initialize = Just Initialize
      }
  }
  where
  initialState {context: currentUser} =
    { currentUser
    , lists: NotAsked
    , showCreateResource: false
    , pastedUrl: Nothing
    , createResourceForThisList: Nothing
    }

  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      void $ H.fork $ handleAction LoadLists
      document <- H.liftEffect $ HTMLDocument.toEventTarget <$> (Web.document =<< Web.window)
      -- unsafeCoerce is fine here, we are only listening to clipboard paste events :)
      -- Halogen does the same ;)
      -- https://github.com/purescript-halogen/purescript-halogen/blob/2f8531168207cda5256dc64da60f791afe3855dc/src/Halogen/HTML/Events.purs#L271-L272
      -- https://github.com/purescript-halogen/purescript-halogen/blob/2f8531168207cda5256dc64da60f791afe3855dc/src/Halogen/HTML/Events.purs#L151-L152
      void $ H.subscribe $ HES.eventListener Clipboard.paste document (Just <<< PasteUrl <<< unsafeCoerce)

    Receive {context: currentUser} -> H.modify_ _ {currentUser = currentUser}

    Navigate route e -> navigate_ e route

    LoadLists -> do
      H.modify_ _ {lists = Loading}
      lists <- RemoteData.fromEither <$> noteError <$> getLists
      H.modify_ _ {lists = lists}

    HandleCreateResource (CreateResource.Created resource) -> do
      H.tell List._slot resource.list $ List.ResourceAdded resource
      H.modify_ _ {showCreateResource = false, pastedUrl = Nothing}

    HandleCreateResourceForList (List.CreateResourceForThisList id) -> do
      H.modify_ _ {createResourceForThisList = Just id, showCreateResource = true}

    ToggleCreateResource ->
      H.modify_ \s -> s
        { showCreateResource = not s.showCreateResource
        , pastedUrl = filter (const $ not s.showCreateResource) s.pastedUrl
        , createResourceForThisList = Nothing
        }

    PasteUrl event -> do
      {showCreateResource, lists} <- H.get
      when (not showCreateResource && RemoteData.isSuccess lists) do
        mbUrl <- H.liftEffect $ filter Util.isUrl <$> traverse (DataTransfer.getData MediaType.textPlain) (Clipboard.clipboardData event)
        for_ mbUrl \url -> H.modify_ _ {showCreateResource = true, pastedUrl = Just url}

    NoOp -> pure unit

  render :: State -> H.ComponentHTML Action ChildSlots m
  render st =
    HH.div
      []
      [ header
      , HH.div [ HP.classes [ T.container ] ] [ feed ]
      ]

    where
    header =
      HH.div
        [ HP.classes [ T.flex, T.justifyBetween, T.wFull, T.pt2 ] ]
        [ HH.h1
            [ HP.classes [ T.textGray400, T.mb6, T.text4xl, T.fontBold ] ]
            [ HH.text "Up next" ]
        , maybeElem (filter (not <<< null) $ RemoteData.toMaybe st.lists) \_ ->
            HH.div
              []
              [ HH.button
                  [ HE.onClick \_ ->
                      if st.showCreateResource
                        then NoOp
                        else ToggleCreateResource
                  , HP.classes
                      [ T.flexNone
                      , T.cursorPointer
                      , T.leadingNormal
                      , T.py2
                      , T.px4
                      , T.textWhite
                      , T.roundedMd
                      , T.bgKiwi
                      , T.hoverBgKiwiDark
                      , T.focusBgKiwiDark
                      , T.focusRing2
                      , T.focusRingKiwiDark
                      , T.focusRingOffset2
                      , T.focusOutlineNone
                      ]
                  ]
                  [ HH.text "Add Resource" ]
              ]
        ]

    feed = case st.lists of
      Success lists ->
        HH.div
          []
          [ HH.div
              [ HP.classes [ T.grid, T.gridCols1, T.mdGridCols2, T.xlGridCols3, T.gap4, T.itemsStart ] ]
              $ snoc
                (map (\list -> HH.slot List._slot list.id List.component {list} (HandleCreateResourceForList)) lists)
                listCreate
          , Modal.modal st.showCreateResource ({onClose: ToggleCreateResource, noOp: NoOp}) $
              HH.div
                []
                [ HH.div
                    [ HP.classes [ T.textCenter, T.textGray400, T.text2xl, T.fontBold, T.mb4 ] ]
                    [ HH.text "Add new resource" ]
                , whenElem st.showCreateResource \_ ->
                    let input = {lists, url: st.pastedUrl, selectedList: st.createResourceForThisList, text: Nothing, title: Nothing}
                      in HH.slot CreateResource._slot unit CreateResource.component input HandleCreateResource
                ]
          ]

      Failure msg ->
        HH.div
          [ HP.classes [ T.p4, T.border4, T.borderRed600, T.bgRed200, T.textRed900 ] ]
          [ HH.p [ HP.classes [ T.fontBold, T.textLg ] ] [ HH.text "Error =(" ]
          , HH.p_ [ HH.text msg ]
          ]

      _ -> HH.div [ HP.classes [ T.textCenter ] ] [ HH.text "Loading ..." ]

    listCreate =
      HH.a
        [ safeHref CreateList
        , HE.onClick $ Navigate CreateList <<< Mouse.toEvent
        , HP.classes [ T.border2, T.borderKiwi, T.roundedMd, T.flex, T.itemsCenter, T.justifyCenter, T.p8, T.bgWhite, T.h56 ]
        ]
        [ HH.div
            [ HP.classes
                [ T.cursorPointer
                , T.flex
                , T.flexCol
                , T.justifyCenter
                , T.itemsCenter
                ]
            ]
            [ HH.span [ HP.classes [ T.text7xl, T.textKiwi, T.leadingNone ] ] [ HH.text "+" ]
            , HH.span [ HP.classes [ T.textGray300 ] ] [ HH.text "Create new list" ]
            ]
        ]
