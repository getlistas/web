module Listasio.Page.PublicList where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Either (note)
import Data.Filterable (filter)
import Data.Maybe (Maybe(..), isJust)
import Data.MediaType.Common as MediaType
import Data.Traversable (for_, traverse)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as HES
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Store.Select (selectEq)
import Listasio.Capability.Clipboard (class Clipboard)
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Capability.Now (class Now)
import Listasio.Capability.Resource.List (class ManageList, getLists, getPublicListBySlug)
import Listasio.Capability.Resource.Resource (class ManageResource)
import Listasio.Capability.Resource.User (class ManageUser, userBySlug)
import Listasio.Component.HTML.CreateResource as CreateResource
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Modal as Modal
import Listasio.Component.HTML.PersonalResources as PersonalResources
import Listasio.Component.HTML.PublicResources as PublicResources
import Listasio.Component.HTML.Tag as Tag
import Listasio.Component.HTML.Utils (maybeElem, safeHref, whenElem)
import Listasio.Data.Avatar as Avatar
import Listasio.Data.DateTime (toDisplayMonthDayYear)
import Listasio.Data.List (ListWithIdUserAndMeta)
import Listasio.Data.Profile (ProfileWithIdAndEmail, PublicProfile)
import Listasio.Data.Route (Route(..))
import Listasio.Store as Store
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RemoteData
import Slug (Slug)
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

_slot :: Proxy "publicList"
_slot = Proxy

type Input
  = {user :: Slug, list :: Slug}

type Lists = RemoteData String (Array ListWithIdUserAndMeta)

type StoreState
  = { currentUser :: Maybe ProfileWithIdAndEmail
    , lists :: Lists
    }

data Action
  = Initialize
  | Receive (Connected StoreState Input)
  | LoadList
  | LoadAuthor
  | LoadLists
    -- Adding resources
  | ToggleAddResource
  | PasteUrl Clipboard.ClipboardEvent
  | ResourceAdded CreateResource.Output
    -- Meta
  | Navigate Route Event
  | NoOp

type State
  = { authorSlug :: Slug
    , listSlug :: Slug
    , currentUser :: Maybe ProfileWithIdAndEmail
    , list :: RemoteData String ListWithIdUserAndMeta
    , author :: RemoteData String PublicProfile
    , showAddResource :: Boolean
    , pastedUrl :: Maybe String
    , lists :: Lists
    }

type ChildSlots
  = ( publicResources :: PublicResources.Slot Unit
    , personalResources :: PersonalResources.Slot Unit
    , createResource :: CreateResource.Slot
    )

isAuthor :: State -> Boolean
isAuthor {authorSlug, currentUser} =
  isJust $ filter ((_ == authorSlug) <<< _.slug) currentUser

select :: Store.Store -> StoreState
select {lists, currentUser} = {lists, currentUser}

component
  :: forall q o m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => ManageUser m
  => ManageList m
  => ManageResource m
  => Clipboard m
  => Now m
  => Navigate m
  => H.Component q Input o m
component = connect (selectEq select) $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      , initialize = Just Initialize
      }
  }
  where
  initialState {context: {lists, currentUser}, input: {list, user}} =
    { listSlug: list
    , authorSlug: user
    , currentUser
    , list: NotAsked
    , author: NotAsked
    , showAddResource: false
    , pastedUrl: Nothing
    , lists
    }

  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      void $ H.fork $ handleAction LoadList
      void $ H.fork $ handleAction LoadLists
      void $ H.fork $ handleAction LoadAuthor

      document <- H.liftEffect $ HTMLDocument.toEventTarget <$> (Web.document =<< Web.window)
      -- unsafeCoerce is fine here, we are only listening to clipboard paste events :)
      -- Halogen does the same ;)
      -- https://github.com/purescript-halogen/purescript-halogen/blob/2f8531168207cda5256dc64da60f791afe3855dc/src/Halogen/HTML/Events.purs#L271-L272
      -- https://github.com/purescript-halogen/purescript-halogen/blob/2f8531168207cda5256dc64da60f791afe3855dc/src/Halogen/HTML/Events.purs#L151-L152
      void $ H.subscribe $ HES.eventListener Clipboard.paste document (Just <<< PasteUrl <<< unsafeCoerce)

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

    LoadLists -> do
      {lists, currentUser} <- H.get
      when (isJust currentUser && RemoteData.isNotAsked lists) do
        updateStore $ Store.SetLists Loading
        result <- RemoteData.fromEither <$> note "Could not fetch your lists" <$> getLists
        updateStore $ Store.SetLists result

    PasteUrl event -> do
      st@{showAddResource, list} <- H.get

      when (isAuthor st && not showAddResource && RemoteData.isSuccess list) do
        mbUrl <- H.liftEffect $ filter Util.isUrl <$> traverse (DataTransfer.getData MediaType.textPlain) (Clipboard.clipboardData event)
        for_ mbUrl \url -> H.modify_ _ {showAddResource = true, pastedUrl = Just url}

    ToggleAddResource -> do
      isOwnList <- H.gets isAuthor
      when isOwnList $
        -- TODO: lenses magic xD
        H.modify_ \s -> s
          { showAddResource = not s.showAddResource
          , pastedUrl = filter (const $ not s.showAddResource) s.pastedUrl
          }

    -- CreateResource.Created already handles the global state PogChamp
    ResourceAdded (CreateResource.Created resource) -> do
      isOwnList <- H.gets isAuthor
      when isOwnList do
        H.tell PersonalResources._slot unit $ PersonalResources.ResourceAdded resource
        H.modify_ _ {showAddResource = false, pastedUrl = Nothing}

    Receive {context: {lists, currentUser}} ->
      H.modify_ _ {currentUser = currentUser, lists = lists}

    Navigate route e -> navigate_ e route

    NoOp -> pure unit

  render :: State -> H.ComponentHTML Action ChildSlots m
  render st@{list, author, listSlug, authorSlug, showAddResource, pastedUrl} =
    HH.div
      []
      [ case list of
          NotAsked -> HH.text ""

          -- TODO: loading skeleton
          Loading -> HH.text "Loading ..."

          Success l -> listCols l

          -- TODO: error message
          Failure msg -> HH.text msg
      ]

    where
    isOwnList = isAuthor st

    listCols :: ListWithIdUserAndMeta -> _
    listCols l@{title, is_public} =
      HH.div
        []
        [ HH.div
            [ HP.classes [ T.flex, T.itemsCenter, T.justifyBetween, T.smJustifyStart, T.flexWrap, T.smFlexNowrap, T.gap4, T.mb8 ] ]
            [ HH.a
                (case author of
                  Success {slug} ->
                    [ safeHref $ Profile slug
                    , HE.onClick $ Navigate (Profile slug) <<< Mouse.toEvent
                    ]
                  _ -> []
                )
                [ Avatar.renderWithDefault Avatar.Sm
                  $ _.avatar =<< RemoteData.toMaybe author
                ]
              , HH.h1
                [ HP.classes [ T.text3xl, T.fontBold, T.textGray400, T.orderLast, T.smOrderNone ] ]
                [ HH.text title ]
            , whenElem (not is_public) \_ ->
                HH.div
                  [ HP.classes [ T.px1, T.border, T.borderGray200, T.roundedSm, T.textSm, T.textGray300 ] ]
                  [ HH.text "Private" ]
            ]
        , HH.div
            [ HP.classes
                [ T.grid
                , T.gridCols1
                , T.lgGridCols3
                , T.gap4
                ]
            ]
            [ HH.div
                [ HP.classes [ T.lgSticky, T.lgTop0 ] ]
                [ listDetails l ]
            , HH.div
                [ HP.classes [ T.lgColSpan2 ] ]
                [ if isOwnList
                    then HH.slot PersonalResources._slot unit PersonalResources.component {list: l.id} absurd
                    else HH.slot PublicResources._publicResources unit PublicResources.component {listId: l.id, listSlug, authorSlug} absurd
                ]
            , whenElem isOwnList \_ ->
                Modal.modal showAddResource ({onClose: ToggleAddResource, noOp: NoOp}) $
                  HH.div
                    []
                    [ HH.div
                        [ HP.classes [ T.textCenter, T.textGray400, T.text2xl, T.fontBold, T.mb4 ] ]
                        [ HH.text "Add new resource" ]
                    , whenElem showAddResource \_ ->
                        let input = {lists: [l], url: pastedUrl, selectedList: Just l.id, text: Nothing, title: Nothing}
                          in HH.slot CreateResource._slot unit CreateResource.component input ResourceAdded
                    ]
            ]
        ]

    listDetails {slug, description, resource_metadata, tags, updated_at, forks_count, likes_count, subscriptions_count} =
      HH.div
        [ HP.classes
            [ T.bgWhite
            , T.p4
            , T.roundedLg
            , T.lgSticky
            , T.lgTop4
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
        , whenElem isOwnList \_ ->
            HH.button
              [ HE.onClick $ const ToggleAddResource
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
              [ Icons.plus [ Icons.classes [ T.h6, T.w6, T.mr2 ] ]
              , HH.text "Add Resource"
              ]
        , whenElem isOwnList \_ ->
            HH.a
              [ safeHref $ EditList authorSlug slug
              , HE.onClick $ Navigate (EditList authorSlug slug) <<< Mouse.toEvent
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
                  , T.mt4
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
