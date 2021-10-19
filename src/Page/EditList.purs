module Listasio.Page.EditList where

import Prelude

import Data.Array as A
import Data.Either (Either, note)
import Data.Lens (preview)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Store.Select (selectEq)
import Listasio.Capability.Navigate (class Navigate, navigate, navigate_)
import Listasio.Capability.Resource.List (class ManageList, deleteList, getListBySlug, updateList)
import Listasio.Component.HTML.Button as Button
import Listasio.Component.HTML.CardsAndSidebar as CardsAndSidebar
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.ListForm as ListForm
import Listasio.Component.HTML.Utils (safeHref)
import Listasio.Data.ID (ID)
import Listasio.Data.Lens (_list)
import Listasio.Data.List (CreateListFields, ListWithIdUserAndMeta)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Route (Route(..))
import Listasio.Store as Store
import Network.RemoteData (RemoteData(..), _Success)
import Network.RemoteData as RemoteData
import Slug (Slug)
import Tailwind as T
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent as Mouse

_slot :: Proxy "editList"
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
  | LoadList
  | Receive (Connected StoreState Input)
  | HandleListForm CreateListFields
  | Navigate Route Event
  | DeleteList
  | DeleteListCancel
  | DeleteListConfirm

type State
  = { currentUser :: Maybe ProfileWithIdAndEmail
    , list :: RemoteData String ListWithIdUserAndMeta
    , listSlug :: Slug
    , userSlug :: Slug
    , confirmDelete :: Boolean
    }

type Slots = (formless :: ListForm.Slot)

failedToFetch :: forall a. Maybe a -> Either String a
failedToFetch = note "Could not get list"

replaceById :: forall r. {id :: ID | r} -> {id :: ID | r} -> {id :: ID | r}
replaceById new old
  | new.id == old.id = new
  | otherwise = old

select :: Store.Store -> StoreState
select {lists, currentUser} = {lists, currentUser}

ensureList :: RemoteData String (Maybe ListWithIdUserAndMeta) -> RemoteData String ListWithIdUserAndMeta
ensureList (Success (Just list)) = Success list
ensureList (Success Nothing) = NotAsked
ensureList (Failure e) = Failure e
ensureList Loading = Loading
ensureList NotAsked = NotAsked

component
  :: forall q o m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => ManageList m
  => H.Component q Input o m
component = connect (selectEq select) $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , receive = Just <<< Receive
      }
  }
  where
  initialState {context: {lists, currentUser}, input: {list, user}} =
    { currentUser
    , list: ensureList $ A.find (eq list <<< _.slug) <$> lists
    , listSlug: list
    , userSlug: user
    , confirmDelete: false
    }

  handleAction :: Action -> H.HalogenM State Action Slots o m Unit
  handleAction = case _ of
    Initialize -> void $ H.fork $ handleAction LoadList

    LoadList -> do
      st <- H.get
      case st.list of
        NotAsked -> do
          H.modify_ _ {list = Loading}

          list <- RemoteData.fromEither <$> failedToFetch <$> getListBySlug {list: st.listSlug, user: st.userSlug}

          -- TODO: since we fetch only when missing, should update store as well?
          H.modify_ _ {list = list}

        _ -> pure unit

    Receive {context: {currentUser, lists}} -> do
      {listSlug} <- H.get

      let list = ensureList $ A.find (eq listSlug <<< _.slug) <$> lists

      H.modify_ _ {currentUser = currentUser, list = list}

      void $ H.fork $ handleAction LoadList

    Navigate route e -> navigate_ e route

    HandleListForm newList -> do
      mbList <- H.gets $ preview (_list <<< _Success)
      for_ mbList \oldList -> do
        void $ H.query F._formless unit $ F.injQuery $ ListForm.SetCreateStatus Loading unit

        mbUpdatedList <- updateList oldList.id newList

        case mbUpdatedList of
          Just updatedList -> do
            void $ H.query F._formless unit $ F.injQuery $ ListForm.SetCreateStatus (Success updatedList) unit
            -- We modify the local one in case there are no global lists
            -- Eg. when loading this page first
            H.modify_ _ {list = Success updatedList}

            updateStore $ Store.OverLists $ map $ replaceById updatedList

          Nothing ->
            void $ H.query F._formless unit $ F.injQuery $ ListForm.SetCreateStatus (Failure "Could not update list") unit

    DeleteList ->
      H.modify_ _ {confirmDelete = true}

    DeleteListCancel ->
      H.modify_ _ {confirmDelete = false}

    DeleteListConfirm -> do
      mbList <- H.gets $ preview (_list <<< _Success)

      -- TODO: show toast on success
      for_ mbList \{id} -> do
        result <- deleteList id
        case result of
          -- TODO show error
          Nothing -> pure unit
          Just _ -> do
            updateStore $ Store.OverLists $ A.filter $ (_ /= id) <<< _.id
            navigate Dashboard

  render :: State -> H.ComponentHTML Action Slots m
  render {list: mbList, confirmDelete, listSlug, userSlug} =
    HH.div [] [ header, content ]

    where
    header =
      HH.div
        [ HP.classes [ T.pt2 ] ]
        [ HH.div
            [ HP.classes [ T.flex, T.itemsCenter, T.justifyBetween ] ]
            [ HH.h1
                [ HP.classes [ T.textGray400, T.mb6, T.text4xl, T.fontBold ] ]
                [ HH.text $ RemoteData.maybe "..." _.title mbList  ]
            , HH.a
                [ safeHref $ PublicList userSlug listSlug
                , HE.onClick $ Navigate (PublicList userSlug listSlug) <<< Mouse.toEvent
                , HP.classes
                    [ T.flex
                    , T.itemsCenter
                    , T.textGray300
                    ]
                ]
                [ Icons.eye [ Icons.classes [ T.w6, T.h6, T.mr2 ] ]
                , HH.text "View list"
                ]
            ]
        ]

    mkLayout cards =
      CardsAndSidebar.layout
        [ { active: true
          , icon: Icons.userCircle
          , label: "Settings"
          , link: Nothing
          }
        , { active: false
          , icon: Icons.gridAdd
          , label: "Integrations"
          , link:
              Just
                { action: Navigate (IntegrationsList userSlug listSlug)
                , route: IntegrationsList userSlug listSlug
                }
          }
        , { active: false
          , icon: Icons.documentAdd
          , label: "Import"
          , link:
              Just
                { action: Navigate (ImportResourcesList userSlug listSlug)
                , route: ImportResourcesList userSlug listSlug
                }
          }
        ]
        cards

    content =
      case mbList of
        Success list ->
          mkLayout
            [ { cta: Nothing
              , content: HH.slot F._formless unit ListForm.formComponent {list: Just list} HandleListForm
              , title: "Details"
              , description: Nothing
              }
            , { cta: Nothing
              , content: dangerZone
              , title: "Danger zone"
              , description: Nothing
              }
            ]

        -- TODO: better message
        Failure msg ->
          mkLayout
            [ { cta: Nothing
              , content: HH.div [ HP.classes [ T.textManzana ] ] [ HH.text msg ]
              , title: "Details"
              , description: Nothing
              }
            ]

        -- TODO: better message
        _ ->
          mkLayout
            [ { cta: Nothing
              , content: HH.div [ HP.classes [ T.textGray400 ] ] [ HH.text "Loading ..." ]
              , title: "Details"
              , description: Nothing
              }
            ]

    dangerZone =
      HH.div
        [ HP.classes
            [ T.roundedMd
            , T.border2
            , T.borderManzana
            , T.mt4
            , T.p4
            ]
        ]
        [ deleteListEl ]

    deleteListEl =
      HH.div
        [ HP.classes [ T.px4, T.py5, T.smP6 ]
        , HE.onMouseLeave $ const DeleteListCancel
        ]
        [ HH.div
            [ HP.classes [ T.smFlex, T.smItemsStart, T.smJustifyBetween ] ]
            [ HH.div
                []
                [ HH.h3
                    [ HP.classes [ T.textLg, T.leading6, T.fontMedium, T.textGray400 ] ]
                    [ HH.text "Delete list" ]
                , HH.div
                    [ HP.classes [ T.mt2, T.maxWXl, T.textSm, T.textGray300 ] ]
                    [ HH.p
                        []
                        [ HH.text "Deleting a list also deletes its resources. Cannot be reverted." ]
                    ]
                ]
            , HH.div
                [ HP.classes
                    [ T.mt5
                    , T.smMt0
                    , T.smMl6
                    , T.smFlexShrink0
                    , T.smFlex
                    , T.smItemsCenter
                    , T.flex
                    , T.justifyCenter
                    , T.z20
                    ]
                ]
                [ if confirmDelete
                    then
                      Button.danger $ (Button.dangerDefaultProps DeleteListConfirm)
                        { label = HH.text "Confirm"
                        , props = [ HE.onFocusOut $ const DeleteListCancel ]
                        , classes = [ T.w32 ]
                        }
                    else
                      Button.danger $ (Button.dangerDefaultProps DeleteList)
                        { label = HH.text "Delete list"
                        , classes = [ T.w32 ]
                        }
                ]
            ]
        ]
