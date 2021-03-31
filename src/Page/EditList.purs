module Listasio.Page.EditList where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Either (note)
import Data.Lens (preview)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Capability.Navigate (class Navigate, navigate, navigate_)
import Listasio.Capability.Resource.List (class ManageList, deleteList, getListBySlug, updateList)
import Listasio.Component.HTML.Button as Button
import Listasio.Component.HTML.CardsAndSidebar as CardsAndSidebar
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.ListForm as ListForm
import Listasio.Data.Lens (_list)
import Listasio.Data.List (CreateListFields, ListWithIdAndUser)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Route (Route(..))
import Listasio.Env (UserEnv)
import Network.RemoteData (RemoteData(..), _Success)
import Network.RemoteData as RemoteData
import Slug (Slug)
import Tailwind as T
import Web.Event.Event (Event)

data Action
  = Receive { currentUser :: Maybe ProfileWithIdAndEmail, listSlug :: Slug }
  | HandleListForm CreateListFields
  | Navigate Route Event
  | DeleteList
  | DeleteListCancel
  | DeleteListConfirm

type State
  = { currentUser :: Maybe ProfileWithIdAndEmail
    , list :: RemoteData String ListWithIdAndUser
    , slug :: Slug
    , confirmDelete :: Boolean
    }

type Slots = ( formless :: ListForm.Slot )

component
  :: forall q o m r
   . MonadAff m
  => MonadAsk { userEnv :: UserEnv | r } m
  => Navigate m
  => ManageList m
  => H.Component HH.HTML q { listSlug :: Slug } o m
component = Connect.component $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }
  where
  initialState { currentUser, listSlug } =
    { currentUser
    , list: NotAsked
    , slug: listSlug
    , confirmDelete: false
    }

  handleAction :: Action -> H.HalogenM State Action Slots o m Unit
  handleAction = case _ of
    Receive { currentUser } -> do
      st <- H.get
      H.modify_ _ { currentUser = currentUser }
      case st.currentUser, currentUser of
        Nothing, Just { slug } -> do
          H.modify_ _ { list = Loading }
          log $ show { list: st.slug, user: slug }
          list <- RemoteData.fromEither <$> note "Could not get list" <$> getListBySlug { list: st.slug, user: slug }
          H.modify_ _ { list = list }
        _, _ -> pure unit

    Navigate route e -> navigate_ e route

    HandleListForm newList -> do
      mbList <- H.gets $ preview (_list <<< _Success)
      case mbList of
        Nothing -> pure unit
        Just list -> do
          void $ H.query F._formless unit $ F.injQuery $ ListForm.SetCreateStatus Loading unit

          mbCreatedList <- updateList list.id newList

          case mbCreatedList of
            Just createdList -> do
              void $ H.query F._formless unit $ F.injQuery $ ListForm.SetCreateStatus (Success createdList) unit
              H.modify_ _ { list = Success createdList }
            Nothing ->
              void $ H.query F._formless unit $ F.injQuery $ ListForm.SetCreateStatus (Failure "Could not create list") unit

    DeleteList ->
      H.modify_ _ { confirmDelete = true }

    DeleteListCancel ->
      H.modify_ _ { confirmDelete = false }

    DeleteListConfirm -> do
      mbList <- H.gets $ preview (_list <<< _Success)
      case mbList of
        Nothing -> pure unit
        Just list -> do
          -- TODO: deleteList should result `Maybe Unit` instead
          -- TODO: show toast on success
          deleteList list.id
          navigate Dashboard

  render :: State -> H.ComponentHTML Action Slots m
  render {currentUser, list: mbList, confirmDelete} =
    HH.div [] [ header, content ]

    where
    header =
      HH.h1
        [ HP.classes [ T.textGray400, T.mb6, T.text4xl, T.fontBold ] ]
        [ HH.text $ RemoteData.maybe "..." _.title mbList  ]

    mkLayout list cards =
      CardsAndSidebar.layout
        [ { active: true
          , icon: Icons.userCircle
          , label: "List settings"
          , link: Nothing
          }
        , { active: false
          , icon: Icons.gridAdd
          , label: "Integrations"
          , link:
              map
                ( \{ slug } ->
                    { action: Just <<< Navigate (IntegrationsList slug)
                    , route: EditList slug
                    }
                )
                list
          }
        ]
        cards

    content =
      case mbList of
        Success list ->
          mkLayout
            (Just list)
            [ { cta: Nothing
              , content: HH.slot F._formless unit ListForm.formComponent { list: Just list } $ Just <<< HandleListForm
              , title: "Details"
              , description: Nothing
              }
            , { cta: Nothing
              , content: dangerZone list
              , title: "Danger zone"
              , description: Nothing
              }
            ]

        -- TODO: better message
        Failure msg ->
          mkLayout
            Nothing
            [ { cta: Nothing
              , content: HH.div [ HP.classes [ T.textManzana ] ] [ HH.text msg ]
              , title: "Details"
              , description: Nothing
              }
            ]

        -- TODO: better message
        _ ->
          mkLayout
            Nothing
            [ { cta: Nothing
              , content: HH.div [ HP.classes [ T.textGray400 ] ] [ HH.text "Loading ..." ]
              , title: "Details"
              , description: Nothing
              }
            ]

    dangerZone :: ListWithIdAndUser -> _
    dangerZone list =
      HH.div
        [ HP.classes
            [ T.roundedMd
            , T.border2
            , T.borderManzana
            , T.mt4
            , T.p4
            ]
        ]
        [ deleteListEl list ]

    deleteListEl :: ListWithIdAndUser -> _
    deleteListEl list =
      HH.div
        [ HP.classes [ T.px4, T.py5, T.smP6 ] ]
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
                [ case confirmDelete of
                    false ->  Button.danger_ (HH.text "Delete list") false $ Just DeleteList
                    true ->
                      Button.danger
                        (HH.text "Confirm")
                        false
                        (Just DeleteListConfirm)
                        [ HE.onFocusOut \_ -> Just DeleteListCancel ]
                ]
            ]
        ]
