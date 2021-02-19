module Listasio.Page.EditList where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Either (note)
import Data.Lens (preview)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Capability.Navigate (class Navigate, navigate, navigate_)
import Listasio.Capability.Resource.List (class ManageList, deleteList, getListBySlug, updateList)
import Listasio.Component.HTML.Layout as Layout
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
  = Initialize
  | Receive { currentUser :: Maybe ProfileWithIdAndEmail, listSlug :: Slug }
  | HandleListForm CreateListFields
  | Navigate Route Event
  | DeleteList

type State
  = { currentUser :: Maybe ProfileWithIdAndEmail
    , list :: RemoteData String ListWithIdAndUser
    , slug :: Slug
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
      , initialize = Just Initialize
      }
  }
  where
  initialState { currentUser, listSlug } =
    { currentUser
    , list: NotAsked
    , slug: listSlug
    }

  handleAction :: Action -> H.HalogenM State Action Slots o m Unit
  handleAction = case _ of
    Initialize -> pure unit

    Receive { currentUser } -> do
      st <- H.get
      H.modify_ _ { currentUser = currentUser }
      case st.currentUser, currentUser of
        Nothing, Just { slug } -> do
          H.modify_ _ { list = Loading }
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
              void $ H.query F._formless unit $ F.injQuery $ ListForm.SetCreateStatus (Success unit) unit
              H.modify_ _ { list = Success createdList }
            Nothing ->
              void $ H.query F._formless unit $ F.injQuery $ ListForm.SetCreateStatus (Failure "Could not create list") unit

    DeleteList -> do
      mbList <- H.gets $ preview (_list <<< _Success)
      case mbList of
        Nothing -> pure unit
        Just list -> do
          -- TODO: deleteList should result `Maybe Unit` instead
          -- TODO: show toast on success
          deleteList list.id
          navigate Dashboard

  render :: State -> H.ComponentHTML Action Slots m
  render { currentUser, list: mbList } =
    Layout.dashboard
      currentUser
      Navigate
      Nothing
      $ HH.div
          []
          [ header
          , HH.div
              [ HP.classes [ T.grid, T.gridCols1, T.mdGridCols2 ] ]
              [ content ]
          ]
    where
    header =
      HH.h1
        [ HP.classes [ T.textGray400, T.mb6, T.text4xl, T.fontBold ] ]
        [ HH.text "List Settings" ]

    content =
      case mbList of
        Success list ->
          HH.div
            []
            [ HH.slot F._formless unit ListForm.formComponent { list: Just list } $ Just <<< HandleListForm
            , HH.section [ HP.classes [ T.mt12 ] ] [ dangerZone list ]
            ]

        -- TODO: better message
        Failure msg -> HH.div [ HP.classes [ T.textManzana ] ] [ HH.text msg ]

        -- TODO: better message
        _ -> HH.text "Loading ..."

    dangerZone :: ListWithIdAndUser -> _
    dangerZone list =
      HH.div
        []
        [ HH.h3
            [ HP.classes [ T.textGray400, T.text2xl, T.fontBold ] ]
            [ HH.text "Danger zone" ]
        , HH.div
            [ HP.classes
                [ T.roundedMd
                , T.border2
                , T.borderManzana
                , T.mt4
                , T.p4
                ]
            ]
            [ deleteListEl list ]
        ]

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
                        [ HH.text "Once you delete a list, all it's resources are deleted as wall and cannot be recovered." ]
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
                    ]
                ]
                [ HH.button
                    [ HP.classes
                        [ T.inlineFlex
                        , T.itemsCenter
                        , T.px4
                        , T.py2
                        , T.border
                        , T.borderTransparent
                        , T.fontMedium
                        , T.roundedMd
                        , T.textWhite
                        , T.bgManzana
                        , T.hoverBgOpacity75
                        , T.focusOutlineNone
                        , T.focusRing2
                        , T.focusRingOffset2
                        , T.focusRingManzana
                        , T.smTextSm
                        ]
                    , HP.type_ HP.ButtonButton
                    , HE.onClick \_ -> Just DeleteList
                    ]
                    [ HH.text "Delete list" ]
                ]
            ]
        ]
