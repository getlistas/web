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
import Halogen.HTML.Properties as HP
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Capability.Resource.List (class ManageList, getListBySlug, updateList)
import Listasio.Component.HTML.Layout as Layout
import Listasio.Component.HTML.ListForm as ListForm
import Listasio.Data.Lens (_list)
import Listasio.Data.List (CreateListFields, ListWithIdAndUser)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Route (Route)
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
          HH.slot F._formless unit ListForm.formComponent { list: Just list } $ Just <<< HandleListForm

        Failure msg -> HH.div [ HP.classes [ T.textManzana ] ] [ HH.text msg ]

        _ -> HH.text "Loading ..."
