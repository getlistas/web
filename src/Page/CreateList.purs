module Listasio.Page.CreateList where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Listasio.Capability.Navigate (class Navigate, navigate, navigate_)
import Listasio.Capability.Resource.List (class ManageList, createList)
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.ListForm as ListForm
import Listasio.Component.HTML.Utils (safeHref)
import Listasio.Data.List (CreateListFields)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Route (Route(..))
import Listasio.Store as Store
import Network.RemoteData (RemoteData(..))
import Tailwind as T
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent (toEvent)

_slot :: Proxy "createList"
_slot = Proxy

data Action
  = Receive (Connected (Maybe ProfileWithIdAndEmail) Unit)
  | HandleCreateForm CreateListFields
  | Navigate Route Event.Event

type State = {currentUser :: Maybe ProfileWithIdAndEmail}

type ChildSlots
  = ( formless :: ListForm.Slot )

component
  :: forall q o m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => ManageList m
  => Navigate m
  => H.Component q Unit o m
component = connect (selectEq _.currentUser) $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }
  where
  initialState {context: currentUser} = {currentUser}

  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Receive {context: currentUser} ->
      H.modify_ _ {currentUser = currentUser}

    Navigate route e -> navigate_ e route

    HandleCreateForm newList -> do
      void $ H.query F._formless unit $ F.injQuery $ ListForm.SetCreateStatus Loading unit

      mbCreatedList <- createList newList

      case mbCreatedList of
        Just createdList -> do
           void $ H.query F._formless unit $ F.injQuery $ ListForm.SetCreateStatus (Success createdList) unit
           navigate Dashboard
        Nothing ->
          void $ H.query F._formless unit $ F.injQuery $ ListForm.SetCreateStatus (Failure "Could not create list") unit

  render :: State -> H.ComponentHTML Action ChildSlots m
  render _ =
    HH.div
      []
      [ title
      , HH.div
          [ HP.classes [ T.grid, T.gridCols1, T.mdGridCols2 ] ]
          [ HH.slot F._formless unit ListForm.formComponent {list: Nothing} HandleCreateForm
          ]
      ]

    where
    title =
      HH.div
        [ HP.classes [ T.flex, T.itemsCenter, T.textGray400, T.mb6, T.text4xl, T.fontBold, T.pt2  ] ]
        [ HH.a
            [ safeHref Dashboard
            , HE.onClick $ Navigate Dashboard <<< toEvent
            , HP.classes [ T.textGray200, T.mr8, T.flexShrink0 ]
            ]
            [ Icons.chevronLeft [ Icons.classes [ T.h10, T.w10 ] ] ]
        , HH.text "Create List"
        ]
