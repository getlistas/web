module Listasio.Component.HTML.EditResource where

import Prelude

import Data.Array (sortWith) as A
import Data.Either (note)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Monad (class MonadStore, updateStore)
import Listasio.Capability.Navigate (class Navigate)
import Listasio.Capability.Resource.List (class ManageList, getLists)
import Listasio.Capability.Resource.Resource (class ManageResource, updateResource)
import Listasio.Component.HTML.ResourceForm as ResourceForm
import Listasio.Data.List (ListWithIdUserAndMeta)
import Listasio.Data.Resource (Resource, ListResource)
import Listasio.Store as Store
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RemoteData
import Type.Proxy (Proxy(..))
import Util (filterNonAlphanum)

type Slot = forall query. H.Slot query Output Unit

_slot = Proxy :: Proxy "editResource"

data Action
  = HandleFormMessage Resource
  | LoadLists

type State
  =
  { lists :: Array ListWithIdUserAndMeta
  , resource :: ListResource
  }

type Input
  =
  { lists :: Array ListWithIdUserAndMeta
  , resource :: ListResource
  }

data Output
  = Updated { old :: ListResource, new :: ListResource }

type ChildSlots
  = (formless :: ResourceForm.Slot)

component
  :: forall query m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => ManageResource m
  => ManageList m
  => Navigate m
  => H.Component query Input Output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      }
  }
  where
  initialState { lists, resource } =
    { lists: A.sortWith (filterNonAlphanum <<< _.title) lists
    , resource
    }

  handleAction :: Action -> H.HalogenM State Action ChildSlots Output m Unit
  handleAction = case _ of
    HandleFormMessage updated -> do
      { resource: saved } <- H.get
      void $ H.query F._formless unit $ F.injQuery $ ResourceForm.SetCreateStatus Loading unit

      mbUpdated <- updateResource saved.id updated

      case mbUpdated of
        Just resource -> do
          H.raise $ Updated { old: saved, new: resource }
          void $ H.query F._formless unit $ F.injQuery $ ResourceForm.SetCreateStatus (Success resource) unit
          handleAction LoadLists

        Nothing ->
          void $ H.query F._formless unit $ F.injQuery $ ResourceForm.SetCreateStatus (Failure "Could not create resource") unit

    LoadLists -> do
      result <- RemoteData.fromEither <$> note "Could not fetch your lists" <$> getLists
      updateStore $ Store.SetLists result

  render :: State -> HH.ComponentHTML Action ChildSlots m
  render { lists, resource } =
    HH.div
      []
      [ HH.slot F._formless unit ResourceForm.formComponent formInput HandleFormMessage ]
    where
    formInput = { lists, selectedList: Just resource.list, initialInput }
    initialInput = ResourceForm.InputToEdit resource

