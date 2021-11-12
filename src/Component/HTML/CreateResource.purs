module Listasio.Component.HTML.CreateResource where

import Prelude

import Data.Array (filter, sortWith) as A
import Data.CodePoint.Unicode (isAlphaNum)
import Data.Either (note)
import Data.Maybe (Maybe(..))
import Data.String (fromCodePointArray, toCodePointArray) as String
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Monad (class MonadStore, updateStore)
import Listasio.Capability.Navigate (class Navigate)
import Listasio.Capability.Resource.List (class ManageList, getLists)
import Listasio.Capability.Resource.Resource (class ManageResource, createResource)
import Listasio.Component.HTML.ResourceForm as ResourceForm
import Listasio.Data.ID (ID)
import Listasio.Data.List (ListWithIdUserAndMeta)
import Listasio.Data.Resource (Resource, ListResource)
import Listasio.Store as Store
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RemoteData
import Type.Proxy (Proxy(..))

type Slot = forall query. H.Slot query Output Unit

_slot = Proxy :: Proxy "createResource"

data Action
  = HandleFormMessage Resource
  | LoadLists

type State
  =
  { lists :: Array ListWithIdUserAndMeta
  , selectedList :: Maybe ID
  , url :: Maybe String
  , title :: Maybe String
  , text :: Maybe String
  }

type Input
  =
  { lists :: Array ListWithIdUserAndMeta
  , selectedList :: Maybe ID
  , url :: Maybe String
  , title :: Maybe String
  , text :: Maybe String
  }

data Output
  = Created ListResource

type ChildSlots
  = (formless :: ResourceForm.Slot)

filterNonAlphanum :: String -> String
filterNonAlphanum =
  String.fromCodePointArray <<< A.filter isAlphaNum <<< String.toCodePointArray

component
  :: forall query m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => ManageList m
  => ManageResource m
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
  initialState { lists, selectedList, url, title, text } =
    { lists: A.sortWith (filterNonAlphanum <<< _.title) lists
    , selectedList
    , url
    , title
    , text
    }

  handleAction :: Action -> H.HalogenM State Action ChildSlots Output m Unit
  handleAction = case _ of
    HandleFormMessage newResource -> do
      void $ H.query F._formless unit $ F.injQuery $ ResourceForm.SetCreateStatus Loading unit

      mbNewResource <- createResource newResource

      case mbNewResource of
        Just resource -> do
          H.raise $ Created resource
          void $ H.query F._formless unit $ F.injQuery $ ResourceForm.SetCreateStatus (Success resource) unit
          handleAction LoadLists

        Nothing ->
          void $ H.query F._formless unit $ F.injQuery $ ResourceForm.SetCreateStatus (Failure "Could not create resource") unit

    LoadLists -> do
      result <- RemoteData.fromEither <$> note "Could not fetch your lists" <$> getLists
      updateStore $ Store.SetLists result

  render :: State -> HH.ComponentHTML Action ChildSlots m
  render { lists, selectedList, url, title, text } =
    HH.div
      []
      [ HH.slot F._formless unit ResourceForm.formComponent formInput HandleFormMessage ]
    where
    formInput = { lists, selectedList, initialInput }
    initialInput = ResourceForm.InputToCreate { url, title, text }

