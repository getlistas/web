module Listasio.Capability.Resource.List where

import Prelude

import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Listasio.Api.Endpoint (Pagination)
import Listasio.Data.ID (ID)
import Listasio.Data.List (CreateListFields, ListWithIdAndUser, ListWithIdUserAndMeta)

class Monad m <= ManageList m where
  createList :: CreateListFields -> m (Maybe ListWithIdAndUser)
  getList :: ID -> m (Maybe ListWithIdUserAndMeta)
  getLists :: m (Maybe (Array ListWithIdUserAndMeta))
  deleteList :: ID -> m Unit
  discoverLists :: Pagination -> m (Maybe (Array ListWithIdAndUser))
  forkList :: ID -> m (Maybe ListWithIdAndUser)

instance manageListHalogenM :: ManageList m => ManageList (HalogenM st act slots msg m) where
  createList = lift <<< createList
  getList = lift <<< getList
  getLists = lift getLists
  deleteList = lift <<< deleteList
  discoverLists = lift <<< discoverLists
  forkList = lift <<< forkList
