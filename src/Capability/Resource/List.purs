module Listasio.Capability.Resource.List where

import Prelude

import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Listasio.Api.Endpoint (Pagination)
import Listasio.Data.ID (ID)
import Listasio.Data.List (CreateListFields, ListWithIdAndUser, ListWithIdUserAndMeta)
import Slug (Slug)

class Monad m <= ManageList m where
  createList :: CreateListFields -> m (Maybe ListWithIdAndUser)
  getList :: ID -> m (Maybe ListWithIdUserAndMeta)
  getListBySlug :: { user :: Slug, list :: Slug } -> m (Maybe ListWithIdAndUser)
  getLists :: m (Maybe (Array ListWithIdUserAndMeta))
  updateList :: ID -> CreateListFields -> m (Maybe ListWithIdAndUser)
  deleteList :: ID -> m Unit
  discoverLists :: Pagination -> m (Maybe (Array ListWithIdAndUser))
  forkList :: ID -> m (Maybe ListWithIdAndUser)

instance manageListHalogenM :: ManageList m => ManageList (HalogenM st act slots msg m) where
  createList = lift <<< createList
  getList = lift <<< getList
  getListBySlug = lift <<< getListBySlug
  getLists = lift getLists
  updateList id list = lift $ updateList id list
  deleteList = lift <<< deleteList
  discoverLists = lift <<< discoverLists
  forkList = lift <<< forkList
