module Listasio.Capability.Resource.List where

import Prelude

import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Listasio.Api.Endpoint (Pagination)
import Listasio.Data.ID (ID)
import Listasio.Data.List (CreateListFields, ListWithIdAndUser, ListWithIdUserAndMeta, PublicList)
import Slug (Slug)

class Monad m <= ManageList m where
  createList :: CreateListFields -> m (Maybe ListWithIdAndUser)
  getList :: ID -> m (Maybe ListWithIdUserAndMeta)

  -- TODO: these two endpoints should return a different type (ie. the private
  --       list has information that the public one should not). But is not yet
  --       supported by the backend
  getListBySlug :: { user :: Slug, list :: Slug } -> m (Maybe ListWithIdAndUser)
  getPublicListBySlug :: { user :: Slug, list :: Slug } -> m (Maybe ListWithIdUserAndMeta)

  getLists :: m (Maybe (Array ListWithIdUserAndMeta))
  updateList :: ID -> CreateListFields -> m (Maybe ListWithIdAndUser)
  deleteList :: ID -> m Unit
  discoverLists :: Pagination -> m (Maybe (Array PublicList))
  forkList :: ID -> m (Maybe ListWithIdAndUser)

instance manageListHalogenM :: ManageList m => ManageList (HalogenM st act slots msg m) where
  createList = lift <<< createList
  getList = lift <<< getList
  getListBySlug = lift <<< getListBySlug
  getPublicListBySlug = lift <<< getPublicListBySlug
  getLists = lift getLists
  updateList id list = lift $ updateList id list
  deleteList = lift <<< deleteList
  discoverLists = lift <<< discoverLists
  forkList = lift <<< forkList
