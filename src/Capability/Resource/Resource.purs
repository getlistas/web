module Listasio.Capability.Resource.Resource where

import Prelude

import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Listasio.Data.ID (ID)
import Listasio.Data.Resource (ListResource, Resource)
import Listasio.Data.ResourceMetadata (ResourceMeta)
import Slug (Slug)

type PositionChangeBody = { previus :: Maybe ID }

class Monad m <= ManageResource m where
  getMeta :: String -> m (Maybe ResourceMeta)
  getResources :: m (Maybe (Array ListResource))
  getListResources :: { list :: ID, completed :: Maybe Boolean } -> m (Maybe (Array ListResource))
  getPublicListResources :: {user :: Slug, list :: Slug} -> m (Maybe (Array ListResource))
  createResource :: Resource -> m (Maybe ListResource)
  completeResource :: ListResource -> m (Maybe Unit)
  deleteResource :: ListResource -> m (Maybe Unit)
  changePosition :: ListResource -> PositionChangeBody -> m (Maybe Unit)

instance manageResourceHalogenM :: ManageResource m => ManageResource (HalogenM st act slots msg m) where
  getMeta = lift <<< getMeta
  getResources = lift getResources
  getListResources = lift <<< getListResources
  getPublicListResources = lift <<< getPublicListResources
  createResource = lift <<< createResource
  completeResource = lift <<< completeResource
  deleteResource = lift <<< deleteResource
  changePosition id body = lift $ changePosition id body
