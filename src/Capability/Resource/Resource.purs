module Listasio.Capability.Resource.Resource where

import Prelude

import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Listasio.Api.Endpoint (SearchResourcesArgs)
import Listasio.Data.ID (ID)
import Listasio.Data.Resource (ListResource, Resource, ImportResourcesBody)
import Listasio.Data.ResourceMetadata (ResourceMeta)
import Slug (Slug)

type PositionChangeBody = { previus :: Maybe ID }

type PublicResourcesArgs
  = { user :: Slug, list :: Slug }

class Monad m <= ManageResource m where
  getMeta :: String -> m (Maybe ResourceMeta)
  getResources :: m (Maybe (Array ListResource))
  getListResources :: { list :: ID, completed :: Maybe Boolean } -> m (Maybe (Array ListResource))
  getPublicListResources :: PublicResourcesArgs -> m (Maybe (Array ListResource))
  searchResources :: SearchResourcesArgs -> m (Maybe (Array ListResource))
  createResource :: Resource -> m (Maybe ListResource)
  updateResource :: ID -> Resource -> m (Maybe ListResource)
  completeResource :: ListResource -> m (Maybe Unit)
  uncompleteResource :: ListResource -> m (Maybe Unit)
  deleteResource :: ListResource -> m (Maybe Unit)
  changePosition :: ListResource -> PositionChangeBody -> m (Maybe Unit)
  importResources :: ImportResourcesBody -> m (Maybe Unit)

instance manageResourceHalogenM :: ManageResource m => ManageResource (HalogenM st act slots msg m) where
  getMeta = lift <<< getMeta
  getResources = lift getResources
  getListResources = lift <<< getListResources
  getPublicListResources = lift <<< getPublicListResources
  searchResources = lift <<< searchResources
  createResource = lift <<< createResource
  updateResource id = lift <<< updateResource id
  completeResource = lift <<< completeResource
  uncompleteResource = lift <<< uncompleteResource
  deleteResource = lift <<< deleteResource
  changePosition id = lift <<< changePosition id
  importResources = lift <<< importResources
