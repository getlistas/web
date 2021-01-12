module Listasio.Capability.Resource.Resource where

import Prelude

import Data.DateTime (DateTime)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Listasio.Data.ID (ID)
import Listasio.Data.Resource (ListResource, Resource)
import Listasio.Data.ResourceMetadata (ResourceMeta)

-- TODO: where should this type be defined ?
type ListResources
  = { items :: Array ListResource
    , total :: Int
    , read :: Int
    , last_done :: Maybe DateTime
    }

class Monad m <= ManageResource m where
  getMeta :: String -> m (Maybe ResourceMeta)
  getListResources :: ID -> m (Maybe ListResources)
  getResources :: m (Maybe (Array ListResource))
  createResource :: Resource -> m (Maybe ListResource)
  completeResource :: ListResource -> m (Maybe Unit)
  deleteResource :: ListResource -> m (Maybe Unit)

instance manageResourceHalogenM :: ManageResource m => ManageResource (HalogenM st act slots msg m) where
  getMeta = lift <<< getMeta
  getListResources = lift <<< getListResources
  getResources = lift getResources
  createResource = lift <<< createResource
  completeResource = lift <<< completeResource
  deleteResource = lift <<< deleteResource
