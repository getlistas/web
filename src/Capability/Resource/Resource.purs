module Listasio.Capability.Resource.Resource where

import Prelude

import Data.DateTime (DateTime)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Listasio.Data.ID (ID)
import Listasio.Data.Resource (ListResource, Resource)

-- TODO: where should this type be defined ?
type ListResources
  = { items :: Array ListResource
    , total :: Int
    , read :: Int
    , last_done :: Maybe DateTime
    }

class Monad m <= ManageResource m where
  getListResources :: ID -> m (Maybe ListResources)
  getResources :: m (Maybe (Array ListResource))
  createResource :: Resource -> m (Maybe ListResource)
  completeResource :: ListResource -> m (Maybe Unit)

instance manageResourceHalogenM :: ManageResource m => ManageResource (HalogenM st act slots msg m) where
  getListResources = lift <<< getListResources
  getResources = lift getResources
  createResource = lift <<< createResource
  completeResource = lift <<< completeResource
