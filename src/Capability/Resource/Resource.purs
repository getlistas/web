module Listasio.Capability.Resource.Resource where

import Prelude

import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Listasio.Data.Resource (ListResource, Resource)

class Monad m <= ManageResource m where
  getListResources :: String -> m (Maybe (Array ListResource))
  createResource :: Resource -> String -> m (Maybe ListResource)

instance manageResourceHalogenM :: ManageResource m => ManageResource (HalogenM st act slots msg m) where
  getListResources = lift <<< getListResources
  createResource resource id = lift $ createResource resource id
