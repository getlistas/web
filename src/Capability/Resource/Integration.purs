module Listasio.Capability.Resource.Integration where

import Prelude

import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Listasio.Data.ID (ID)
import Listasio.Data.Integration (IntegrationFields)

class Monad m <= ManageIntegration m where
  createIntegration :: IntegrationFields -> m (Maybe Unit)
  deleteIntegration :: ID -> m (Maybe Unit)

instance manageIntegrationHalogenM :: ManageIntegration m => ManageIntegration (HalogenM st act slots msg m) where
  createIntegration = lift <<< createIntegration
  deleteIntegration = lift <<< deleteIntegration
