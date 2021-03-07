module Listasio.Capability.Resource.Integration where

import Prelude

import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Listasio.Data.ID (ID)
import Listasio.Data.Integration (RssIntegrationFields, RssIntegration)

class Monad m <= ManageIntegration m where
  createRssIntegration :: RssIntegrationFields -> m (Maybe RssIntegration)
  deleteRssIntegration :: ID -> m (Maybe Unit)
  getListIntegrations :: ID -> m (Maybe (Array RssIntegration))

instance manageIntegrationHalogenM :: ManageIntegration m => ManageIntegration (HalogenM st act slots msg m) where
  createRssIntegration = lift <<< createRssIntegration
  deleteRssIntegration = lift <<< deleteRssIntegration
  getListIntegrations = lift <<< getListIntegrations
