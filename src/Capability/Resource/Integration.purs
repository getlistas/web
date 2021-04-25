module Listasio.Capability.Resource.Integration where

import Prelude

import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Listasio.Data.ID (ID)
import Listasio.Data.Integration (Integration, ListSubscription, ListSubscriptionFields, RssIntegration, RssIntegrationFields)

class Monad m <= ManageIntegration m where
  createRssIntegration :: RssIntegrationFields -> m (Maybe RssIntegration)
  subscribeToList :: ListSubscriptionFields -> m (Maybe ListSubscription)
  deleteIntegration :: ID -> m (Maybe Unit)
  getListIntegrations :: ID -> m (Maybe (Array Integration))

instance manageIntegrationHalogenM :: ManageIntegration m => ManageIntegration (HalogenM st act slots msg m) where
  createRssIntegration = lift <<< createRssIntegration
  deleteIntegration = lift <<< deleteIntegration
  getListIntegrations = lift <<< getListIntegrations
  subscribeToList = lift <<< subscribeToList
