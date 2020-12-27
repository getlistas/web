module Listasio.Capability.Navigate where

import Prelude

import Control.Monad.Trans.Class (lift)
import Listasio.Data.Route (Route)
import Halogen (HalogenM)
import Routing.PushState (LocationState)
import Web.Event.Event (Event)

class Monad m <= Navigate m where
  navigate :: Route -> m Unit
  navigate_ :: Event -> Route -> m Unit
  logout :: m Unit
  locationState :: m LocationState

instance navigateHalogenM :: Navigate m => Navigate (HalogenM st act slots msg m) where
  navigate = lift <<< navigate
  navigate_ e r = lift $ navigate_ e r
  logout = lift logout
  locationState = lift locationState
