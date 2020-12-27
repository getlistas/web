module Listasio.Capability.Resource.User where

import Prelude

import Listasio.Api.Request (LoginFields, RegisterFields)
import Listasio.Data.Profile (Profile)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)

class Monad m <= ManageUser m where
  loginUser :: LoginFields -> m (Maybe Profile)
  registerUser :: RegisterFields -> m (Maybe Profile)
  getCurrentUser :: m (Maybe Profile)
  updateUser :: Profile -> m Unit

instance manageUserHalogenM :: ManageUser m => ManageUser (HalogenM st act slots msg m) where
  loginUser = lift <<< loginUser
  registerUser = lift <<< registerUser
  getCurrentUser = lift getCurrentUser
  updateUser = lift <<< updateUser
