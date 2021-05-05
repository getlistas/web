module Listasio.Capability.Resource.User where

import Prelude

import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Listasio.Api.Request (LoginFields, RegisterFields)
import Listasio.Data.Metrics (Metric)
import Listasio.Data.Profile (Profile, ProfileWithIdAndEmail, PublicProfile)
import Slug (Slug)

class Monad m <= ManageUser m where
  loginUser :: LoginFields -> m (Maybe ProfileWithIdAndEmail)
  googleLoginUser :: m (Maybe ProfileWithIdAndEmail)
  registerUser :: RegisterFields -> m (Maybe ProfileWithIdAndEmail)
  getCurrentUser :: m (Maybe ProfileWithIdAndEmail)
  updateUser :: Profile -> m Unit
  userBySlug :: Slug -> m (Maybe PublicProfile)
  userMetrics :: Slug -> m (Maybe (Array Metric))

instance manageUserHalogenM :: ManageUser m => ManageUser (HalogenM st act slots msg m) where
  loginUser = lift <<< loginUser
  googleLoginUser = lift googleLoginUser
  registerUser = lift <<< registerUser
  getCurrentUser = lift getCurrentUser
  updateUser = lift <<< updateUser
  userBySlug = lift <<< userBySlug
  userMetrics = lift <<< userMetrics
