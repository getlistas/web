module Listasio.Capability.Analytics where

import Prelude

import Control.Monad.Cont.Trans (lift)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Maybe (Maybe)
import Halogen (HalogenM)

class Monad m <= Analytics m where
  init :: m Unit
  userSet :: forall a. EncodeJson a => a -> m Unit
  track :: forall a. EncodeJson a => String -> Maybe a -> m Unit

instance analyticsHalogenM :: Analytics m => Analytics (HalogenM st act slog msg m) where
  init = lift init
  userSet = lift <<< userSet
  track eventName data_ = lift $ track eventName data_
