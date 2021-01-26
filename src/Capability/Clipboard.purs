module Listasio.Capability.Clipboard where

import Prelude

import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)

class Monad m <= Clipboard m where
  writeText :: String -> m Boolean

instance clipboardHalogenM :: Clipboard m => Clipboard (HalogenM st act slots msg m) where
  writeText = lift <<< writeText
