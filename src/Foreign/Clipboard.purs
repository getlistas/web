module Listasio.Foreign.Clipboard where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Effect (Effect)
import Effect.Aff (Aff)

foreign import writeText_ :: String -> Effect (Promise Boolean)

writeText :: String -> Aff Boolean
writeText = Promise.toAffE <<< writeText_
