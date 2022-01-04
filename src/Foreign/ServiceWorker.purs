module Listasio.Foreign.ServiceWorker where

import Control.Promise (Promise)
import Control.Promise as Promise
import Effect (Effect)
import Effect.Aff (Aff)

foreign import register_ :: Effect (Promise Boolean)

register :: Aff Boolean
register = Promise.toAffE register_
