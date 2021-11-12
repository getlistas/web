module Listasio.Foreign.Splitbee where

import Prelude

import Data.Argonaut.Core (Json)
import Effect (Effect)

foreign import init :: { scriptUrl :: String } -> Effect Unit

foreign import track :: String -> Effect Unit

foreign import trackWithData :: String -> Json -> Effect Unit

foreign import userSet :: Json -> Effect Unit
