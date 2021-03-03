module Listasio.Data.Integration where

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Listasio.Data.ID (ID)
import Listasio.Data.ID as ID

type IntegrationFields
  = { url :: String
    , list :: ID
    }

integrationFieldsCodec :: JsonCodec IntegrationFields
integrationFieldsCodec =
  CAR.object "IntegrationFields"
    { url: CA.string
    , list: ID.codec
    }
