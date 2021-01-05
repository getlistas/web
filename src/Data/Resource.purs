module Listasio.Data.Resource where

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe)

type ResourceRep row
  = ( url :: String
    , title :: String
    , list :: String
    , description :: Maybe String
    | row
    )

type Resource
  = { | ResourceRep () }

-- created_at  :: DateTime<Utc>
-- updated_at  :: DateTime<Utc>
type ListResource
  = {
    | ResourceRep
      ( id :: String
      , user :: String
      , completed_at :: Maybe String
      )
    }

resourceCodec :: JsonCodec Resource
resourceCodec =
  CAR.object "Resource"
    { url: CA.string
    , title: CA.string
    , description: CAC.maybe CA.string
    , list: CA.string
    }

listResourceCodec :: JsonCodec ListResource
listResourceCodec =
  CAR.object "Resource"
    { id: CA.string
    , list: CA.string
    , user: CA.string
    , url: CA.string
    , title: CA.string
    , description: CAC.maybe CA.string
    , completed_at: CAC.maybe CA.string
    }
