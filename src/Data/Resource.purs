module Listasio.Data.Resource where

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe)

type ResourceRep row
  = ( url :: String
    , title :: String
    , description :: Maybe String
    | row
    )

type Resource
  = { | ResourceRep () }

type ID = { "$oid" :: String }
idCodec :: JsonCodec ID
idCodec = CAR.object "ID" { "$oid": CA.string }

-- created_at  :: DateTime<Utc>
-- updated_at  :: DateTime<Utc>
type ListResource
  = {
    | ResourceRep
      ( _id :: ID
      , list :: ID
      , user :: ID
      , completed_at :: Maybe String
      )
    }

resourceCodec :: JsonCodec Resource
resourceCodec =
  CAR.object "Resource"
    { url: CA.string
    , title: CA.string
    , description: CAC.maybe CA.string
    }

listResourceCodec :: JsonCodec ListResource
listResourceCodec =
  CAR.object "Resource"
    { _id: idCodec
    , list: idCodec
    , user: idCodec
    , url: CA.string
    , title: CA.string
    , description: CAC.maybe CA.string
    , completed_at: CAC.maybe CA.string
    }
