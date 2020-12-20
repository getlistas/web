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

-- TODO to string eventually
type ID
  = { "$oid" :: String }

type ListResource
  = {
    | ResourceRep
      ( list :: ID -- TODO should be NonEmptyString or newtype ID ?
      , _id :: ID -- TODO should be NonEmptyString ?
      , user :: ID -- TODO should be some newtype ID ?
      , completed_at :: Maybe String -- TODO:: DateTime<Utc>
      -- TODO
      -- created_at  :: DateTime<Utc>
      -- updated_at  :: DateTime<Utc>
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
    { _id: CAR.object "ID" { "$oid": CA.string }
    , list: CAR.object "ID" { "$oid": CA.string }
    , user: CAR.object "ID" { "$oid": CA.string }
    , url: CA.string
    , title: CA.string
    , description: CAC.maybe CA.string
    , completed_at: CAC.maybe CA.string
    }
