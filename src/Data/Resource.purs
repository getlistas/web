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

-- TODO to string
type ID = { "$oid" :: String }
idCodec :: JsonCodec ID
idCodec = CAR.object "ID" { "$oid": CA.string }

-- TODO:
--   created_at  :: DateTime<Utc>
--   updated_at  :: DateTime<Utc>
type ListResource
  = {
    | ResourceRep
      ( list :: ID -- TODO Newtype
      , _id :: ID -- TODO Newtype
      , user :: ID -- TODO Newtype
      , completed_at :: Maybe String -- TODO Date type
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
