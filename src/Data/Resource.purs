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

type MongoID = { "$oid" :: String }
idCodec :: JsonCodec MongoID
idCodec = CAR.object "MongoID" { "$oid": CA.string }

type MongoDate = { "$date" :: String }
mongoDateCodec :: JsonCodec MongoDate
mongoDateCodec = CAR.object "MongoDate" { "$date": CA.string }

-- created_at  :: DateTime<Utc>
-- updated_at  :: DateTime<Utc>
type ListResource
  = {
    | ResourceRep
      ( _id :: MongoID
      , list :: MongoID
      , user :: MongoID
      , completed_at :: Maybe MongoDate
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
    , completed_at: CAC.maybe mongoDateCodec
    }
