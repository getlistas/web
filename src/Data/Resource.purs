module Listasio.Data.Resource where

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.DateTime (DateTime)
import Data.Maybe (Maybe)
import Listasio.Data.DateTime as DateTime

type ResourceRep row
  = ( url :: String
    , title :: String
    , list :: String
    , description :: Maybe String
    | row
    )

type Resource
  = { | ResourceRep () }

type ListResource
  = {
    | ResourceRep
      ( id :: String
      , user :: String
      , created_at :: DateTime
      , updated_at :: DateTime
      , completed_at :: Maybe DateTime
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
    , created_at: DateTime.codec
    , updated_at: DateTime.codec
    , completed_at: CAC.maybe DateTime.codec
    }
