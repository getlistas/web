module Listasio.Data.Resource where

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.DateTime (DateTime)
import Data.Maybe (Maybe)
import Listasio.Data.DateTime as DateTime
import Listasio.Data.ID (ID)
import Listasio.Data.ID as ID

type ResourceRep row
  = ( url :: String
    , title :: String
    , list :: ID
    , description :: Maybe String
    | row
    )

type Resource
  = { | ResourceRep () }

type ListResource
  = {
    | ResourceRep
      ( id :: ID
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
    , list: ID.codec
    }

listResourceCodec :: JsonCodec ListResource
listResourceCodec =
  CAR.object "Resource"
    { id: ID.codec
    , list: ID.codec
    , user: CA.string
    , url: CA.string
    , title: CA.string
    , description: CAC.maybe CA.string
    , created_at: DateTime.codec
    , updated_at: DateTime.codec
    , completed_at: CAC.maybe DateTime.codec
    }
