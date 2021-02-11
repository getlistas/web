module Listasio.Data.List where

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.DateTime (DateTime)
import Data.Maybe (Maybe)
import Listasio.Data.DateTime as DateTime
import Listasio.Data.ID (ID)
import Listasio.Data.ID as ID
import Listasio.Data.Resource (ListResource, listResourceCodec)

type ResourceMeta
  = { count :: Int
    , completed_count :: Int
    , next :: Maybe ListResource
    }

type ListRep row
  = ( title :: String
    , description :: Maybe String
    , tags :: Array String
    , is_public :: Boolean
    | row
    )

type List
  = { | ListRep () }

type ListWithIdAndUser
  = {
    | ListRep
      ( id :: ID
      , user :: ID
      , created_at :: DateTime
      , updated_at :: DateTime
      )
    }

type ListWithIdUserAndMeta
  = {
    | ListRep
      ( id :: ID
      , user :: ID
      , created_at :: DateTime
      , updated_at :: DateTime
      , resource_metadata :: ResourceMeta
      )
    }

resourceMetaCodec :: JsonCodec ResourceMeta
resourceMetaCodec =
  CAR.object "ResourceMeta"
    { count: CA.int
    , completed_count: CA.int
    , next: CAC.maybe listResourceCodec
    }

listCodec :: JsonCodec List
listCodec =
  CAR.object "List"
    { title: CA.string
    , description: CAC.maybe CA.string
    , tags: CAC.array CA.string
    , is_public: CA.boolean
    }

listWitIdAndUserCodec :: JsonCodec ListWithIdAndUser
listWitIdAndUserCodec =
  CAR.object "List"
    { id: ID.codec
    , title: CA.string
    , description: CAC.maybe CA.string
    , tags: CAC.array CA.string
    , user: ID.codec
    , is_public: CA.boolean
    , created_at: DateTime.codec
    , updated_at: DateTime.codec
    }

listWitIdUserAndMetaCodec :: JsonCodec ListWithIdUserAndMeta
listWitIdUserAndMetaCodec =
  CAR.object "List"
    { id: ID.codec
    , title: CA.string
    , description: CAC.maybe CA.string
    , tags: CAC.array CA.string
    , user: ID.codec
    , is_public: CA.boolean
    , created_at: DateTime.codec
    , updated_at: DateTime.codec
    , resource_metadata: resourceMetaCodec
    }
