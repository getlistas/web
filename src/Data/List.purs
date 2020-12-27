module Listasio.Data.List where

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe)

type ListRep row
  = ( title :: String -- TODO should be NonEmptyString ?
    , description :: Maybe String
    , tags :: Array String
    , is_public :: Boolean
    | row
    )

type List
  = { | ListRep () }

-- TODO to string eventually
type ID
  = { "$oid" :: String }

-- TODO should be NonEmptyString or newtype ID ?
-- TODO:
--   created_at  :: DateTime<Utc>
--   updated_at  :: DateTime<Utc>
type ListWithIdAndUser
  = { | ListRep ( _id :: ID, user :: ID ) }

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
    { _id: CAR.object "ID" { "$oid": CA.string }
    , title: CA.string
    , description: CAC.maybe CA.string
    , tags: CAC.array CA.string
    , user: CAR.object "ID" { "$oid": CA.string }
    , is_public: CA.boolean
    }
