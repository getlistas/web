module Listasio.Data.ResourceMetadata where

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe)
import Listasio.Data.Resource (ListResource, listResourceCodec)

type ResourceMeta
  =
  { can_resolve :: Boolean
  , title :: Maybe String
  , description :: Maybe String
  , thumbnail :: Maybe String
  , resource :: Maybe ListResource
  }

metaCodec :: JsonCodec ResourceMeta
metaCodec =
  CAR.object "Resource"
    { can_resolve: CA.boolean
    , title: CAC.maybe CA.string
    , description: CAC.maybe CA.string
    , thumbnail: CAC.maybe CA.string
    , resource: CAC.maybe listResourceCodec
    }
