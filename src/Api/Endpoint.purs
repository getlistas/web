-- | Doneq uses a REST API for resource management.
-- | This module defines endpoints in a data type
-- | which ensures invalid endpoints fail to compile.
module Doneq.Api.Endpoint where

import Prelude hiding ((/))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Routing.Duplex (RouteDuplex', root)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

type PaginationRep
  = ( limit :: Maybe Int
    , offset :: Maybe Int
    )

type Pagination
  = { | PaginationRep }

data Endpoint
  = Login
  | User
  | Users

derive instance genericEndpoint :: Generic Endpoint _

endpointCodec :: RouteDuplex' Endpoint
endpointCodec =
  root
    $ sum
        { "Login": "users" / "auth" / noArgs
        , "User": "user" / noArgs -- TODO: slug ?
        , "Users": "users" / noArgs
        }
