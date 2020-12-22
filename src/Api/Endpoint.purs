-- | Listasio uses a REST API for resource management.
-- | This module defines endpoints in a data type
-- | which ensures invalid endpoints fail to compile.
module Listasio.Api.Endpoint where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Routing.Duplex (RouteDuplex', int, optional, root, segment, string)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))

type PaginationRep
  = ( limit :: Maybe Int
    , skip :: Maybe Int
    )

type Pagination
  = { | PaginationRep }

data Endpoint
  = Login
  | User
  | Users
  | List String -- TODO user ID newtype
  | Lists
  | Discover Pagination
  | ListResources String

derive instance genericEndpoint :: Generic Endpoint _

endpointCodec :: RouteDuplex' Endpoint
endpointCodec =
  root
    $ sum
        { "Login": "users" / "auth" / noArgs
        , "User": "user" / noArgs -- TODO: slug ?
        , "Users": "users" / noArgs
        , "List": "lists" / string segment
        , "Lists": "lists" / noArgs
        , "Discover": "discover" ?
            { skip: optional <<< int
            , limit: optional <<< int
            }
        , "ListResources": "lists" / string segment / "resources"
        }
