-- | Listasio uses a REST API for resource management.
-- | This module defines endpoints in a data type
-- | which ensures invalid endpoints fail to compile.
module Listasio.Api.Endpoint where

import Prelude hiding ((/))

import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Listasio.Data.ID (ID)
import Listasio.Data.ID as ID
import Routing.Duplex (RouteDuplex', as, int, optional, root, segment)
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
  | List ID
  | Lists
  | Discover Pagination
  | Resources
  | ResourcesByList { list :: ID }
  | CompleteResource ID

derive instance genericEndpoint :: Generic Endpoint _

endpointCodec :: RouteDuplex' Endpoint
endpointCodec =
  root
    $ sum
        { "Login": "users" / "auth" / noArgs
        , "User": "user" / noArgs
        , "Users": "users" / noArgs
        , "List": "lists" / id segment
        , "Lists": "lists" / noArgs
        , "Discover": "discover" ?
            { skip: optional <<< int
            , limit: optional <<< int
            }
        , "Resources": "resources" / noArgs
        , "ResourcesByList": "resources" ? { list: id }
        , "CompleteResource": "resources" / id segment / "complete"
        }

id :: RouteDuplex' String -> RouteDuplex' ID
id = as ID.toString (ID.parse >>> note "Bad ID")
