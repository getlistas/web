-- | Listasio uses a REST API for resource management.
-- | This module defines endpoints in a data type
-- | which ensures invalid endpoints fail to compile.
module Listasio.Api.Endpoint where

import Prelude hiding ((/))

import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Listasio.Data.ID (ID)
import Listasio.Data.ID as ID
import Listasio.Data.Integration (IntegrationKind(..))
import Listasio.Data.Route (slug)
import Routing.Duplex (RouteDuplex', as, boolean, int, optional, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))
import Slug (Slug)

type PaginationRep
  = ( limit :: Maybe Int
    , skip :: Maybe Int
    )

type Pagination
  = { | PaginationRep }

data SortingResources
  = PositionAsc
  | PositionDes
  | DateAsc
  | DateDes

data Endpoint
  = Login
  | GoogleLogin
  | User
  | Users
  | List ID
  | ListBySlug Slug Slug
  | ListFork ID
  | Lists
  | Discover Pagination
  | Resources
  | ResourcesByList { list :: ID, sort :: SortingResources, completed :: Boolean }
  | Resource ID
  | CompleteResource ID
  | ResourceMeta
  | Integrations { list :: ID, kind :: IntegrationKind }
  | RssIntegrations
  | RssIntegration ID

derive instance genericEndpoint :: Generic Endpoint _

endpointCodec :: RouteDuplex' Endpoint
endpointCodec =
  root
    $ sum
        { "Login": "users" / "auth" / noArgs
        , "GoogleLogin": "users" / "google-auth" / noArgs
        , "User": "user" / noArgs
        , "Users": "users" / noArgs
        , "List": "lists" / id segment
        , "ListBySlug": "users" / slug segment / "lists" / slug segment
        , "ListFork": "lists" / id segment / "fork"
        , "Lists": "lists" / noArgs
        , "Discover": "discover" ?
            { skip: optional <<< int
            , limit: optional <<< int
            }
        , "Resources": "resources" / noArgs
        , "ResourcesByList": "resources"
            ? { list: id, sort: sortingResources, completed: boolean }
        , "Resource": "resources" / id segment
        , "CompleteResource": "resources" / id segment / "complete"
        , "ResourceMeta": "resource-metadata" / noArgs
        , "Integrations": "integrations" ? { list: id, kind: integrationKind }
        , "RssIntegrations": "integrations" / "rss" / noArgs
        , "RssIntegration": "integrations" / "rss" / id segment
        }

id :: RouteDuplex' String -> RouteDuplex' ID
id = as ID.toString (ID.parse >>> note "Bad ID")

integrationKind :: RouteDuplex' String -> RouteDuplex' IntegrationKind
integrationKind = as toString parse
  where
  toString KindRss = "rss"

  parse = case _ of
    "rss" -> Right KindRss
    s -> Left $ "Bad IntegrationKind '" <> s <> "'"


sortingResources :: RouteDuplex' String -> RouteDuplex' SortingResources
sortingResources = as toString parse
  where
  toString PositionAsc = "position_asc"
  toString PositionDes = "position_des"
  toString DateAsc = "date_asc"
  toString DateDes = "date_des"

  parse = case _ of
    "position_asc" -> Right PositionAsc
    "position_des" -> Right PositionDes
    "date_asc" -> Right DateAsc
    "date_des" -> Right DateDes
    s -> Left $ "Bad SortingResources '" <> s <> "'"

