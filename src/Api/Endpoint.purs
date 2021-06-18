-- | Listasio uses a REST API for resource management.
-- | This module defines endpoints in a data type
-- | which ensures invalid endpoints fail to compile.
module Listasio.Api.Endpoint where

import Prelude hiding ((/))

import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Listasio.Data.ID (ID)
import Listasio.Data.ID as ID
import Listasio.Data.Integration (IntegrationKind(..))
import Routing.Duplex (RouteDuplex', as, boolean, int, optional, root, segment, string)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))
import Slug (Slug)
import Slug as Slug

type PaginationRep row
  = ( limit :: Maybe Int
    , skip :: Maybe Int
    | row
    )

type Pagination
  = {| PaginationRep ()}

data SortingResources = PositionAsc | PositionDes

type SearchResourcesArgs
  = {
    | PaginationRep
      ( list :: Maybe ID
      , completed :: Maybe Boolean
      , sort :: Maybe SortingResources
      , search_text :: Maybe String
      )
    }

defaultSearch :: SearchResourcesArgs
defaultSearch =
  { skip: Nothing
  , limit: Nothing
  , list: Nothing
  , completed: Nothing
  , sort: Nothing
  , search_text: Nothing
  }

type ResourcesByListArgs
  = { list :: ID
    , sort :: SortingResources
    , completed :: Maybe Boolean
    }

type IntegrationsArgs
  = { list :: ID
    , kind :: Maybe IntegrationKind
    }

data Endpoint
  = Login
  | GoogleLogin
  | User
  | Me
  | Users
  | UserBySlug Slug
  | UserMetrics Slug
  | List ID
  | ListBySlug Slug Slug
  | ListResourcesBySlug Slug Slug
  | ListFork ID
  | Lists
  | Discover Pagination
  | Resources
  | ResourcesByList ResourcesByListArgs
  | SearchResources SearchResourcesArgs
  | Resource ID
  | CompleteResource ID
  | UncompleteResource ID
  | PositionResource ID
  | ResourceMeta
  | Integration ID
  | Integrations IntegrationsArgs
  | RssIntegrations
  | ListSubscriptionIntegrations

derive instance genericEndpoint :: Generic Endpoint _

endpointCodec :: RouteDuplex' Endpoint
endpointCodec =
  root
    $ sum
        { "Login": "users" / "auth" / noArgs
        , "GoogleLogin": "users" / "google-auth" / noArgs
        , "User": "user" / noArgs
        , "Me": "users" / "me" / noArgs
        , "Users": "users" / noArgs
        , "UserBySlug": "users" / Slug.term segment
        , "UserMetrics": "users" / Slug.term segment / "metrics"
        , "List": "lists" / id segment
        , "ListBySlug": "users" / Slug.term segment / "lists" / Slug.term segment
        , "ListResourcesBySlug": "users" / Slug.term segment / "lists" / Slug.term segment / "resources"
        , "ListFork": "lists" / id segment / "fork"
        , "Lists": "lists" / noArgs
        , "Discover": "discover" ?
            { skip: optional <<< int
            , limit: optional <<< int
            }
        , "Resources": "resources" / noArgs
        , "ResourcesByList": "resources"
            ? {list: id, sort: sortingResources, completed: optional <<< boolean}
        , "SearchResources": "resources"
            ? { list: optional <<< id
              , completed: optional <<< boolean
              , sort: optional <<< sortingResources
              , search_text: optional <<< string
              , skip: optional <<< int
              , limit: optional <<< int
              }
        , "Resource": "resources" / id segment
        , "CompleteResource": "resources" / id segment / "complete"
        , "UncompleteResource": "resources" / id segment / "undo-complete"
        , "PositionResource": "resources" / id segment / "position"
        , "ResourceMeta": "resource-metadata" / noArgs
        , "Integration": "integrations" / id segment
        , "Integrations": "integrations"
            ? {list: id, kind: optional <<< integrationKind}
        , "RssIntegrations": "integrations" / "rss" / noArgs
        , "ListSubscriptionIntegrations": "integrations" / "listas-subscription" / noArgs
        }

id :: RouteDuplex' String -> RouteDuplex' ID
id = as ID.toString (ID.parse >>> note "Bad ID")

integrationKind :: RouteDuplex' String -> RouteDuplex' IntegrationKind
integrationKind = as toString parse
  where
  toString KindRss = "rss"
  toString KindListSubscription = "listas-subscription"

  parse = case _ of
    "rss" -> Right KindRss
    "listas-subscription" -> Right KindListSubscription
    s -> Left $ "Bad IntegrationKind '" <> s <> "'"


sortingResources :: RouteDuplex' String -> RouteDuplex' SortingResources
sortingResources = as toString parse
  where
  toString PositionAsc = "position_asc"
  toString PositionDes = "position_des"

  parse = case _ of
    "position_asc" -> Right PositionAsc
    "position_des" -> Right PositionDes
    s -> Left $ "Bad SortingResources '" <> s <> "'"

