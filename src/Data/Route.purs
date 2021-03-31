module Listasio.Data.Route where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Routing.Duplex (RouteDuplex', optional, root, segment, string)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))
import Slug (Slug)
import Slug as Slug

data Route
  = Home
  | About
  | Discover
  | Pricing
  | Changelog
  | Profile Slug
    -- Legal
  | Terms
  | Policy
    -- Auth
  | Login
  | Register
  | VerifyEmailSuccess
  | VerifyEmailFailure
    -- Private
  | Dashboard
  | Resources
  | Settings
  | CreateList
  | CreateResource { url :: Maybe String }
  | ViewList Slug
  | EditList Slug
  | IntegrationsList Slug

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

-- | Bidirectional codec for our route parsing.
routeCodec :: RouteDuplex' Route
routeCodec =
  root
    $ sum
        { "Home": noArgs
        , "About": "about" / noArgs
        , "Discover": "discover" / noArgs
        , "Pricing": "pricing" / noArgs
        , "Changelog": "changelog" / noArgs
        , "Profile": "u" / Slug.term segment
          -- Legal
        , "Terms": "terms" / noArgs
        , "Policy": "policy" / noArgs
          -- Auth
        , "Login": "signin" / noArgs
        , "Register": "register" / noArgs
        , "VerifyEmailSuccess": "verify-email" / "success" / noArgs
        , "VerifyEmailFailure": "verify-email" / "failure" / noArgs
          -- Private
        , "Dashboard": "dashboard" / noArgs
        , "Resources": "dashboard" / "resources" / noArgs
        , "Settings": "settings" / noArgs
        , "CreateList": "list" / "create" / noArgs
        , "CreateResource": "resources" / "create" ? { url: optional <<< string }
        , "ViewList": "list" / Slug.term segment
        , "EditList": "list" / Slug.term segment / "edit"
        , "IntegrationsList": "list" / Slug.term segment / "integrations"

        }
