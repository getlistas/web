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
  | Discover
  | Profile Slug
  | PublicList Slug Slug
    -- Info
  | About
  | Pricing
  | Changelog
  | HowTo
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
  | Library
  | Settings
  | CreateList
  | CreateResource {url :: Maybe String, title :: Maybe String, text :: Maybe String}
    -- List
  | ViewList Slug
  | EditList Slug Slug
  | IntegrationsList Slug Slug
  | ImportResourcesList Slug Slug

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

-- | Bidirectional codec for our route parsing.
routeCodec :: RouteDuplex' Route
routeCodec =
  root
    $ sum
        { "Home": noArgs
        , "Discover": "discover" / noArgs
        , "Profile": "u" / Slug.term segment
        , "PublicList": "l" / Slug.term segment / Slug.term segment
          -- Info
        , "About": "about" / noArgs
        , "Pricing": "pricing" / noArgs
        , "Changelog": "changelog" / noArgs
        , "HowTo": "how-to" / noArgs
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
        , "Library": "library" / noArgs
        , "Settings": "settings" / noArgs
        , "CreateList": "list" / "create" / noArgs
        , "CreateResource": "resources" / "create"
            ? { url: optional <<< string
              , title: optional <<< string
              , text: optional <<< string
              }

          -- TODO remove this route
        , "ViewList": "list" / Slug.term segment
        , "EditList": "l" / Slug.term segment / Slug.term segment / "edit"
        , "IntegrationsList": "l" / Slug.term segment / Slug.term segment / "integrations"
        , "ImportResourcesList": "l" / Slug.term segment / Slug.term segment / "import"
        }
