module Doneq.Data.Route where

import Prelude hiding ((/))
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Doneq.Data.Username (Username)
import Doneq.Data.Username as Username
import Routing.Duplex (RouteDuplex', as, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Slug (Slug)
import Slug as Slug

data Route
  = Home
  | About
  | Login
  | Register
  | Settings
  | Profile Username
  | ViewList Slug
  | EditList Slug
  | Dashboard
  | Done
  | Discover

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
        , "Login": "login" / noArgs
        , "Register": "register" / noArgs
        , "Settings": "settings" / noArgs
        , "Profile": "profile" / uname segment
        , "ViewList": "list" / slug segment
        , "EditList": "list" / slug segment / "edit"
        , "Dashboard": "dashboard" / noArgs
        , "Done": "dashboard" / "done" / noArgs
        , "Discover": "discover" / noArgs
        }

-- | This combinator transforms a codec over `String` into one that operates on the `Slug` type.
slug :: RouteDuplex' String -> RouteDuplex' Slug
slug = as Slug.toString (Slug.parse >>> note "Bad slug")

-- | This combinator transforms a codec over `String` into one that operates on the `Username` type.
uname :: RouteDuplex' String -> RouteDuplex' Username
uname = as Username.toString (Username.parse >>> note "Bad username")
