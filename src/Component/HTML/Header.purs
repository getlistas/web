-- | This module exports a pure HTML function to render a consistent header throughout the app.
module Doneq.Component.HTML.Header where

import Data.Maybe (Maybe)
import Doneq.Data.Profile (ProfileRep)
import Doneq.Data.Route (Route)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Tailwind as T

-- | Our header will be a pure render function, but we'll require a route as an argument so we can
-- | judge whether a link should display active or not. We'll allow for any profile record type so
-- | long as it has our core fields -- this makes the header reusable across pages despite which
-- | variation on `Profile` they use.
header :: forall i p r. Maybe { | ProfileRep r } -> Route -> HH.HTML i p
header currentUser route =
  HH.nav
    [ HP.classes
        [ T.py2
        , T.container
        , T.bgRed100
        , T.flex
        , T.justifyCenter
        , T.itemsCenter
        , T.textGray800
        ]
    ]
    [ HH.h1 [ HP.classes [ T.text4xl, T.leadingNone ] ] [ HH.text "doneq" ]
    ]
