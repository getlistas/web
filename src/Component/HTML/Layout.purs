module Listasio.Component.HTML.Layout where

import Data.Maybe (Maybe)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Listasio.Component.HTML.Header (header)
import Listasio.Data.Profile (ProfileRep)
import Listasio.Data.Route (Route)
import Tailwind as T
import Web.Event.Event (Event)

dashboard :: forall i p r.
     Maybe { | ProfileRep r }
  -> (Route -> Event -> p)
  -> Maybe Route
  -> HH.HTML i p
  -> HH.HTML i p
  -> HH.HTML i p
dashboard currentUser navigate route title content =
  HH.div
    [ HP.classes [ T.minHScreen, T.wScreen, T.bgGray100 ] ]
    [ HH.div
        [ HP.classes [ T.container, T.mxAuto, T.px2, T.flex, T.flexCol ] ]
        [ header currentUser navigate route
        , HH.h1
            [ HP.classes [ T.textGray400, T.mb6, T.text4xl, T.fontBold ] ]
            [ title ]
        , content
        ]
    ]

noheader :: forall i p r.
     Maybe { | ProfileRep r }
  -> (Route -> Event -> p)
  -> Maybe Route
  -> HH.HTML i p
  -> HH.HTML i p
noheader currentUser navigate route content =
  HH.div
    [ HP.classes [ T.minHScreen, T.wScreen, T.bgGray100 ] ]
    [ HH.div
        [ HP.classes [ T.container, T.mxAuto, T.px2, T.flex, T.flexCol ] ]
        [ header currentUser navigate route
        , content
        ]
    ]
