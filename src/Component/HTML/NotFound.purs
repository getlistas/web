module Listasio.Component.HTML.NotFound where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Tailwind as T

elem :: forall i p. HH.HTML i p
elem =
  HH.div
    [ HP.classes [ T.textGray400, T.fontMono, T.mt10, T.textCenter ] ]
    [ HH.p [ HP.classes [ T.mb6, T.text2xl ] ] [ HH.text "404 - Not found" ]
    , HH.p [ HP.classes [ T.text4xl ] ] [ HH.text "¯\\_(ツ)_/¯" ]
    ]
