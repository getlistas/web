module Listasio.Component.HTML.Logo where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Listasio.Component.HTML.Icons as Icons
import Tailwind as T

elem :: forall i p. HH.HTML i p
elem =
  HH.div
    [ HP.classes [ T.p2, T.flex, T.itemsCenter ] ]
    [ Icons.listas
        [ Icons.classes [ T.h16, T.w16, T.textKiwi ] ]
    , HH.h1
        [ HP.classes [ T.text3xl, T.ml1, T.leadingNone, T.textGray400 ] ]
        [ HH.text "listas" ]
    ]
