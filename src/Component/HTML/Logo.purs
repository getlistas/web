module Listasio.Component.HTML.Logo where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Listasio.Component.HTML.Icons as Icons
import Tailwind as T

elem :: forall i p. HH.HTML i p
elem =
  HH.div
    [ HP.classes [ T.pr2, T.py2, T.flex, T.itemsCenter ] ]
    [ Icons.listas
        [ Icons.classes [ T.h12, T.w12, T.textKiwi ] ]
    , HH.h1
        [ HP.classes [ T.text3xl, T.ml2, T.leadingNone, T.textGray400 ] ]
        [ HH.text "listas" ]
    ]
