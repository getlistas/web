module Listasio.Component.HTML.Wip where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Listasio.Component.HTML.Icons as Icons
import Tailwind as T

elem :: forall i p. HH.HTML i p
elem =
  HH.div
    [ HP.classes [ T.p2, T.roundedLg, T.bgDurazno, T.smP3, T.mb8 ] ]
    [ HH.div
        [ HP.classes [ T.flex, T.itemsCenter ] ]
        [ HH.span
            [ HP.classes [ T.flex, T.p2, T.roundedLg, T.bgManzana ] ]
            [ Icons.code
                [ Icons.classes [ T.h6, T.w6, T.textWhite ] ]
            ]
        , HH.p
            [ HP.classes [ T.ml3, T.fontMedium, T.textWhite ] ]
            [ HH.text "Work in progress"
            ]
        ]
    ]
