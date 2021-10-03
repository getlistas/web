module Listasio.Component.HTML.Tag where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Tailwind as T

tag :: forall i p. String -> HH.HTML i p
tag text =
  HH.span
    [ HP.classes
        [ T.leadingNormal
        , T.mr1
        , T.mb1
        , T.px1
        , T.textGray400
        , T.textXs
        , T.roundedSm
        , T.border
        , T.borderGray400
        ]
    ]
    [ HH.text text ]
