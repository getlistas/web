module Listasio.Component.HTML.Button where

import Prelude

import DOM.HTML.Indexed (HTMLbutton)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Tailwind as T

type DangerProps i p
  =
  { label :: HH.HTML i p
  , disabled :: Boolean
  , action :: p
  , props :: Array (HH.IProp HTMLbutton p)
  , classes :: Array HH.ClassName
  }

dangerDefaultProps :: forall i p. p -> DangerProps i p
dangerDefaultProps action =
  { label: HH.text ""
  , disabled: false
  , action
  , props: []
  , classes: []
  }

danger :: forall i p. DangerProps i p -> HH.HTML i p
danger { label, disabled, action, props, classes } =
  HH.button
    ( props
        <>
          [ HP.classes
              ( classes
                  <>
                    [ T.inlineFlex
                    , T.itemsCenter
                    , T.justifyCenter
                    , T.px4
                    , T.py2
                    , T.border
                    , T.borderTransparent
                    , T.fontMedium
                    , T.roundedMd
                    , T.textRed700
                    , T.bgRed100
                    , T.hoverBgRed200
                    , T.focusOutlineNone
                    , T.focusRing2
                    , T.focusRingOffset2
                    , T.focusRingManzana
                    , T.smTextSm
                    ]
              )
          , HP.type_ HP.ButtonButton
          , HP.disabled disabled
          , HE.onClick $ const action
          ]
    )
    [ label ]

primary :: forall i p. HH.HTML i p -> Boolean -> p -> HH.HTML i p
primary label disabled action =
  HH.button
    [ HP.classes
        [ T.inlineFlex
        , T.itemsCenter
        , T.justifyCenter
        , T.px4
        , T.py2
        , T.border
        , T.borderTransparent
        , T.fontMedium
        , T.roundedMd
        , T.textWhite
        , T.bgKiwi
        , T.hoverBgOpacity75
        , T.focusOutlineNone
        , T.focusRing2
        , T.focusRingOffset2
        , T.focusRingKiwi
        , T.smTextSm
        , T.disabledCursorNotAllowed
        , T.disabledOpacity50
        ]
    , HP.type_ HP.ButtonButton
    , HP.disabled disabled
    , HE.onClick $ const action
    ]
    [ label ]
