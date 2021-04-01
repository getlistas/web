module Listasio.Component.HTML.ToggleGroup (toggleGroup) where

import Prelude

import Data.Maybe (Maybe)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Component.HTML.Utils (cx)
import Tailwind as T

type ButtonSpec p
  = { action :: Maybe p
    , label :: String
    , active :: Boolean
    }

toggleGroup :: forall i p. Boolean -> Array (ButtonSpec p) -> HH.HTML i p
toggleGroup disabled btns =
  HH.div
    [ HP.classes
        [ T.roundedMd
        , T.bgGray100
        , T.py1
        , T.flex
        , T.justifyBetween
        , T.divideX2
        , T.divideWhite
        , T.textSm
        , cx T.cursorNotAllowed disabled
        , T.wFull
        , T.smWAuto
        ]
    ]
    $ map (toggleGroupBtn disabled) btns

toggleGroupBtn :: forall i p. Boolean -> ButtonSpec p -> HH.HTML i p
toggleGroupBtn disabled { action, label, active } =
  HH.div
    [ HP.classes [ T.px2, T.wFull, T.smWAuto ] ]
    [ HH.button
        [ HP.type_ HP.ButtonButton
        , HE.onClick \_ -> action
        , HP.disabled disabled
        , HP.classes
            [ T.roundedMd
            , T.px4
            , T.py1
            , T.wFull
            , T.textCenter
            , cx T.bgKiwi active
            , cx T.textWhite active
            , cx T.bgTransparent $ not active
            , cx T.textGray300 $ not active
            , T.focusOutlineNone
            , T.focusRing2
            , T.focusRingGray300
            , T.focusRingInset
            , T.disabledCursorNotAllowed
            , cx T.disabledOpacity50 $ active
            ]
        ]
        [ HH.text label ]
    ]
