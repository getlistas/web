module Listasio.Component.HTML.ButtonGroupMenu where

import Prelude

import Listasio.Component.HTML.Icons as Icons
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Component.HTML.Utils (whenElem)
import Tailwind as T

type Props i p
  =
  { mainAction :: p
  , toggleMenu :: p
  , label :: HH.HTML i p
  , isOpen :: Boolean
  , disabled :: Boolean
  }

props :: forall i p. { mainAction :: p, toggleMenu :: p } -> Props i p
props { mainAction, toggleMenu } =
  { mainAction
  , toggleMenu
  , label: HH.text ""
  , isOpen: false
  , disabled: false
  }

type MenuItem i p
  =
  { action :: p
  , label :: HH.HTML i p
  , disabled :: Boolean
  }

buttonGroupMenu :: forall i p. Props i p -> NonEmptyArray (MenuItem i p) -> HH.HTML i p
buttonGroupMenu { mainAction, label, toggleMenu, isOpen, disabled } menuItems =
  HH.span
    [ HP.classes
        [ T.relative
        , T.z10
        , T.inlineFlex
        , T.shadowSm
        , T.roundedMd
        ]
    ]
    [ HH.button
        [ HP.type_ HP.ButtonButton
        , HE.onClick $ const mainAction
        , HP.classes
            [ T.relative
            , T.inlineFlex
            , T.itemsCenter
            , T.py1
            , T.px2
            , T.textSm
            , T.textWhite
            , T.roundedLMd
            , T.bgKiwi
            , T.hoverBgKiwiDark
            , T.focusZ10
            , T.focusOutlineNone
            , T.focusRing1
            , T.focusRingKiwiDark
            , T.focusRingOffset1
            , T.disabledCursorNotAllowed
            , T.disabledOpacity50
            ]
        , HP.disabled disabled
        ]
        [ label ]
    , HH.button
        [ HP.type_ HP.ButtonButton
        , HE.onClick $ const toggleMenu
        , HP.classes
            [ T.relative
            , T.inlineFlex
            , T.itemsCenter
            , T.py1
            , T.px2
            , T.textXs
            , T.textWhite
            , T.roundedRMd
            , T.bgKiwi
            , T.hoverBgKiwiDark
            , T.focusZ10
            , T.focusOutlineNone
            , T.focusRing1
            , T.focusRingKiwiDark
            , T.focusRingOffset1
            ]
        ]
        [ HH.span [ HP.classes [ T.srOnly ] ] [ HH.text "Open options" ]
        , Icons.dotsVertical [ Icons.classes [ T.h4, T.w4 ] ]
        ]

    -- Invisible overlay to close on outside click
    , whenElem isOpen \_ ->
        HH.div
          [ HE.onClick $ const toggleMenu
          , HP.classes [ T.fixed, T.inset0 ]
          ]
          [ HH.div [ HP.classes [ T.absolute, T.inset0 ] ] [] ]

    , whenElem isOpen \_ ->
        HH.div
          [ HP.classes
              [ T.originTopRight
              , T.absolute
              , T.left12
              , T.top1
              , T.mt8
              , T.w44
              , T.roundedMd
              , T.shadowLg
              , T.bgWhite
              , T.ring1
              , T.ringBlack
              , T.ringOpacity5
              , T.z20
              ]
          ]
          [ HH.div
              [ HP.classes [ T.py1 ] ]
              $ map
                  ( \i ->
                      HH.button
                        [ HP.classes
                            [ T.block
                            , T.px4
                            , T.py2
                            , T.textSm
                            , T.textGray700
                            , T.hoverBgGray100
                            , T.hoverTextGray900
                            , T.cursorPointer
                            , T.wFull
                            , T.disabledTextGray200
                            , T.disabledCursorNotAllowed
                            , T.focusOutlineNone
                            , T.focusRing2
                            , T.focusRingGray300
                            ]
                        , HP.type_ HP.ButtonButton
                        , HE.onClick $ const i.action
                        , HP.disabled i.disabled
                        ]
                        [ i.label ]
                  )
              $ toArray menuItems
          ]
    ]
