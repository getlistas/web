module Listasio.Component.HTML.ButtonGroupMenu where

import Prelude

import Bible.Component.HTML.Icons as Icons
import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Component.HTML.Utils (whenElem)
import Tailwind as T

type Props i p
  = { mainAction :: Maybe p
    , label :: HH.HTML i p
    , toggleMenu :: Maybe p
    , isOpen :: Boolean
    }

props :: forall i p. Props i p
props =
  { mainAction: Nothing
  , label: HH.text ""
  , toggleMenu: Nothing
  , isOpen: false
  }

buttonGroupMenu :: forall i p. Props i p -> NonEmptyArray { action :: Maybe p, label :: HH.HTML i p } -> HH.HTML i p
buttonGroupMenu { mainAction, label, toggleMenu, isOpen } menuItems =
  HH.span
    [ HP.classes
        [ T.relative
        , T.z0
        , T.inlineFlex
        , T.shadowSm
        , T.roundedMd
        ]
    ]
    [ HH.button
        [ HP.type_ HP.ButtonButton
        , HE.onClick \_ -> mainAction
        , HP.classes
            [ T.relative
            , T.inlineFlex
            , T.itemsCenter
            , T.p2
            , T.roundedLMd
            , T.border
            , T.borderGray400
            , T.bgWhite
            , T.textXs
            , T.fontMedium
            , T.textGray400
            , T.leadingNone
            , T.hoverBgGray100
            , T.focusZ10
            , T.focusOutlineNone
            , T.focusRing1
            , T.focusRingKiwi
            , T.focusBorderKiwi
            ]
        ]
        [ label ]
    , HH.span
        [ HP.classes [ T.negMlPx, T.relative, T.block ] ]
        [ HH.button
            [ HP.type_ HP.ButtonButton
            , HE.onClick \_ -> toggleMenu
            , HP.classes
                [ T.relative
                , T.inlineFlex
                , T.itemsCenter
                , T.px2
                , T.py2
                , T.roundedRMd
                , T.border
                , T.borderGray400
                , T.bgWhite
                , T.textGray400
                , T.hoverBgGray100
                , T.focusZ10
                , T.focusOutlineNone
                , T.focusRing1
                , T.focusRingKiwi
                , T.focusBorderKiwi
                ]
            ]
            [ HH.span [ HP.classes [ T.srOnly] ] [ HH.text "Open options" ]
            -- Heroicon name: chevron-down
            -- TODO: icon
            , Icons.dotsHorizontal [ Icons.classes [ T.flexShrink0, T.h5, T.w5 ] ]
            ]
        -- TODO: transitions
        --
        -- Dropdown panel, show/hide based on dropdown state.
        --
        -- Entering: "transition ease-out duration-100"
        --   From: "transform opacity-0 scale-95"
        --   To: "transform opacity-100 scale-100"
        -- Leaving: "transition ease-in duration-75"
        --   From: "transform opacity-100 scale-100"
        --   To: "transform opacity-0 scale-95"
        , whenElem isOpen \_ ->
            HH.div
              [ HP.classes
                  [ T.originTopRight
                  , T.absolute
                  , T.right0
                  , T.mt2
                  , T.negMr1
                  , T.w40
                  , T.roundedMd
                  , T.shadowLg
                  , T.bgWhite
                  , T.ring1
                  , T.ringBlack
                  , T.ringOpacity5
                ]
              ]
              [ HH.div
                  [ HP.classes [ T.py1 ] ]
                  $ map
                      (\i ->
                        HH.a
                          [ HP.classes
                              [ T.block
                              , T.px4
                              , T.py2
                              , T.textSm
                              , T.textGray700
                              , T.hoverBgGray100
                              , T.hoverTextGray900
                              , T.cursorPointer
                              ]
                          , HE.onClick \_ -> i.action
                          ]
                          [ i.label ]
                      )
                  $ toArray menuItems
              ]
        ]
    ]
