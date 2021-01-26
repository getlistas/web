module Listasio.Component.HTML.ButtonGroupMenu where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray, toArray)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Component.HTML.Utils (whenElem)
import Tailwind as T

type Props p
  = { mainAction :: Maybe p
    , label :: String
    , toggleMenu :: Maybe p
    , isOpen :: Boolean
    }

props :: forall p. Props p
props =
  { mainAction: Nothing
  , label: ""
  , toggleMenu: Nothing
  , isOpen: false
  }

buttonGroupMenu :: forall i p. Props p -> NonEmptyArray { action :: Maybe p, text :: String } -> HH.HTML i p
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
            , T.px4
            , T.py2
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
        [ HH.text label ]
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
            [ HH.span [ HP.classes [ T.srOnly] ] [ HH.text "Open options" ]
            -- Heroicon name: chevron-down
            -- TODO: icon
            , HH.span [] [ HH.text "..." ]
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
                      (\{text, action} ->
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
                          , HE.onClick \_ -> action
                          ]
                          [ HH.text text ]
                      )
                  $ toArray menuItems
              ]
        ]
    ]
