module Listasio.Component.HTML.Modal where

import Prelude

import Listasio.Component.HTML.Icons as Icons
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Component.HTML.Utils (maybeElem, whenElem)
import Tailwind as T
import Web.UIEvent.KeyboardEvent as KeyboardEvent

modal :: forall i p. Boolean -> Maybe p -> HH.HTML i p -> HH.HTML i p
modal show mbOnClose content =
  whenElem show \_ ->
    HH.div
      [ HP.classes [ T.fixed, T.z10, T.inset0, T.overflowYAuto ]
      , HE.onKeyDown \e ->
          if KeyboardEvent.code e == "Escape" then mbOnClose else Nothing
      ]
      [ HH.div
          [ HP.classes
              [ T.flex
              , T.itemsEnd
              , T.justifyCenter
              , T.minHScreen
              , T.pt4
              , T.px4
              , T.pb20
              , T.textCenter
              , T.smBlock
              , T.smP0
              ]
          ]
          [ -- Background overlay

            -- TODO
            -- Entering: "ease-out duration-300"
            --   From: "opacity-0"
            --   To: "opacity-100"

            -- Leaving: "ease-in duration-200"
            --   From: "opacity-100"
            --   To: "opacity-0"
            HH.div
              [ HE.onClick \_ -> mbOnClose
              , HP.classes [ T.fixed, T.inset0, T.transitionOpacity ]
              ]
              [ HH.div [ HP.classes [ T.absolute, T.inset0, T.bgGray200, T.opacity50 ] ] [] ]
            -- This element is to trick the browser into centering the modal contents
          , HH.span [ HP.classes [ T.hidden, T.smInlineBlock, T.smAlignMiddle, T.smHScreen ] ] []
            -- Modal panel

            -- TODO
            -- Entering: "ease-out duration-300"
            --   From: "opacity-0 translate-y-4 sm:translate-y-0 sm:scale-95"
            --   To: "opacity-100 translate-y-0 sm:scale-100"

            -- Leaving: "ease-in duration-200"
            --   From: "opacity-100 translate-y-0 sm:scale-100"
            --   To: "opacity-0 translate-y-4 sm:translate-y-0 sm:scale-95"
          , whenElem show \_ ->
              HH.div
                [ HP.classes
                    [ T.inlineBlock
                    , T.alignBottom
                    , T.bgWhite
                    , T.roundedLg
                    , T.px4
                    , T.pt5
                    , T.pb4
                    , T.textLeft
                    , T.overflowHidden
                    , T.transform
                    , T.transitionAll
                    , T.smMy8
                    , T.smAlignMiddle
                    , T.smMaxWLg
                    , T.smWFull
                    , T.smP6
                    ]
                ]
                [ maybeElem mbOnClose \_ ->
                    HH.div
                      [ HP.classes
                          [ T.absolute
                          , T.top0
                          , T.right0
                          , T.pt4
                          , T.pr4
                          ]
                      ]
                      [ HH.button
                          [ HE.onClick \_ -> mbOnClose
                          , HP.type_ HP.ButtonButton
                          , HP.classes
                              [ T.bgWhite
                              , T.roundedMd
                              , T.textGray200
                              , T.hoverTextGray300
                              , T.focusOutlineNone
                              , T.focusRing2
                              , T.focusRingOffset2
                              , T.focusRingGray300
                              ]
                          ]
                          [ HH.span [ HP.classes [ T.srOnly ] ] [ HH.text "Close" ]
                          , Icons.x [ Icons.classes [ T.h5, T.w5 ] ]
                          ]
                      ]
                , content
                ]
          ]
      ]
