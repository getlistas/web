module Listasio.Component.HTML.CardsAndSidebar where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Utils (maybeElem, safeHref)
import Listasio.Data.Route (Route)
import Tailwind as T
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent (toEvent)

type SidebarItem p
  = { active :: Boolean
    , icon :: Icons.Icon
    , label :: String
    , link :: Maybe { action :: Event -> p, route :: Route }
    }

type SectionCard i p
  = { cta :: Maybe { label :: String, action :: p }
    , content :: HH.HTML i p
    , title :: String
    , description :: Maybe String
    }

layout :: forall i p. Array (SidebarItem p) -> Array (SectionCard i p) -> HH.HTML i p
layout items sections =
  HH.div
    [ HP.classes [ T.lgGrid, T.lgGridCols12, T.lgGapX5 ] ]
    [ HH.aside
        [ HP.classes
            [ T.py6
            , T.px2
            , T.smPx6
            , T.lgPy0
            , T.lgPx0
            , T.lgColSpan3
            ]
        ]
        [ HH.nav
            [ HP.classes [ T.spaceY1 ] ]
            $ map itemEl items
        ]
    , HH.div
        [ HP.classes [ T.spaceY6, T.smPx6, T.lgPx0, T.lgColSpan9 ] ]
        $ map cardEl sections
    ]
  where
  cardEl :: SectionCard i p -> HH.HTML i p
  cardEl {title, description, content, cta} =
    HH.div
      [ HP.classes [ T.shadow, T.smRoundedMd, T.smOverflowHidden ] ]
      [ HH.div
          [ HP.classes [ T.bgWhite, T.py6, T.px4, T.spaceY6, T.smP6 ] ]
          [ HH.div
              []
              [ HH.h3
                  [ HP.classes [ T.textLg, T.leading6, T.fontMedium, T.textGray400 ] ]
                  [ HH.text title ]
              , maybeElem description \desc ->
                  HH.p
                    [ HP.classes [ T.mt1, T.textSm, T.textGray500 ] ]
                    [ HH.text desc ]
              ]
          , content
          ]
      , maybeElem cta \{action, label} ->
          HH.div
            [ HP.classes [ T.px4, T.py3, T.bgGray50, T.textRight, T.smPx6 ] ]
            [ HH.button
                [ HE.onClick $ const action
                , HP.classes
                    [ T.bgIndigo600
                    , T.border
                    , T.borderTransparent
                    , T.roundedMd
                    , T.shadowSm
                    , T.py2
                    , T.px4
                    , T.inlineFlex
                    , T.justifyCenter
                    , T.textSm
                    , T.fontMedium
                    , T.textWhite
                    , T.hoverBgKiwi
                    , T.focusOutlineNone
                    , T.focusRing2
                    , T.focusRingOffset2
                    , T.focusRingIndigo500
                    ]
                , HP.type_ HP.ButtonSubmit
                ]
                [ HH.text label ]
            ]
      ]

  itemEl :: SidebarItem p -> HH.HTML i p
  itemEl {label, icon, active, link} =
    case link, active of
      _, true ->
        HH.a
          [ HP.classes
              [ T.bgGray100
              , T.textKiwi
              , T.hoverTextKiwi
              , T.hoverBgWhite
              , T.roundedMd
              , T.px3
              , T.py2
              , T.flex
              , T.itemsCenter
              , T.textSm
              , T.fontMedium
              , T.cursorDefault
              ]
          ]
          [ icon
              [ Icons.classes
                  [ T.textKiwi
                  , T.flexShrink0
                  , T.negMl1
                  , T.mr3
                  , T.h6
                  , T.w6
                  ]
              ]
          , HH.span [ HP.classes [ T.truncate ] ] [ HH.text label ]
          ]

      Nothing, _ ->
        HH.a
          [ HP.classes
              [ T.textGray200
              , T.cursorNotAllowed
              , T.roundedMd
              , T.px3
              , T.py2
              , T.flex
              , T.itemsCenter
              , T.textSm
              , T.fontMedium
              ]
          ]
          [ icon
              [ Icons.classes
                  [ T.textGray200
                  , T.flexShrink0
                  , T.negMl1
                  , T.mr3
                  , T.h6
                  , T.w6
                  ]
              ]
          , HH.span [ HP.classes [ T.truncate ] ] [ HH.text label ]
          ]

      Just {action, route}, _ ->
        HH.a
          [ HP.classes
              [ T.textGray400
              , T.hoverTextGray700
              , T.hoverBgWhite
              , T.group
              , T.roundedMd
              , T.px3
              , T.py2
              , T.flex
              , T.itemsCenter
              , T.textSm
              , T.fontMedium
              ]
          , safeHref route
          , HE.onClick $ action <<< toEvent
          ]
          [ icon
              [ Icons.classes
                  [ T.textGray400
                  , T.groupHoverTextGray700
                  , T.flexShrink0
                  , T.negMl1
                  , T.mr3
                  , T.h6
                  , T.w6
                  ]
              ]
          , HH.span [ HP.classes [ T.truncate ] ] [ HH.text label ]
          ]
