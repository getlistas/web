module Listasio.Component.HTML.Footer where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Utils (safeHref)
import Listasio.Data.Route (Route(..))
import Tailwind as T
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent as Mouse

footer :: forall i p. (Route -> Event -> p) -> HH.HTML i p
footer navigate =
  HH.footer
    [ HP.classes [ T.bgKiwiDark, T.mt12 ] ]
    [ HH.div
        [ HP.classes
            [ T.mxAuto
            , T.maxWMd
            , T.py12
            , T.px4
            , T.overflowHidden
            , T.smMaxW3xl
            , T.smPx6
            , T.lgMaxW7xl
            , T.lgPx8
            ]
        ]
        [ HH.nav
            [ HP.classes [ T.negMx5, T.negMy2, T.flex, T.flexWrap, T.justifyCenter ] ]
            [ link About "About"
            , externalLink
                "https://www.notion.so/gillchristian/8178e02c59604fc88bb5dc91358c736f"
                "Roadmap"
            -- , link Changelog "Changelog"
            , externalLink "https://collectednotes.com/getlistas" "Blog"
            -- , link Terms "Terms"
            -- , link Policy "Data Policy"
            , externalLink "mailto:hi@listas.io" "Contact us"
            ]
        , HH.div
            [ HP.classes [ T.mt8, T.flex, T.justifyCenter, T.spaceX6 ] ]
            [ HH.a
                [ HP.classes [ T.textWhite, T.hoverTextGray10 ], HP.href "https://twitter.com/getlistas" ]
                [ HH.span
                    [ HP.classes [ T.srOnly ] ]
                    [ HH.text "Twitter" ]
                , Icons.twitter [ Icons.classes [ T.h6, T.w6 ] ]
                ]
            , HH.a
                [ HP.classes [ T.textWhite, T.hoverTextGray10 ], HP.href "https://github.com/getlistas" ]
                [ HH.span
                    [ HP.classes [ T.srOnly ] ]
                    [ HH.text "GitHub" ]
                , Icons.github [ Icons.classes [ T.h6, T.w6 ] ]
                ]
            ]
        , HH.p
            [ HP.classes [ T.mt8, T.textCenter, T.textBase, T.textWhite ] ]
            -- TODO: get current year
            [ HH.text "© 2021 Listas" ]
        ]
    ]
  where
  onNavigate route = navigate route <<< Mouse.toEvent

  link route name =
    HH.div
      [ HP.classes [ T.px5, T.py2 ] ]
      [ HH.a
          [ HP.classes [ T.textBase, T.textWhite, T.hoverTextGray10 ]
          , safeHref route
          , HE.onClick $ onNavigate route
          ]
          [ HH.text name ]
      ]

  externalLink route name =
    HH.div
      [ HP.classes [ T.px5, T.py2 ] ]
      [ HH.a
          [ HP.classes [ T.textBase, T.textWhite, T.hoverTextGray10 ]
          , HP.href route
          ]
          [ HH.text name ]
      ]
