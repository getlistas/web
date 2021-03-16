module Listasio.Component.HTML.Header where

import Prelude

import Data.Maybe (Maybe(..), isJust, isNothing)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Utils (maybeElem, safeHref, whenElem, cx)
import Listasio.Data.Profile (ProfileRep)
import Listasio.Data.Route (Route(..))
import Listasio.Data.Username as Username
import Tailwind as T
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent (toEvent)

header :: forall i p r. Maybe { | ProfileRep r } -> (Route -> Event -> p) -> Maybe Route -> HH.HTML i p
header currentUser navigate route =
  HH.nav
    [ HP.classes [ T.py12, T.container, T.flex, T.justifyBetween, T.itemsCenter, T.flexWrap ] ]
    [ HH.a
        [ HP.classes [ T.border2, T.py3, T.px2, T.borderKiwi ]
        , safeHref Home
        , HE.onClick (onNavigate Home)
        ]
        [ HH.h1
            [ HP.classes [ T.text2xl, T.leadingNone, T.textGray400 ] ]
            [ HH.text "Listas" ]
        ]
    , HH.div
        [ HP.classes [ T.flex, T.flexWrap, T.justifyBetween, T.itemsCenter ] ]
        [ whenElem (isJust currentUser) \_ -> navLink Dashboard $ HH.text "Up next"
        , whenElem (isJust currentUser) \_ -> navLink Resources $ HH.text "Resources"
        , navLink Discover $ HH.text "Discover"
        , navLink About $ HH.text "About"
        , whenElem (isNothing currentUser) \_ -> navLink Register $ HH.text "Try for free"
        , whenElem (isNothing currentUser) \_ -> navLink Login $
            HH.div
              [ HP.classes [ T.flex, T.itemsCenter ] ]
              [ HH.span [] [ HH.text "Sign in" ]
              , Icons.login [ Icons.classes [ T.h5, T.w5, T.ml2 ] ]
              ]
        , maybeElem currentUser \{ name } ->
            HH.a
              [ safeHref Settings
              , HE.onClick (onNavigate Settings)
              , HP.classes
                  [ T.flex
                  , T.itemsCenter
                  , T.group
                  ]
              ]
              [ HH.span
                  [ HP.classes
                      [ cx T.fontBold $ isRoute Settings
                      , cx T.textGray400 $ isRoute Settings
                      , cx T.fontBold $ isRoute Settings
                      , cx T.textGray300 $ not $ isRoute Settings
                      , T.borderB2
                      , cx T.borderTransparent $ not $ isRoute Settings
                      , cx T.borderKiwi $ isRoute Settings
                      , T.groupHoverBorderB2
                      , T.groupHoverBorderKiwi
                      , T.mr2
                      ]
                  ]
                  [ HH.text $ Username.toString name ]
              , HH.div
                  [ HP.classes [ T.w10, T.h10, T.roundedFull, T.bgGray100, T.flex, T.justifyCenter, T.itemsCenter ] ]
                  [ Icons.userCircle [ Icons.classes [ T.w8, T.h8, T.textGray300 ] ] ]
              ]
        ]
    ]
  where
  onNavigate r = Just <<< navigate r <<< toEvent
  isRoute expected = Just expected == route
  navLink route' contents =
    HH.a
      [ safeHref route'
      , HE.onClick (onNavigate route')
      , HP.classes
          [ cx T.fontBold isCurrent
          , cx T.textGray400 isCurrent
          , cx T.fontBold isCurrent
          , cx T.textGray300 $ not isCurrent
          , T.borderB2
          , cx T.borderTransparent $ not isCurrent
          , cx T.borderKiwi isCurrent
          , T.hoverBorderB2
          , T.hoverBorderKiwi
          , T.mr8
          ]
      ]
      [ contents ]
    where isCurrent = isRoute route'
