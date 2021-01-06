module Listasio.Component.HTML.Header where

import Prelude

import Data.Maybe (Maybe(..), isJust, isNothing)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
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
        [ HP.classes [ T.border2, T.py3, T.px2, T.borderGray400 ]
        , safeHref Home
        , HE.onClick (onNavigate Home)
        ]
        [ HH.h1
            [ HP.classes [ T.text2xl, T.leadingNone, T.textGray400 ] ]
            [ HH.text "listas.io" ]
        ]
    , HH.div
        [ HP.classes [ T.flex, T.flexWrap, T.justifyBetween, T.itemsCenter ] ]
        [ whenElem (isJust currentUser) \_ -> navLink Dashboard "Dashboard"
        , whenElem (isJust currentUser) \_ -> navLink Resources "Resources"
        , navLink Discover "Discover"
        , navLink About "About"
        , whenElem (isNothing currentUser) \_ -> navLink Register "Try for free"
        , whenElem (isNothing currentUser) \_ -> navLink Login "Sign in"
        , maybeElem currentUser \{ slug } ->
            HH.a
              [ safeHref Settings
              , HE.onClick (onNavigate Settings)
              , HP.classes
                  [ T.flex
                  , T.itemsCenter
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
                      , T.hoverBorderB2
                      , T.hoverBorderKiwi
                      , T.mr2
                      ]
                  ]
                  [ HH.text $ Username.toString slug ]
              , HH.img [ HP.classes [ T.w8, T.h8, T.roundedFull ], HP.src "https://avatars2.githubusercontent.com/u/8309423?s=460&u=0f306a70fdcc2359d21b4918efaabf617a396c91&v=4" ]
              ]
        ]
    ]
  where
  onNavigate r = Just <<< navigate r <<< toEvent
  isRoute expected = Just expected == route
  navLink route' text =
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
      [ HH.text text ]
    where isCurrent = isRoute route'
