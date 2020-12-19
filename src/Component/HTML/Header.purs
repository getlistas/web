module Listasio.Component.HTML.Header where

import Prelude
import Data.Maybe (Maybe(..), isJust, isNothing)
import Listasio.Component.HTML.Utils (maybeElem, safeHref, whenElem, cx)
import Listasio.Data.Profile (ProfileRep)
import Listasio.Data.Route (Route(..))
import Listasio.Data.Username as Username
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Tailwind as T
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent (toEvent)

header :: forall i p r. Maybe { | ProfileRep r } -> (Route -> Event -> p) -> Route -> HH.HTML i p
header currentUser navigate route =
  HH.nav
    [ HP.classes
        [ T.py2
        , T.container
        , T.bgRed100
        , T.flex
        , T.flexCol
        , T.justifyCenter
        , T.itemsCenter
        , T.textGray800
        ]
    ]
    [ HH.h1
        [ HP.classes [ T.text4xl, T.leadingNone ] ]
        [ HH.a [ safeHref Home, HE.onClick (onNavigate Home) ] [ HH.text "listas.io" ] ]
    , HH.div
        [ HP.classes [ T.flex, T.justifyAround, T.mt8, T.w6d12 ] ]
        [ whenElem (isJust currentUser) \_ ->
            HH.a
              [ safeHref Dashboard
              , HE.onClick (onNavigate Dashboard)
              , HP.classes [ cx T.fontBold $ route == Dashboard, cx T.underline $ route == Dashboard ]
              ]
              [ HH.text "Dashboard" ]
        , whenElem (isJust currentUser) \_ ->
            HH.a
              [ safeHref Done
              , HE.onClick (onNavigate Done)
              , HP.classes [ cx T.fontBold $ route == Done, cx T.underline $ route == Done ]
              ]
              [ HH.text "Done" ]
        , HH.a
            [ safeHref About
            , HE.onClick (onNavigate About)
            , HP.classes [ cx T.fontBold $ route == About, cx T.underline $ route == About ]
            ]
            [ HH.text "About" ]
        , HH.a
            [ safeHref Discover
            , HE.onClick (onNavigate Discover)
            , HP.classes [ cx T.fontBold $ route == Discover, cx T.underline $ route == Discover ]
            ]
            [ HH.text "Discover" ]
        , whenElem (isNothing currentUser) \_ ->
            HH.a
              [ safeHref Login
              , HE.onClick (onNavigate Login)
              , HP.classes [ cx T.fontBold $ route == Login, cx T.underline $ route == Login ]
              ]
              [ HH.text "Login" ]
        , whenElem (isNothing currentUser) \_ ->
            HH.a
              [ safeHref Register
              , HE.onClick (onNavigate Register)
              , HP.classes [ cx T.fontBold $ route == Register, cx T.underline $ route == Register ]
              ]
              [ HH.text "Register" ]
        , maybeElem currentUser (user <<< Username.toString <<< _.slug)
        ]
    ]
  where
  onNavigate r = Just <<< navigate r <<< toEvent

  user username =
    HH.div [ HP.classes [ T.flex ] ]
      [ HH.img
          [ HP.classes [ T.roundedFull, T.h6, T.w6, T.mr1, T.ml2 ]
          , HP.src "https://static.productionready.io/images/smiley-cyrus.jpg"
          ]
      , HH.text username
      ]
