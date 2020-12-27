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
              , HP.classes [ cx T.fontBold $ route == Just Dashboard, cx T.underline $ route == Just Dashboard ]
              ]
              [ HH.text "Dashboard" ]
        , whenElem (isJust currentUser) \_ ->
            HH.a
              [ safeHref Done
              , HE.onClick (onNavigate Done)
              , HP.classes [ cx T.fontBold $ route == Just Done, cx T.underline $ route == Just Done ]
              ]
              [ HH.text "Done" ]
        , HH.a
            [ safeHref About
            , HE.onClick (onNavigate About)
            , HP.classes [ cx T.fontBold $ route == Just About, cx T.underline $ route == Just About ]
            ]
            [ HH.text "About" ]
        , HH.a
            [ safeHref Discover
            , HE.onClick (onNavigate Discover)
            , HP.classes [ cx T.fontBold $ route == Just Discover, cx T.underline $ route == Just Discover ]
            ]
            [ HH.text "Discover" ]
        , whenElem (isNothing currentUser) \_ ->
            HH.a
              [ safeHref Login
              , HE.onClick (onNavigate Login)
              , HP.classes [ cx T.fontBold $ route == Just Login, cx T.underline $ route == Just Login ]
              ]
              [ HH.text "Login" ]
        , whenElem (isNothing currentUser) \_ ->
            HH.a
              [ safeHref Register
              , HE.onClick (onNavigate Register)
              , HP.classes [ cx T.fontBold $ route == Just Register, cx T.underline $ route == Just Register ]
              ]
              [ HH.text "Register" ]
        , maybeElem currentUser \{ slug } ->
            HH.a
              [ safeHref Settings
              , HE.onClick (onNavigate Settings)
              , HP.classes [ cx T.fontBold $ route == Just Settings, cx T.underline $ route == Just Settings ]
              ]
              [ HH.text $ Username.toString slug ]
        ]
    ]
  where
  onNavigate r = Just <<< navigate r <<< toEvent
