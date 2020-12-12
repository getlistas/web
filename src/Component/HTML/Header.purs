module Doneq.Component.HTML.Header where

import Prelude

import Data.Maybe (Maybe(..))
import Doneq.Component.HTML.Utils (safeHref)
import Doneq.Data.Profile (ProfileRep)
import Doneq.Data.Route (Route(..))
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
        [ HH.a [ safeHref Home, HE.onClick (onNavigate Home) ] [ HH.text "doneq" ] ]
    , HH.div
        [ HP.classes [ T.flex, T.justifyAround, T.mt8, T.w6d12 ] ]
        [ HH.a
            [ safeHref About, HE.onClick (onNavigate About) ]
            [ HH.text "About" ]
        , HH.a
            [ safeHref Discover, HE.onClick (onNavigate Discover) ]
            [ HH.text "Discover" ]
        , HH.a
            [ safeHref Login, HE.onClick (onNavigate Login) ]
            [ HH.text "Login" ]
        , HH.a
            [ safeHref Register, HE.onClick (onNavigate Register) ]
            [ HH.text "Register" ]
        ]
    ]
  where
  onNavigate r = Just <<< navigate r <<< toEvent
