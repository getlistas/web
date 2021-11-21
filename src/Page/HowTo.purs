module Listasio.Page.HowTo where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Component.HTML.Utils (safeHref)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Route (Route(..))
import Listasio.Store as Store
import Tailwind as T
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent as Mouse

_slot :: Proxy "howTo"
_slot = Proxy

data Action
  = Receive (Connected (Maybe ProfileWithIdAndEmail) Unit)
  | Navigate Route Event

type State
  = { currentUser :: Maybe ProfileWithIdAndEmail }

component
  :: forall q o m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => H.Component q Unit o m
component = connect (selectEq _.currentUser) $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }
  where
  initialState { context: currentUser } = { currentUser }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Receive { context: currentUser } ->
      H.modify_ _ { currentUser = currentUser }

    Navigate route e -> navigate_ e route

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render _ =
    HH.div
      []
      [ HH.div
          [ HP.classes [ T.pt2, T.flex, T.flexCol, T.gap12 ] ]
          [ HH.h1
              [ HP.classes [ T.textGray400, T.text4xl, T.fontBold ] ]
              [ HH.text "How to add resources" ]
          , HH.div
              []
              [ HH.h2
                  [ HP.classes [ T.textGray400, T.mb4, T.text2xl, T.fontBold ]
                  , HP.id "quick-save"
                  ]
                  [ HH.text "Quick save" ]
              , HH.p
                  [ HP.classes [ T.textGray400, T.textLg ] ]
                  [ HH.text "On Desktop, press "
                  , HH.code
                      [ HP.classes [ T.textManzana, T.bgGray100, T.p1, T.roundedSm ] ]
                      [ HH.text "Ctrl + v" ]
                  , HH.text " or "
                  , HH.code
                      [ HP.classes [ T.textManzana, T.bgGray100, T.p1, T.roundedSm ] ]
                      [ HH.text "Cmd + v" ]
                  , HH.text " when on the "
                  , HH.a
                      [ safeHref Dashboard
                      , HE.onClick $ Navigate Dashboard <<< Mouse.toEvent
                      , HP.target "_blank"
                      , HP.rel "noreferrer noopener nofollow"
                      , HP.classes
                          [ T.textKiwi
                          , T.hoverUnderline
                          , T.cursorPointer
                          ]
                      ]
                      [ HH.text "Up next" ]
                  , HH.text " page or any list page to quickly save an URL you copied."
                  ]
              ]
          , HH.div []
              [ HH.h2
                  [ HP.classes [ T.textGray400, T.mb4, T.text2xl, T.fontBold ]
                  , HP.id "browser-extensions"
                  ]
                  [ HH.text "Browser extensions" ]
              , HH.p
                  [ HP.classes [ T.textGray400, T.mb2, T.textLg ] ]
                  [ HH.text "Sabe the current tab to Listas with our browser extensions." ]
              , HH.ul
                  [ HP.classes [ T.textGray400, T.textLg, T.listDisc, T.listInside ] ]
                  [ HH.li
                      []
                      [ HH.a
                          [ HP.href "https://chrome.google.com/webstore/detail/save-to-listas/nnpnmodoppncmkhbmdeabepeppcnmdno"
                          , HP.target "_blank"
                          , HP.rel "noreferrer noopener nofollow"
                          , HP.classes
                              [ T.textKiwi
                              , T.hoverUnderline
                              , T.cursorPointer
                              ]
                          ]
                          [ HH.text "Chrome extension" ]
                      ]
                  , HH.li
                      []
                      [ HH.a
                          [ HP.href "https://addons.mozilla.org/en-US/firefox/addon/save-to-listas/"
                          , HP.target "_blank"
                          , HP.rel "noreferrer noopener nofollow"
                          , HP.classes
                              [ T.textKiwi
                              , T.hoverUnderline
                              , T.cursorPointer
                              ]
                          ]
                          [ HH.text "Firefox add-on" ]
                      ]
                  , HH.li
                      []
                      [ HH.text "Safari extension coming soon" ]
                  ]

              ]
          , HH.div
              []
              [ HH.h2
                  [ HP.classes [ T.textGray400, T.mb4, T.text2xl, T.fontBold ]
                  , HP.id "ios-shortcut"
                  ]
                  [ HH.text "Save to Listas on iOS" ]
              , HH.p
                  [ HP.classes [ T.textGray400, T.mb2, T.textLg ] ]
                  [ HH.text "Install "
                  , HH.a
                      [ HP.href "https://www.icloud.com/shortcuts/d3209ade253140e2992d074f0889652a"
                      , HP.target "_blank"
                      , HP.rel "noreferrer noopener nofollow"
                      , HP.classes
                          [ T.textKiwi
                          , T.hoverUnderline
                          , T.cursorPointer
                          ]
                      ]
                      [ HH.text "the iOS Shortcut" ]
                  , HH.text " to share any link from other apps to Listas."
                  ]
              , HH.p
                  [ HP.classes [ T.textGray400, T.mb2, T.textLg ] ]
                  [ HH.text "For mor information on shortcuts check the iOS "
                  , HH.a
                      [ HP.href "https://support.apple.com/en-gb/guide/shortcuts/welcome/ios"
                      , HP.target "_blank"
                      , HP.rel "noreferrer noopener nofollow"
                      , HP.classes
                          [ T.textKiwi
                          , T.hoverUnderline
                          , T.cursorPointer
                          ]
                      ]
                      [ HH.text "Shortcuts User Guide" ]
                  , HH.text "."
                  ]
              , HH.img
                  [ HP.src "https://i.imgur.com/uNwwLr9.jpg"
                  , HP.classes [ T.mt2, T.maxWFull ]
                  ]
              ]
          , HH.div
              []
              [ HH.h2
                  [ HP.classes [ T.textGray400, T.mb4, T.text2xl, T.fontBold ]
                  , HP.id "android-pwa"
                  ]
                  [ HH.text "Save to Listas on Android" ]
              , HH.p
                  [ HP.classes [ T.textGray400, T.mb2, T.textLg ] ]
                  [ HH.text "Install Listas to your Home screen to share any link from other apps to Listas."
                  ]
              , HH.div
                  [ HP.classes [ T.flex, T.flexWrap, T.gap4 ] ]
                  [ HH.img
                      [ HP.src "https://i.imgur.com/Q7X1Y2O.jpg"
                      , HP.classes [ T.maxWFull ]
                      ]
                  , HH.img
                      [ HP.src "https://i.imgur.com/bJNiGa2.jpg"
                      , HP.classes [ T.maxWFull ]
                      ]
                  ]
              ]
          , HH.div
              []
              [ HH.h2
                  [ HP.classes [ T.textGray400, T.mb4, T.text2xl, T.fontBold ]
                  , HP.id "bulk-import"
                  ]
                  [ HH.text "Bulk import" ]
              , HH.p
                  [ HP.classes [ T.textGray400, T.textLg, T.mb2 ] ]
                  [ HH.text "Import many links at once on the Import section on the list's Settings page."
                  ]
              , HH.p
                  [ HP.classes [ T.textGray400, T.textLg, T.mb2 ] ]
                  [ HH.text "For example, this can be used to import all OneTab's exported links at once."
                  ]
              , HH.img
                  [ HP.src "https://i.imgur.com/xeEp7tW.png"
                  , HP.classes [ T.maxWFull ]
                  ]
              ]
          ]
      ]

