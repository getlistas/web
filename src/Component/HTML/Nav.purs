module Listasio.Component.HTML.Nav where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Lens (over, set)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Capability.Resource.User (class ManageUser)
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Utils (cx, safeHref, whenElem)
import Listasio.Data.Lens (_menuOpen)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Route (Route(..))
import Listasio.Data.Username as Username
import Listasio.Env (UserEnv)
import Tailwind as T
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent as Mouse

type Slot = forall query. H.Slot query Void Unit

data AuthStatus
  = ShowLoading
  | ShowAuth
  | ShowUser ProfileWithIdAndEmail

derive instance eqForm :: Eq AuthStatus

data Action
  = Receive {currentUser :: Maybe ProfileWithIdAndEmail, route :: Maybe Route}
  | Navigate Route Event
  | ToggleMenu
  | AndClose Action

type State
  = { currentUser :: Maybe ProfileWithIdAndEmail
    , menuOpen :: Boolean
    , authStatus :: AuthStatus
    , currentRoute :: Maybe Route
    }

component
  :: forall q o m r
   . MonadAff m
  => MonadAsk {userEnv :: UserEnv | r} m
  => ManageUser m
  => Navigate m
  => H.Component HH.HTML q {route:: Maybe Route} o m
component = Connect.component $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }
  where
  initialState {currentUser, route} =
    { currentUser
    , menuOpen: false
    , authStatus: maybe ShowLoading ShowUser currentUser
    , currentRoute: route
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Receive {currentUser, route} -> do
     H.modify_ _
       { authStatus = maybe ShowAuth ShowUser currentUser
       , currentUser = currentUser
       , currentRoute = route
       }

    Navigate route e -> navigate_ e route

    ToggleMenu -> H.modify_ $ over _menuOpen not

    AndClose a -> do
      H.modify_ $ set _menuOpen false
      handleAction a

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render {currentUser, menuOpen, authStatus, currentRoute} =
    HH.div
      [ HP.classes [ T.relative, T.bgGray10 ] ]
      [ HH.div
          [ HP.classes [ T.relative, T.pt6, T.pb16 ] ]
          [ nav
          , mobileNav
          ]
      ]

    where
    -- TODO: Home nav shares most of this

    onNavigate route = Just <<< Navigate route <<< Mouse.toEvent

    onNavigateAndClose route = Just <<< AndClose <<< Navigate route <<< Mouse.toEvent

    isRoute = (_ == currentRoute) <<< Just

    logo =
      HH.a
        [ HP.classes [ T.border2, T.py3, T.px2, T.borderKiwi, T.block ]
        , safeHref Home
        , HE.onClick $ onNavigate Home
        ]
        [ HH.h1
            [ HP.classes [ T.text2xl, T.leadingNone, T.textGray400 ] ]
            [ HH.text "Listas" ]
        ]

    mobileLink route name =
      HH.a
        [ HP.classes
            [ T.block
            , T.px3
            , T.py2
            , T.roundedMd
            , T.textBase
            , T.fontMedium
            , T.textGray300
            , T.hoverTextGray400
            , T.hoverBgGray100
            , cx T.bgGray100 isCurrent
            ]
        , safeHref route
        , HE.onClick $ onNavigateAndClose route
        ]
        [ HH.text name ]

      where isCurrent = isRoute route

    desktopLink route name =
      HH.a
        [ HP.classes
            [ T.fontMedium
            , cx T.textGray400 isCurrent
            , cx T.textGray300 $ not isCurrent
            , T.borderB2
            , cx T.borderTransparent $ not isCurrent
            , cx T.borderKiwi isCurrent
            , T.hoverBorderB2
            , T.hoverBorderKiwi
            ]
        , safeHref route
        , HE.onClick $ onNavigate route
        ]
        [ HH.text name ]

      where isCurrent = isRoute route

    nav =
      HH.nav
        [ HP.classes
            [ T.relative
            , T.mxAuto
            , T.flex
            , T.itemsCenter
            , T.justifyBetween
            ]
        ]
        [ HH.div
            [ HP.classes [ T.flex, T.itemsCenter, T.flex1 ] ]
            [ HH.div
                [ HP.classes [ T.flex, T.itemsCenter, T.justifyBetween, T.wFull, T.mdWAuto ] ]
                [ logo
                , HH.div
                    [ HP.classes [ T.flex, T.itemsCenter, T.mdHidden ] ]
                    [ HH.button
                        [ HP.classes
                            [ T.bgKiwi
                            , T.roundedMd
                            , T.p2
                            , T.inlineFlex
                            , T.itemsCenter
                            , T.justifyCenter
                            , T.textWhite
                            , T.hoverBgKiwiDark
                            , T.focusOutlineNone
                            , T.focusRing2
                            , T.focusRingInset
                            , T.focusRingWhite
                            ]
                        , HP.type_ HP.ButtonButton
                        , HE.onClick $ \_ -> Just ToggleMenu
                        ]
                        [ HH.span
                            [ HP.classes [ T.srOnly ] ]
                            [ HH.text "Open main menu" ]
                        , Icons.menu [ Icons.classes [ T.h6, T.w6 ] ]
                        ]
                    ]
                ]
            , HH.div
                [ HP.classes [ T.hidden, T.spaceX10, T.mdFlex, T.mdMl10 ] ]
                case currentUser of
                  Just _ ->
                    [ desktopLink Dashboard "Up next"
                    , desktopLink Resources "Resources"
                    , desktopLink Discover "Discover"
                    , desktopLink Pricing "Pricing"
                    , desktopLink About "About"
                    ]

                  Nothing ->
                    [ desktopLink Discover "Discover"
                    , desktopLink Pricing "Pricing"
                    , desktopLink About "About"
                    ]
            ]
          , case authStatus of
              ShowLoading -> HH.text ""

              ShowUser {name} ->
                HH.div
                  [ HP.classes [ T.hidden, T.mdFlex ] ]
                  [ HH.a
                      [ HP.classes
                          [ T.inlineFlex
                          , T.itemsCenter
                          , T.py1
                          , T.fontMedium
                          , cx T.textGray300 $ not $ isRoute Settings
                          , cx T.textGray400 $ isRoute Settings
                          , T.flex
                          , T.itemsCenter
                          , T.group
                          ]
                      , safeHref Settings
                      , HE.onClick $ onNavigate Settings
                      ]
                      [ HH.span
                          [ HP.classes
                              [ T.borderB2
                              , cx T.borderTransparent $ not $ isRoute Settings
                              , cx T.borderKiwi $ isRoute Settings
                              , T.groupHoverBorderKiwi
                              , T.mr2
                              ]
                          ]
                          [ HH.text $ Username.toString name ]
                      , HH.div
                          [ HP.classes
                              [ T.w10
                              , T.h10
                              , T.roundedFull
                              , T.bgGray100
                              , T.flex
                              , T.justifyCenter
                              , T.itemsCenter
                              ]
                          ]
                          [ Icons.userCircle [ Icons.classes [ T.w8, T.h8 ] ] ]
                      ]
                  ]

              ShowAuth ->
                HH.div
                  [ HP.classes [ T.hidden, T.mdFlex, T.itemsCenter ] ]
                  [ HH.a
                      [ HP.classes
                          [ T.inlineFlex
                          , T.itemsCenter
                          , T.px4
                          , T.py1
                          , T.mr8
                          , T.border
                          , T.borderTransparent
                          , T.textSm
                          , T.fontMedium
                          , T.roundedMd
                          , T.textWhite
                          , T.bgKiwi
                          , T.hoverBgKiwiDark
                          , T.focusRing2
                          , T.focusRingKiwi
                          , T.focusRingOffset2
                          , T.focusOutlineNone
                          ]
                      , safeHref Register
                      , HE.onClick $ onNavigate Register
                      ]
                      [ HH.text "Try for free" ]
                  , desktopLink Login "Sign in"
                  ]
        ]

    mobileNav =
      whenElem menuOpen \_ ->
        HH.div
          [ HP.classes
              [ T.absolute
              , T.top0
              , T.insetX0
              , T.p2
              , T.transition
              , T.transform
              , T.originTopRight
              , T.mdHidden
              ]
          ]
          [ HH.div
              [ HP.classes
                  [ T.roundedLg
                  , T.shadowMd
                  , T.bgWhite
                  , T.ring1
                  , T.ringBlack
                  , T.ringOpacity5
                  , T.overflowHidden
                  ]
              ]
              [ HH.div
                  [ HP.classes [ T.px5, T.pt4, T.flex, T.itemsCenter, T.justifyBetween ] ]
                  [ HH.div [] [ logo ]
                  , HH.div
                      [ HP.classes [ T.negMr2 ] ]
                      [ HH.button
                          [ HP.classes
                              [ T.bgWhite
                              , T.roundedMd
                              , T.p2
                              , T.inlineFlex
                              , T.itemsCenter
                              , T.justifyCenter
                              , T.textGray400
                              , T.hoverBgGray100
                              , T.focusOutlineNone
                              , T.focusRing2
                              , T.focusRingInset
                              , T.focusRingKiwi
                              ]
                          , HP.type_ HP.ButtonButton
                          , HE.onClick $ \_ -> Just ToggleMenu
                          ]
                          [ HH.span
                              [ HP.classes [ T.srOnly ] ]
                              [ HH.text "Close menu" ]
                          , Icons.x
                              [ Icons.classes [ T.h6, T.w6 ] ]
                          ]
                      ]
                  ]
              , HH.div
                  [ HP.classes [ T.px2, T.pt2, T.pb3, T.spaceY1 ] ]
                  case currentUser of
                    Just _ ->
                      [ mobileLink Dashboard "Up next"
                      , mobileLink Resources "Resources"
                      , mobileLink Discover "Discover"
                      , mobileLink Pricing "Pricing"
                      , mobileLink About "About"
                      ]

                    Nothing ->
                      [ mobileLink Discover "Discover"
                      , mobileLink Pricing "Pricing"
                      , mobileLink About "About"
                      ]
              , case authStatus of
                  ShowUser _ -> HH.text ""
                  ShowLoading -> HH.text ""

                  _ ->
                    HH.div
                      [ HP.classes [ T.py4, T.px5 ] ]
                      [ HH.a
                          [ HP.classes
                              [ T.wFull
                              , T.flex
                              , T.itemsCenter
                              , T.justifyCenter
                              , T.px4
                              , T.py2
                              , T.textCenter
                              , T.fontMedium
                              , T.textWhite
                              , T.bgKiwi
                              , T.hoverBgKiwiDark
                              , T.focusOutlineNone
                              , T.focusRing2
                              , T.focusRingKiwi
                              , T.focusRingOffset2
                              , T.roundedMd
                              ]
                          , safeHref Register
                          , HE.onClick $ onNavigateAndClose Register
                          ]
                          [ HH.text "Try for free" ]
                      ]
              , case authStatus of
                  ShowLoading -> HH.text ""

                  ShowUser {name} ->
                    HH.a
                      [ HP.classes
                          [ T.block
                          , T.wFull
                          , T.px5
                          , T.py3
                          , T.textCenter
                          , T.fontMedium
                          , T.bgGray50
                          , T.hoverBgGray100
                          , cx T.textGray300 $ not $ isRoute Settings
                          , cx T.textGray400 $ isRoute Settings
                          ]
                      , safeHref Settings
                      , HE.onClick $ onNavigateAndClose Settings
                      ]
                      [ HH.span
                          [ HP.classes [ T.py1, T.borderB2, T.borderKiwi ] ]
                          [ HH.text $ Username.toString name ]
                      ]

                  ShowAuth ->
                    HH.a
                      [ HP.classes
                          [ T.block
                          , T.wFull
                          , T.px5
                          , T.py3
                          , T.textCenter
                          , T.fontMedium
                          , T.textKiwiDark
                          , T.bgGray50
                          , T.hoverBgGray100
                          ]
                      , safeHref Login
                      , HE.onClick $ onNavigateAndClose Login
                      ]
                      [ HH.text "Sign in" ]
              ]
          ]