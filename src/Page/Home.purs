module Listasio.Page.Home where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Lens (over, set)
import Data.Maybe (Maybe(..), isJust, maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Store.Select (selectEq)
import Listasio.Capability.Analytics (class Analytics)
import Listasio.Capability.Navigate (class Navigate, logout, navigate_)
import Listasio.Capability.Resource.User (class ManageUser, getCurrentUser)
import Listasio.Component.HTML.Footer (footer)
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Login as Login
import Listasio.Component.HTML.Logo as Logo
import Listasio.Component.HTML.Register as Register
import Listasio.Component.HTML.Typed as Typed
import Listasio.Component.HTML.Utils (safeHref, whenElem)
import Listasio.Data.Avatar as Avatar
import Listasio.Data.Lens (_mobileMenuOpen, _showSettingsMenu)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Route (Route(..))
import Listasio.Data.Username as Username
import Listasio.Store as Store
import Svg.Parser.Halogen (icon)
import Tailwind as T
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent as Mouse

_slot :: Proxy "home"
_slot = Proxy

data Form
  = ShowLoading
  | ShowRegister
  | ShowLogin
  | ShowUser ProfileWithIdAndEmail

hasUser :: Form -> Boolean
hasUser (ShowUser _) = true
hasUser _ = false

derive instance eqForm :: Eq Form

type ChildSlots
  = ( register :: Register.Slot
    , login :: Login.Slot
    , typed :: Typed.Slot Unit
    )

data Action
  = Initialize
  | GetCurrentUser
  | Receive (Connected (Maybe ProfileWithIdAndEmail) Unit)
  | Navigate Route Event
  | GoToSignin Register.Output
  | GoToRegister Login.Output
  | AndClose Action
  | ToggleMobileMenu
  | ToggleSettingsMenu
  | Logout

type State
  = { currentUser :: Maybe ProfileWithIdAndEmail
    , mobileMenuOpen :: Boolean
    , showSettingsMenu :: Boolean
    , authStatus :: Form
    }

component
  :: forall q o m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => ManageUser m
  => Navigate m
  => Analytics m
  => H.Component q Unit o m
component = connect (selectEq _.currentUser) $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      , initialize = Just Initialize
      }
  }
  where
  initialState {context: currentUser} =
    { currentUser
    , mobileMenuOpen: false
    , showSettingsMenu: false
    , authStatus: maybe ShowRegister ShowUser currentUser
    }

  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      st <- H.get
      when (isJust st.currentUser) $ void $ H.fork $ handleAction GetCurrentUser

    Receive {context: currentUser} -> do
      prev <- H.get

      H.modify_ _ {authStatus = maybe ShowRegister ShowUser currentUser, currentUser = currentUser}

      case prev.currentUser, currentUser of
        Nothing, Just _ -> void $ H.fork $ handleAction GetCurrentUser
        _, _ -> pure unit

    GetCurrentUser -> do
      user <- getCurrentUser
      updateStore $ maybe Store.LogoutUser Store.LoginUser user

    Navigate route e -> navigate_ e route

    GoToSignin Register.GoToSignin -> do
       {authStatus} <- H.get
       when (authStatus == ShowRegister) do
          H.modify_ _ {authStatus = ShowLogin}

    GoToRegister Login.GoToRegister -> do
       {authStatus} <- H.get
       when (authStatus == ShowLogin) do
          H.modify_ _ {authStatus = ShowRegister}

    Logout -> logout

    ToggleMobileMenu -> H.modify_ $ over _mobileMenuOpen not

    ToggleSettingsMenu -> H.modify_ $ over _showSettingsMenu not

    AndClose a -> do
      H.modify_
        $ set _mobileMenuOpen false
            <<< set _showSettingsMenu false
      handleAction a

  render :: State -> H.ComponentHTML Action ChildSlots m
  render {currentUser, mobileMenuOpen, authStatus, showSettingsMenu} =
    HH.div
      [ HP.classes [ T.bgWhite ] ]
      [ heroAndNav
      , whenElem false \_ -> splitImageAndFeature
      , readingListsFeatureCard
      , whenElem false \_ -> discoveryFeatureCard
      , featuresList
      , footer Navigate
      ]

    where
    onNavigate route = Navigate route <<< Mouse.toEvent

    onNavigateAndClose route = AndClose <<< Navigate route <<< Mouse.toEvent

    logo = HH.a [ safeHref Home, HE.onClick $ onNavigate Home ] [ Logo.elem ]

    mobileLink route name =
      HH.a
        [ HP.classes
            [ T.block
            , T.px3
            , T.py2
            , T.roundedMd
            , T.textBase
            , T.fontMedium
            , T.textGray700
            , T.hoverTextGray900
            , T.hoverBgGray100
            ]
        , safeHref route
        , HE.onClick $ onNavigate route
        ]
        [ HH.text name ]

    desktopLink route name =
      HH.a
        [ HP.classes [ T.fontMedium, T.textGray300, T.hoverTextKiwi ]
        , safeHref route
        , HE.onClick $ onNavigate route
        ]
        [ HH.text name ]

    heroText =
      HH.div
        [ HP.classes
            [ T.px4
            , T.smPx6
            , T.lgPx8
            , T.smTextCenter
            , T.mdMaxW2xl
            , T.mdMxAuto
            , T.lgColSpan6
            , T.lgTextLeft
            , T.lgFlex
            , T.lgItemsCenter
            ]
        ]
        [ HH.div
            []
            [ HH.h1
                [ HP.classes
                    [ T.mt4
                    , T.trackingTight
                    , T.fontExtrabold
                    , T.smMt5
                    , T.smLeadingNone
                    , T.lgMt6
                    , T.text4xl
                    , T.lgText5xl
                    ]
                ]
                [ HH.span
                    [ HP.classes [ T.textGray400, T.block ] ]
                    [ HH.text "Create, manage, share"
                    ]
                , HH.span
                    [ HP.classes [ T.textKiwi, T.block ] ]
                    [ HH.text " your "
                    , HH.slot Typed._slot unit Typed.component {words: NEA.cons' "reading" ["watching", "listening"]} absurd
                    , HH.text " lists"
                    ]
                ]
            , HH.p
                [ HP.classes
                    [ T.mt3
                    , T.textBase
                    , T.textGray400
                    , T.smMt5
                    , T.smTextXl
                    , T.lgTextLg
                    , T.xlTextXl
                    ]
                ]
                [ HH.text "Your reading lists under control. Keep a reference and stats of any article, podcast, or video you have consumed. Discover, copy, and follow content from others." ]
            ]
        ]

    nav =
      HH.nav
        [ HP.classes
            [ T.relative
            , T.maxW7xl
            , T.mxAuto
            , T.flex
            , T.itemsCenter
            , T.justifyBetween
            , T.px4
            , T.smPx6
            , T.lgPx8
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
                        , HE.onClick $ const ToggleMobileMenu
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
                [ whenElem (hasUser authStatus) \_ ->
                    desktopLink Dashboard "Up next"
                , whenElem (hasUser authStatus) \_ ->
                    desktopLink History "History"
                , desktopLink Discover "Discover"
                ]
            ]
          , case authStatus of
              ShowUser {slug, name} ->
                HH.div
                  [ HP.classes [ T.ml4, T.relative, T.flexShrink0 ] ]
                  [ HH.div
                      []
                      [ HH.div
                          [ HP.classes [ T.hidden, T.mdFlex ] ]
                          [ HH.button
                              [ HP.classes
                                  [ T.inlineFlex
                                  , T.itemsCenter
                                  , T.py1
                                  , T.fontMedium
                                  , T.textGray300
                                  , T.flex
                                  , T.itemsCenter
                                  , T.group
                                  , T.focusOutlineNone
                                  ]
                              , HE.onClick $ const ToggleSettingsMenu
                              ]
                              [ HH.span
                                  [ HP.classes
                                      [ T.borderB2
                                      , T.borderTransparent
                                      , T.groupHoverBorderKiwi
                                      , T.mr2
                                      ]
                                  ]
                                  [ HH.text $ Username.toString name ]
                              , Avatar.renderWithDefault Avatar.Sm $ _.avatar =<< currentUser
                              ]
                          ]

                      ]
                  , whenElem showSettingsMenu \_ ->
                      HH.div
                        [ HP.classes
                            [ T.originTopRight
                            , T.absolute
                            , T.right0
                            , T.mt1
                            , T.w48
                            , T.roundedMd
                            , T.shadowLg
                            , T.py1
                            , T.bgWhite
                            , T.ring1
                            , T.ringBlack
                            , T.ringOpacity5
                            , T.focusOutlineNone
                            , T.divideY2
                            , T.divideOpacity50
                            , T.divideGray100
                            ]
                        ]
                        [ HH.div
                            []
                            [ HH.a
                                [ HP.classes
                                    [ T.block
                                    , T.px4
                                    , T.py2
                                    , T.textSm
                                    , T.textGray700
                                    , T.hoverBgGray100
                                    , T.focusBgGray100
                                    , T.focusOutlineNone
                                    ]
                                , safeHref Settings
                                , HE.onClick $ onNavigateAndClose $ Profile slug
                                ]
                                [ HH.text "Profile" ]
                            , HH.a
                                [ HP.classes
                                    [ T.block
                                    , T.px4
                                    , T.py2
                                    , T.textSm
                                    , T.textGray700
                                    , T.hoverBgGray100
                                    , T.focusBgGray100
                                    , T.focusOutlineNone
                                    ]
                                , safeHref Settings
                                , HE.onClick $ onNavigateAndClose Settings
                                ]
                                [ HH.text "Settings" ]
                            ]
                        , HH.div
                            []
                            [ HH.button
                              [ HP.classes
                                  [ T.wFull
                                  , T.px4
                                  , T.py2
                                  , T.textSm
                                  , T.textGray700
                                  , T.hoverBgGray100
                                  , T.focusBgGray100
                                  , T.focusOutlineNone
                                  , T.textLeft
                                  , T.flex
                                  , T.itemsCenter
                                  ]
                              , HE.onClick $ const $ AndClose Logout
                              ]
                              [ HH.span [] [ HH.text "Log out" ]
                              , Icons.logout [ Icons.classes [ T.h5, T.w5, T.ml2 ] ]
                              ]
                            ]
                        ]
                  ]

              ShowLoading -> HH.text ""

              _ ->
                HH.div
                  [ HP.classes [ T.hidden, T.mdFlex, T.itemsCenter, T.gap8 ] ]
                  [ HH.a
                      [ HP.classes
                          [ T.inlineFlex
                          , T.itemsCenter
                          , T.px4
                          , T.py1
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
      whenElem mobileMenuOpen \_ ->
        HH.div
          [ HP.classes
              [ T.fixed
              , T.top0
              , T.insetX0
              , T.p2
              , T.transition
              , T.transform
              , T.originTopRight
              , T.mdHidden
              , T.z20
              , T.maxHFull
              , T.overflowYAuto
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
                          , HE.onClick $ const ToggleMobileMenu
                          ]
                          [ HH.span
                              [ HP.classes [ T.srOnly ] ]
                              [ HH.text "Close menu" ]
                          , Icons.x
                              [ Icons.classes [ T.h6, T.w6 ] ]
                          ]
                      ]
                  ]
              , case currentUser of
                  Just {slug} ->
                    HH.div
                      [ HP.classes [ T.spaceY1, T.divideY2, T.divideOpacity50, T.divideGray100 ] ]
                      [ HH.div
                          [ HP.classes [ T.px2, T.py4, T.spaceY1 ] ]
                          [ mobileLink Dashboard "Up next"
                          , mobileLink History "History"
                          , mobileLink Discover "Discover"
                          ]
                      , HH.div
                          [ HP.classes [ T.px2, T.py4 ] ]
                          [ HH.a
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
                                  , T.flex
                                  , T.itemsCenter
                                  ]
                              , safeHref $ Profile slug
                              , HE.onClick $ onNavigateAndClose $ Profile slug
                              ]
                              [ HH.text "Profile" ]
                          , HH.a
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
                                  , T.flex
                                  , T.itemsCenter
                                  ]
                              , safeHref Settings
                              , HE.onClick $ onNavigateAndClose Settings
                              ]
                              [ HH.text "Settings" ]
                          , HH.button
                              [ HP.classes
                                  [ T.block
                                  , T.wFull
                                  , T.px3
                                  , T.py2
                                  , T.roundedMd
                                  , T.textBase
                                  , T.fontMedium
                                  , T.textGray300
                                  , T.hoverTextGray400
                                  , T.hoverBgGray100
                                  , T.flex
                                  , T.itemsCenter
                                  ]
                              , HE.onClick $ const $ AndClose Logout
                              ]
                              [ HH.span [] [ HH.text "Log out" ]
                              , Icons.logout [ Icons.classes [ T.h5, T.w5, T.ml2 ] ]
                              ]
                          ]
                      ]

                  Nothing ->
                    HH.div
                      [ HP.classes [ T.px2, T.pt2, T.pb3, T.spaceY1 ] ]
                      [ mobileLink Discover "Discover"
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
                          , HE.onClick $ onNavigate Register
                          ]
                          [ HH.text "Try for free" ]
                      ]
              , case authStatus of
                  ShowUser {name, slug} ->
                    HH.a
                      [ HP.classes
                          [ T.block
                          , T.wFull
                          , T.px5
                          , T.py3
                          , T.textCenter
                          , T.fontMedium
                          , T.textGray300
                          , T.bgGray50
                          , T.hoverBgGray100
                          ]
                      , safeHref $ Profile slug
                      , HE.onClick $ onNavigateAndClose $ Profile slug
                      ]
                      [ HH.span
                          [ HP.classes [ T.py1, T.borderB2, T.borderKiwi ] ]
                          [ HH.text $ Username.toString name ]
                      ]

                  ShowLoading -> HH.text ""

                  _ ->
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
                      , HE.onClick $ onNavigate Login
                      ]
                      [ HH.text "Sign in" ]
              ]
          ]

    showUserBlock =
      HH.div
        [ HP.classes
            [ T.smMaxWMd
            , T.smWFull
            , T.smMxAuto
            , T.smRoundedLg
            , T.smOverflowHidden
            ]
        ]
        [ HH.div
            [ HP.classes [ T.flex, T.justifyCenter, T.p4 ] ]
            [ whenElem false \_ ->
                HH.a
                  [ HP.classes
                      [ T.inlineFlex
                      , T.itemsCenter
                      , T.justifyCenter
                      , T.px5
                      , T.px5
                      , T.py3
                      , T.border
                      , T.borderTransparent
                      , T.textBase
                      , T.fontMedium
                      , T.roundedMd
                      , T.textWhite
                      , T.bgKiwi
                      , T.hoverBgKiwiDark
                      ]
                  , safeHref Discover
                  , HE.onClick $ onNavigate Dashboard
                  ]
                  [ HH.text "Go to Dashboard" ]
          , research [ Icons.classes [ T.wFull, T.hFull ] ]
          ]
        ]

    authBlock =
      case authStatus of
        ShowLoading -> HH.text ""

        ShowUser _ ->
          showUserBlock

        ShowRegister ->
          HH.div
            [ HP.classes
                [ T.bgWhite
                , T.smMaxWMd
                , T.smWFull
                , T.smMxAuto
                , T.smRoundedLg
                , T.shadowLg
                , T.smOverflowHidden
                ]
            ]
            [ HH.div
                [ HP.classes [ T.px4, T.py8, T.smPx10 ] ]
                [ HH.slot Register._slot unit Register.component unit GoToSignin ]
            , HH.div
                [ HP.classes
                    [ T.px4
                    , T.py6
                    , T.bgWhite
                    , T.borderT2
                    , T.borderGray100
                    , T.smPx10
                    ]
                ]
                [ HH.p
                    [ HP.classes [ T.textXs, T.leading5, T.textGray500 ] ]
                    [ HH.text "By signing up, you agree to our "
                    , HH.a
                        [ HP.classes [ T.fontMedium, T.textGray900, T.hoverUnderline ]
                        , safeHref Terms
                        , HE.onClick $ onNavigate Terms
                        ]
                        [ HH.text "Terms" ]
                    , HH.text " and "
                    , HH.a
                        [ HP.classes [ T.fontMedium, T.textGray900, T.hoverUnderline ]
                        , safeHref Policy
                        , HE.onClick $ onNavigate Policy
                        ]
                        [ HH.text "Data Policy" ]
                    ]
                ]
            ]

        ShowLogin ->
          HH.div
            [ HP.classes
                [ T.bgWhite
                , T.smMaxWMd
                , T.smWFull
                , T.smMxAuto
                , T.smRoundedLg
                , T.shadowLg
                , T.smOverflowHidden
                ]
            ]
            [ HH.div
                [ HP.classes [ T.px4, T.py8, T.smPx10 ] ]
                [ HH.slot Login._slot unit Login.component {redirect: true} GoToRegister ]
            ]

    heroAndNav =
      HH.div
        [ HP.classes [ T.relative, T.bgGray10, T.overflowHidden ] ]
        [ HH.div
            [ HP.classes [ T.relative, T.pt6, T.pb16, T.smPb24 ] ]
            [ nav
            , mobileNav
            , HH.main
                [ HP.classes [ T.mt16, T.smMt24 ] ]
                [ HH.div
                    [ HP.classes [ T.mxAuto, T.maxW7xl ] ]
                    [ HH.div
                        [ HP.classes [ T.lgGrid, T.lgGridCols12, T.lgGap8 ] ]
                        [ heroText
                        , HH.div
                            [ HP.classes [ T.mt16, T.smMt24, T.lgMt0, T.lgColSpan6 ] ]
                            [ whenElem false \_ -> authBlock
                            , showUserBlock
                            ]
                        ]
                    ]
                ]
            ]
        ]

    feature icon title body soon =
      HH.div
        [ HP.classes [ T.pt6 ] ]
        [ HH.div
            [ HP.classes [ T.flowRoot, T.bgGray10, T.roundedLg, T.px6, T.pb8, T.hFull ] ]
            [ HH.div
                [ HP.classes [ T.negMt6 ] ]
                [ HH.div
                    [ HP.classes [ T.mb8 ] ]
                    [ HH.span
                        [ HP.classes
                            [ T.inlineFlex
                            , T.itemsCenter
                            , T.justifyCenter
                            , T.p3
                            , T.bgDurazno
                            , T.roundedMd
                            , T.shadowLg
                            ]
                        ]
                        [ icon [ Icons.classes [ T.h6, T.w6, T.textWhite ] ] ]
                    ]
                , HH.div
                [ HP.classes [ T.relative, T.inlineBlock ] ]
                    [ HH.h3
                        [ HP.classes [ T.textLg, T.fontMedium, T.textGray400, T.trackingTight ] ]
                        [ HH.text title ]
                    , whenElem soon \_ ->
                        HH.div
                          [ HP.classes [ T.textXs, T.fontSemibold, T.textKiwi, T.absolute, T.negTop3, T.negRight6 ] ]
                          [ HH.text "SOON" ]
                    ]
                , HH.p
                    [ HP.classes [ T.mt5, T.textBase, T.textGray500 ] ]
                    [ HH.text body ]
                ]
            ]
        ]

    featuresList =
      HH.div
        [ HP.classes [ T.relative, T.bgWhite, T.py16, T.smPy24, T.lgPy32 ] ]
        [ HH.div
            [ HP.classes
                [ T.mxAuto
                , T.maxWMd
                , T.textCenter
                , T.smMaxW3xl
                , T.px4
                , T.smPx6
                , T.lgPx8
                , T.lgMaxW7xl
                ]
            ]
            [ HH.h2
                [ HP.classes [ T.textBase, T.fontSemibold, T.trackingWider, T.textDurazno, T.uppercase ] ]
                [ HH.text "Features" ]
            , HH.p
                [ HP.classes
                    [ T.mt2
                    , T.text3xl
                    , T.fontExtrabold
                    , T.textGray400
                    , T.trackingTight
                    , T.smText4xl
                    ]
                ]
                [ HH.text "Enhance the way you consume and share content" ]
            , HH.div
                [ HP.classes [ T.mt12 ] ]
                [ HH.div
                    [ HP.classes
                        [ T.grid
                        , T.gridCols1
                        , T.gap8
                        , T.smGridCols2
                        , T.lgGridCols3
                        , T.autoRowsFr
                        ]
                    ]
                    [ feature
                        Icons.bookmark
                        "History"
                        "Keep track of everything you consumed. You’ll never know when you’ll need it again."
                        false
                    , feature
                        Icons.search
                        "Full text search"
                        "Find any saved resource by their content, search more than just titles and tags."
                        true
                    , feature
                        Icons.rss
                        "RSS"
                        "Bring outside content automatically to Listas using the RSS integration."
                        false
                    , feature
                        Icons.duplicate
                        "Copy"
                        "Copy public lists content to read it yourself."
                        true
                    , feature
                        Icons.userAdd
                        "Subscribe"
                        "Subscribe to a list and get up to date content from its author."
                        true
                    , feature
                        Icons.hashtag
                        "Discover"
                        "Explore and discover lists and learning paths from others."
                        true
                    ]
                ]
            ]
        ]

    discoveryFeatureCard =
      HH.div
        [ HP.classes [ T.bgWhite ] ]
        [ HH.div
            [ HP.classes
                [ T.maxW7xl
                , T.mxAuto
                , T.py16
                , T.px4
                , T.smPx6
                , T.lgPx8
                ]
            ]
            [ HH.div
                [ HP.classes
                    [ T.bgKiwi
                    , T.roundedLg
                    , T.shadowXl
                    , T.overflowHidden
                    , T.lgGrid
                    , T.lgGridCols2
                    , T.lgGap4
                    ]
                ]
                [ HH.div
                    [ HP.classes
                        [ T.pt10
                        , T.pb12
                        , T.px6
                        , T.smPt16
                        , T.smPx16
                        , T.lgPy16
                        , T.lgPr0
                        , T.xlPy20
                        , T.xlPx20
                        ]
                    ]
                    [ HH.div
                        [ HP.classes [ T.lgSelfCenter ] ]
                        [ HH.h2
                            [ HP.classes [ T.text3xl, T.fontExtrabold, T.textGray400, T.smText4xl ] ]
                            [ HH.span
                                [ HP.classes [ T.block ] ]
                                [ HH.text "Discover and learning paths" ]
                            ]
                        , HH.p
                            [ HP.classes [ T.mt4, T.textLg, T.leading6, T.textWhite ] ]
                            [ HH.text "Want to put together a list that helped you grow in a specific area and share it with friends? Listas can help! Want to subscribe interesting reading Lists? Listas discover section might have what you need!" ]
                        , HH.p
                            [ HP.classes [ T.mt4, T.textLg, T.leading6, T.textWhite ] ]
                            [ HH.text "Learning paths allow users to create and copy or follow lists designed to be consumed in a specific sequential order laid-out by the author." ]
                        ]
                    ]
                , HH.div
                    [ HP.classes [ T.negMt6, T.aspectW5, T.aspectH3, T.mdAspectW2, T.mdAspectH1 ] ]
                    [ HH.img
                        [ HP.alt "Listas Up Next page"
                        , HP.src "https://i.imgur.com/N0uKvkd.png"
                        , HP.classes
                            [ T.transform
                            , T.translateX6
                            , T.translateY6
                            , T.roundedMd
                            , T.objectCover
                            , T.objectLeftTop
                            , T.smTranslateX16
                            , T.lgTranslateY20
                            ]
                        ]
                    ]
                ]
            ]
        ]

    readingListsFeatureCard =
      HH.div
        [ HP.classes [ T.bgWhite ] ]
        [ HH.div
            [ HP.classes
                [ T.maxW7xl
                , T.mxAuto
                , T.py16
                , T.px4
                , T.smPx6
                , T.lgPx8
                ]
            ]
            [ HH.div
                [ HP.classes
                    [ T.bgKiwi
                    , T.roundedLg
                    , T.shadowXl
                    , T.overflowHidden
                    , T.lgGrid
                    , T.lgGridCols2
                    , T.lgGap4
                    ]
                ]
                [ HH.div
                    [ HP.classes [ T.negMt6, T.aspectW5, T.aspectH3, T.mdAspectW2, T.mdAspectH1 ] ]
                    [ HH.img
                        [ HP.alt "Listas Up Next page"
                        , HP.src "https://i.imgur.com/N0uKvkd.png"
                        , HP.classes
                            [ T.transform
                            , T.roundedMd
                            , T.objectCover
                            , T.objectCenter
                            , T.negTranslateX6
                            , T.negTranslateY6
                            , T.smNegTranslateX6
                            , T.lgNegTranslateY20
                            ]
                        ]
                    ]
                , HH.div
                    [ HP.classes
                        [ T.pt10
                        , T.pb12
                        , T.px6
                        , T.smPt16
                        , T.smPx16
                        , T.lgPy16
                        , T.lgPr16
                        , T.lgPl6
                        , T.xlPy20
                        , T.xlPr20
                        , T.xlPl10
                        ]
                    ]
                    [ HH.div
                        [ HP.classes [ T.lgSelfCenter ] ]
                        [ HH.h2
                            [ HP.classes [ T.text3xl, T.fontExtrabold, T.textGray400, T.smText4xl ] ]
                            [ HH.span
                                [ HP.classes [ T.block ] ]
                                [ HH.text "Manage your reading lists" ]
                            ]
                        , HH.p
                            [ HP.classes [ T.mt4, T.textLg, T.leading6, T.textWhite ] ]
                            [ HH.text "Listas lets you organize your reading material on different lists and focus only on what to read next." ]
                        , HH.p
                            [ HP.classes [ T.mt4, T.textLg, T.leading6, T.textWhite ] ]
                            [ HH.text "You'll have access to everything you read. Having trouble remembering that article you read last year? Find it on Listas. Want to measure how much you read every week? Listas shows you stats of your reading progress and habits." ]
                        ]
                    ]
                ]
            ]
        ]

    splitImageAndFeature =
      HH.div
        [ HP.classes [ T.relative, T.bgGray800 ] ]
        [ HH.div
            [ HP.classes
                [ T.h56
                , T.bgKiwiLight
                , T.smH72
                , T.mdAbsolute
                , T.mdLeft0
                , T.mdHFull
                , T.mdW1d2
                ]
            ]
            [ HH.img
                [ HP.alt "Woman reading on a laptop"
                  -- TODO: https://unsplash.com/photos/gm2qQPnSJBA
                , HP.src "https://images.unsplash.com/photo-1545239352-8cf6abca7cfd?ixlib=rb-1.2.1&ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&auto=format&fit=crop&w=675&q=80"
                , HP.classes [ T.wFull, T.hFull, T.objectCover ]
                ]
            ]
        , HH.div
            [ HP.classes
                [ T.relative
                , T.maxW7xl
                , T.mxAuto
                , T.px4
                , T.py12
                , T.smPx6
                , T.lgPx8
                , T.lgPy16
                ]
            ]
            [ HH.div
                [ HP.classes [ T.mdMlAuto, T.mdW1d2, T.mdPl10 ] ]
                [ HH.h2
                    [ HP.classes [ T.textBase, T.fontSemibold, T.uppercase, T.trackingWider, T.textGray300 ] ]
                    [ HH.text "Reading is best as a community" ]
                , HH.p
                    [ HP.classes
                        [ T.mt2
                        , T.textWhite
                        , T.text3xl
                        , T.fontExtrabold
                        , T.trackingTight
                        , T.smText4xl
                        ]
                    ]
                    [ HH.text "Find out what others are reading" ]
                , HH.p
                    [ HP.classes [ T.mt3, T.textLg, T.textGray300 ] ]
                    [ HH.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Et, egestas tempus tellus etiam sed. Quam a scelerisque amet ullamcorper eu enim et fermentum, augue. Aliquet amet volutpat quisque ut interdum tincidunt duis." ]
                , HH.div
                    [ HP.classes [ T.mt8 ] ]
                    [ HH.div
                        [ HP.classes [ T.inlineFlex, T.roundedMd, T.shadow ] ]
                        [ HH.a
                            [ HP.classes
                                [ T.inlineFlex
                                , T.itemsCenter
                                , T.justifyCenter
                                , T.px5
                                , T.py3
                                , T.border
                                , T.borderTransparent
                                , T.textBase
                                , T.fontMedium
                                , T.roundedMd
                                , T.textGray900
                                , T.bgWhite
                                , T.hoverBgGray50
                                ]
                            , safeHref Discover
                            , HE.onClick $ onNavigate Discover
                            ]
                            [ HH.text "Discover other lists" ]
                        ]
                    ]
                ]
            ]
        ]

research :: Icons.Icon
research = icon
  """
  <svg data-name="Layer 1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 802.027 590.916"><path fill="#e6e6e6" d="M464.906 218.808h337.121v9.889H464.906z"/><path fill="#e6e6e6" d="M492.774 136.101h85.404v85.404h-85.404z"/><path fill="#ccc" d="M509.855 153.182h51.242v51.242h-51.242z"/><path fill="#e6e6e6" d="M684.259 136.101h21.576v85.404h-21.576z"/><path fill="#ccc" d="M684.259 149.586h21.576v9.889h-21.576zM684.259 192.737h21.576v9.889h-21.576z"/><path fill="#e6e6e6" d="M650.997 136.101h21.576v85.404h-21.576z"/><path fill="#ccc" d="M650.997 149.586h21.576v9.889h-21.576zM650.997 192.737h21.576v9.889h-21.576z"/><path fill="#e6e6e6" d="M705.09 145.321l19.335-9.573 37.893 76.537-19.336 9.573z"/><path fill="#ccc" d="M711.072 157.405l19.336-9.573 4.388 8.862-19.336 9.573zM730.219 196.077l19.336-9.573 4.387 8.862-19.336 9.573z"/><path fill="#e6e6e6" d="M524.688 170.382l10.866 18.819 10.865 18.819h-43.461l10.865-18.819 10.865-18.819z"/><path fill="#e6e6e6" d="M543.567 173.978l10.865 18.819 10.866 18.819h-43.461l10.865-18.819 10.865-18.819z"/><circle cx="551.209" cy="162.171" r="6.293" fill="#e6e6e6"/><path fill="#e6e6e6" d="M802.027 92.95H464.906v-9.89h337.121z"/><path fill="#e6e6e6" d="M774.158 85.757h-85.404V.353h85.404z"/><path fill="#ccc" d="M757.078 68.676h-51.242V17.434h51.242z"/><path fill="#e6e6e6" d="M582.673 85.757h-21.576V.353h21.576z"/><path fill="#ccc" d="M582.673 23.727h-21.576v-9.89h21.576zM582.673 66.879h-21.576v-9.89h21.576z"/><path fill="#e6e6e6" d="M615.936 85.757H594.36V.353h21.576z"/><path fill="#ccc" d="M615.936 23.727H594.36v-9.89h21.576zM615.936 66.879H594.36v-9.89h21.576z"/><path fill="#e6e6e6" d="M523.95 86.11l-19.335-9.572L542.508 0l19.335 9.573z"/><path fill="#ccc" d="M551.473 30.52l-19.336-9.573 4.387-8.862 19.336 9.573zM532.326 69.192l-19.336-9.573 4.388-8.862 19.336 9.573z"/><path fill="#e6e6e6" d="M742.244 34.634l-10.865 18.819-10.865 18.819h43.461l-10.866-18.819-10.865-18.819z"/><path fill="#e6e6e6" d="M723.365 38.23L712.5 57.049l-10.865 18.819h43.461l-10.865-18.819-10.866-18.819z"/><circle cx="715.724" cy="26.424" r="6.293" fill="#e6e6e6"/><path fill="#e6e6e6" d="M802.027 364.445H464.906v-9.89h337.121z"/><path fill="#e6e6e6" d="M774.158 357.253h-85.404v-85.404h85.404z"/><path fill="#ccc" d="M757.078 340.172h-51.242V288.93h51.242z"/><path fill="#e6e6e6" d="M582.673 357.253h-21.576v-85.404h21.576z"/><path fill="#ccc" d="M582.673 295.222h-21.576v-9.889h21.576zM582.673 338.373h-21.576v-9.889h21.576z"/><path fill="#e6e6e6" d="M615.936 357.253H594.36v-85.404h21.576z"/><path fill="#ccc" d="M615.936 295.222H594.36v-9.889h21.576zM615.936 338.373H594.36v-9.889h21.576z"/><path fill="#e6e6e6" d="M523.95 357.606l-19.336-9.573 37.893-76.538 19.336 9.574z"/><path fill="#ccc" d="M551.472 302.015l-19.336-9.573 4.388-8.862 19.336 9.573zM532.327 340.686l-19.336-9.573 4.387-8.862 19.336 9.573z"/><path fill="#e6e6e6" d="M742.244 306.129l-10.865 18.819-10.865 18.82h43.461l-10.866-18.82-10.865-18.819z"/><path fill="#e6e6e6" d="M723.365 309.725L712.5 328.544l-10.865 18.82h43.461l-10.865-18.82-10.866-18.819z"/><circle cx="715.724" cy="297.919" r="6.293" fill="#e6e6e6"/><ellipse cx="397.129" cy="562.711" rx="397.129" ry="28.205" fill="#e6e6e6"/><path d="M707.387 578.506c-64.755-11.525-135.568-9.61-209.847 0 33.855-28.581 63.075-57.163 29.22-85.744 66.038 13.624 75.288 12.245 148.751 0-17.039 28.581 14.836 57.163 31.876 85.744z" fill="#89b0ae"/><path d="M707.387 578.506c-64.755-11.525-135.568-9.61-209.847 0 33.855-28.581 63.075-57.163 29.22-85.744 66.038 13.624 75.288 12.245 148.751 0-17.039 28.581 14.836 57.163 31.876 85.744z" opacity=".2"/><path d="M691.592 566.312c-55.007-9.2-115.16-7.67-178.257 0 28.759-22.814 53.58-45.628 24.82-68.442 56.098 10.875 63.956 9.774 126.36 0-14.474 22.814 12.603 45.628 27.077 68.442z" fill="#89b0ae"/><circle cx="670.64" cy="500.244" r="29.309" fill="#2f2e41"/><path fill="#a0616a" d="M537.591 466.249l20.308 3.385 10.154 29.333-29.333 18.052-33.847-50.77H537.591z"/><circle cx="595.13" cy="463.993" r="47.385" fill="#a0616a"/><path d="M333.386 291.377S218.308 303.787 198 301.53s-29.334-4.513-29.334-4.513-13.538 18.051-6.769 25.949a48.822 48.822 0 0013.538 11.282s12.41-3.385 21.436 0 100.411 18.051 116.206 4.513 20.308-47.385 20.308-47.385z" fill="#2f2e41"/><path d="M341.283 542.967s-75.59-21.436-89.128-116.205v-9.026s-14.957 26.239-20.308 31.59c-6.205 6.205-28.205 62.052-27.077 67.693s0 7.897 0 7.897l-32.718-3.384v-10.154s17.487-52.462 18.615-64.872 43.436-98.719 43.436-98.719 15.795-32.718 42.872 0 29.334 55.283 29.334 55.283l34.974 67.692zM172.052 293.633l-36.103-12.41s-33.846-24.82-25.949 0 44 78.975 54.154 73.334 22.366-19.644 19.645-21.668-20.773-16.691-11.747-39.256z" fill="#2f2e41"/><path d="M176.416 504.717l-29.144 14.233s-35.244 5.422-15.589 15.588 73.876 21.011 76.588 11.522 1.635-25.236-1.216-24.817-22.506 2.451-30.64-16.526z" fill="#2f2e41"/><path d="M326.616 468.506s-14.666 63.18 10.154 76.718 198.565 15.795 212.103-9.026 6.77-24.82 6.77-24.82l-36.103-40.616 29.333-2.256s0-9.026-10.153-10.154-41.744-12.41-68.821-4.513-42.872-15.795-42.872-15.795z" fill="#575a89"/><path fill="#3f3d56" d="M307.437 271.069l-19.18 104.924 132.001-15.795 23.692-94.77-136.513 5.641z"/><path fill="#fff" d="M311.386 275.018l-15.795 93.641 120.154-14.102 23.128-85.18-127.487 5.641z"/><path fill="#b3b3b3" d="M292.77 377.121l-4.513-1.128-2.256 4.512 36.103 100.411 3.395-1.298 1.117-6.599-33.846-95.898z"/><path fill="#d0cde1" d="M288.257 374.864l36.103 104.924 136.513-20.308-40.615-99.282-132.001 14.666z"/><path fill="#3f3d56" d="M306.309 371.48v5.641l100.41-11.282-1.128-5.641-99.282 11.282zM308.565 382.762l15.795 44 102.667-12.41-18.051-41.744-100.411 10.154z"/><path d="M437.18 444.813l-3.384-9.025s-25.949-45.129-39.487-36.103 28.205 53.026 28.205 53.026h13.539zM351.437 451.583l-3.385-13.539s-9.025-41.744 6.77-39.487 20.307 41.743 20.307 41.743l-1.128 9.026z" fill="#a0616a"/><path d="M378.514 445.942s-29.333-4.513-30.462 2.256-6.769 108.308 21.436 111.693 168.103 18.05 153.437-16.924-37.231-30.461-37.231-30.461l-95.898 10.154zM467.078 458.916l-29.897-20.872-19.744 15.231 24.821 27.077 24.82-21.436z" fill="#575a89"/><path opacity=".2" d="M386.976 479.224l4.512 42.872 77.847-4.513-67.693-1.128-14.666-37.231z"/><circle cx="618.521" cy="472.603" r="56.41" fill="#2f2e41"/><path fill="#89b0ae" d="M328.309 307.736l69.949-2.435 6.769-25.77-72.205 2.179-4.513 26.026z"/><path fill="#e6e6e6" d="M310.258 322.403l106.051-3.385 1.128-4.513-106.051 3.734-1.128 4.164zM308.001 333.685l106.052-3.385 1.128-4.513-106.052 3.734-1.128 4.164zM306.873 342.71l106.051-3.384 1.129-4.513-106.052 3.733-1.128 4.164z"/></svg>
  """
