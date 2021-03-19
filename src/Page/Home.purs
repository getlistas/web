module Listasio.Page.Home where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Lens (over)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Capability.Analytics (class Analytics)
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Capability.Resource.User (class ManageUser)
import Listasio.Component.HTML.Footer (footer)
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Login as Login
import Listasio.Component.HTML.Register as Register
import Listasio.Component.HTML.Utils (safeHref, whenElem)
import Listasio.Data.Lens (_menuOpen)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Route (Route(..))
import Listasio.Data.Username as Username
import Listasio.Env (UserEnv)
import Tailwind as T
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent as Mouse

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
    )

data Action
  = Initialize
  | Receive {currentUser :: Maybe ProfileWithIdAndEmail}
  | Navigate Route Event
  | ToggleMenu
  | GoToSignin Register.Output
  | GoToRegister Login.Output

type State
  = { currentUser :: Maybe ProfileWithIdAndEmail
    , menuOpen :: Boolean
    , authStatus :: Form
    }

component
  :: forall q o m r
   . MonadAff m
  => MonadAsk {userEnv :: UserEnv | r} m
  => ManageUser m
  => Navigate m
  => Analytics m
  => H.Component HH.HTML q {} o m
component = Connect.component $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      , initialize = Just Initialize
      }
  }
  where
  initialState {currentUser} =
    { currentUser
    , menuOpen: false
    , authStatus: maybe ShowLoading ShowUser currentUser
    }

  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> pure unit

    Receive {currentUser} -> do
     H.modify_ _ {authStatus = maybe ShowRegister ShowUser currentUser, currentUser = currentUser}

    Navigate route e -> navigate_ e route

    ToggleMenu -> H.modify_ $ over _menuOpen not

    GoToSignin Register.GoToSignin -> do
       {authStatus} <- H.get
       when (authStatus == ShowRegister) do
          H.modify_ _ {authStatus = ShowLogin}

    GoToRegister Login.GoToRegister -> do
       {authStatus} <- H.get
       when (authStatus == ShowLogin) do
          H.modify_ _ {authStatus = ShowRegister}

  render :: State -> H.ComponentHTML Action ChildSlots m
  render {currentUser, menuOpen, authStatus} =
    HH.div
      [ HP.classes [ T.bgWhite ] ]
      [ heroAndNav
      , if false then splitImageAndFeature else HH.text ""
      , featureCardWithImage
      , featuresList
      , footer Navigate
      ]

    where
    onNavigate route = Just <<< Navigate route <<< Mouse.toEvent

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
                    , T.text4xl
                    , T.trackingTight
                    , T.fontExtrabold
                    , T.smMt5
                    , T.smLeadingNone
                    , T.lgMt6
                    , T.lgText5xl
                    , T.xlText6xl
                    ]
                ]
                [ HH.span
                    [ HP.classes [ T.textGray400, T.block ] ]
                    [ HH.text "Your Reading, Watching, and Listening lists" ]
                , HH.span
                    [ HP.classes [ T.textKiwi, T.block ] ]
                    [ HH.text "Under control" ]
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
                [ HH.text "Store any digital content, keep the history of what you have consumed, share it with friends, discover trending material and create Learning Paths." ]
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
                [ whenElem (hasUser authStatus) \_ ->
                    desktopLink Dashboard "Up next"
                , whenElem (hasUser authStatus) \_ ->
                    desktopLink Resources "Resources"
                , desktopLink Discover "Discover"
                , desktopLink Pricing "Pricing"
                , desktopLink About "About"
                ]
            ]
          , case authStatus of
              ShowUser {name} ->
                HH.div
                  [ HP.classes [ T.hidden, T.mdFlex ] ]
                  [ HH.a
                      [ HP.classes
                          [ T.inlineFlex
                          , T.itemsCenter
                          , T.py1
                          , T.fontMedium
                          , T.flex
                          , T.itemsCenter
                          , T.group
                          ]
                      , safeHref Settings
                      , HE.onClick $ onNavigate Settings
                      ]
                      [ HH.span
                          [ HP.classes
                              [ T.textGray300
                              , T.borderB2
                              , T.borderKiwi
                              , T.mr2
                              , T.groupHoverTextGray400
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
                          [ Icons.userCircle
                              [ Icons.classes [ T.w8, T.h8, T.textGray300, T.groupHoverTextGray400 ] ]
                          ]
                      ]
                  ]

              ShowLoading -> HH.text ""

              _ ->
                HH.div
                  [ HP.classes [ T.hidden, T.mdFlex ] ]
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
                      , safeHref Login
                      , HE.onClick $ onNavigate Login
                      ]
                      [ HH.text "Sign in" ]
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
                  [ whenElem (hasUser authStatus) \_ ->
                      mobileLink Dashboard "Up next"
                  , whenElem (hasUser authStatus) \_ ->
                      mobileLink Resources "Resources"
                  , mobileLink Discover "Discover"
                  , mobileLink Pricing "Pricing"
                  , mobileLink About "About Us"
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
                  ShowUser {name} ->
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
                      , safeHref Settings
                      , HE.onClick $ onNavigate Settings
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

    authBlock =
      case authStatus of
        ShowLoading -> HH.text ""

        ShowUser _ ->
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
                [ HH.a
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
              ]
            ]
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
                [ HH.slot (SProxy :: _ "register") unit Register.component unit (Just <<< GoToSignin) ]
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
                [ HH.slot (SProxy :: _ "login") unit Login.component {redirect: true} (Just <<< GoToRegister) ]
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
                            [ authBlock
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
            [ HP.classes [ T.flowRoot, T.bgGray10, T.roundedLg, T.px6, T.pb8 ] ]
            [ HH.div
                [ HP.classes [ T.negMt6 ] ]
                [ HH.div
                    []
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
                , case soon of
                    true ->
                      HH.div
                        [ HP.classes [ T.mt8, T.flex, T.justifyCenter ] ]
                        [ HH.h3
                            [ HP.classes [ T.textLg, T.fontMedium, T.textGray400, T.trackingTight ] ]
                            [ HH.text title ]
                        , HH.span
                            [ HP.classes [ T.ml2, T.textXs, T.fontSemibold, T.textKiwi ] ]
                            [ HH.text "SOON" ]
                        ]
                    false ->
                      HH.h3
                        [ HP.classes [ T.mt8, T.textLg, T.fontMedium, T.textGray400, T.trackingTight ] ]
                        [ HH.text title ]
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
                    , T.textGray900
                    , T.trackingTight
                    , T.smText4xl
                    ]
                ]
                [ HH.text "Enhance the way you consume content" ]
            , HH.p
                [ HP.classes [ T.mt5, T.maxWProse, T.mxAuto, T.textXl, T.textGray500 ] ]
                [ HH.text "We are currently working on expanding and improving the Listas feature set. Reach us if you'd like a tailored feature." ]
            , HH.div
                [ HP.classes [ T.mt12 ] ]
                [ HH.div
                    [ HP.classes [ T.grid, T.gridCols1, T.gap8, T.smGridCols2, T.lgGridCols3 ] ]
                    [ feature
                        Icons.calendar
                        "Up next"
                        "Save your content to consume it in the order you want"
                        false
                    , feature
                        Icons.duplicate
                        "Fork lists"
                        "Copy interesting Lists or share your content with others allowing them to copy your public List"
                        false
                    , feature
                        Icons.userAdd
                        "Follow lists"
                        "Follow multiple lists, don't miss anything from the authors you like."
                        true
                    , feature
                        Icons.bookmark
                        "History"
                        "Keep track of everything you consumed, you’ll never know when you’ll need it again."
                        true
                    , feature
                        Icons.rss
                        "RSS"
                        "Bring outside content automatically to Listas using the RSS integration"
                        false
                    , feature
                        Icons.hashtag
                        "Discover"
                        "Explore other users' content and make your lists public to let other users check your content"
                        false
                    ]
                ]
            ]
        ]

    featureCardWithImage =
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
                            [ HP.classes [ T.text3xl, T.fontExtrabold, T.textWhite, T.smText4xl ] ]
                            [ HH.span
                                [ HP.classes [ T.block ] ]
                                [ HH.text "Ready to dive in?" ]
                            ]
                        , HH.p
                            [ HP.classes [ T.mt4, T.textLg, T.leading6, T.textGray400 ] ]
                            [ HH.text "Create an account for free (No credit card required) and start exploring. Listas aims to boost your productivity while keeping track of the content you consumed to be able to come back to it in the future, share it or just look at your stats." ]
                        , HH.p
                            [ HP.classes [ T.mt4, T.textLg, T.leading6, T.textGray400 ] ]
                            [ HH.text "Listas also tries to assist content creators or educators by allowing them to create “Learning Paths” lists so users / students can follow and consume content in the right order and get notified when new content arrives." ]
                        ]
                    ]
                , HH.div
                    [ HP.classes [ T.negMt6, T.aspectW5, T.aspectH3, T.mdAspectW2, T.mdAspectH1 ] ]
                    [ HH.img
                        [ HP.alt "App screenshot"
                        , HP.src "https://i.imgur.com/2UGt2Ko.png"
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
