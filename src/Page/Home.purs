module Listasio.Page.Home where

import Prelude

import Data.Array as A
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
import Listasio.Component.HTML.Utils (maybeElem, safeHref, whenElem)
import Listasio.Data.Avatar as Avatar
import Listasio.Data.Lens (_menuOpen)
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
  | ToggleMenu
  | GoToSignin Register.Output
  | GoToRegister Login.Output
  | NextDrawing
  | AndClose Action
  | Logout

type State
  = { currentUser :: Maybe ProfileWithIdAndEmail
    , menuOpen :: Boolean
    , authStatus :: Form
    , drawing :: Int
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
    , menuOpen: false
    , authStatus: maybe ShowRegister ShowUser currentUser
    , drawing: 0
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

    ToggleMenu -> H.modify_ $ over _menuOpen not

    GoToSignin Register.GoToSignin -> do
       {authStatus} <- H.get
       when (authStatus == ShowRegister) do
          H.modify_ _ {authStatus = ShowLogin}

    GoToRegister Login.GoToRegister -> do
       {authStatus} <- H.get
       when (authStatus == ShowLogin) do
          H.modify_ _ {authStatus = ShowRegister}

    NextDrawing -> H.modify_ \s -> s {drawing = s.drawing + 1}

    Logout -> logout

    AndClose a -> do
      H.modify_ $ set _menuOpen false
      handleAction a

  render :: State -> H.ComponentHTML Action ChildSlots m
  render {currentUser, menuOpen, authStatus, drawing} =
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
                        , HE.onClick $ const ToggleMenu
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
                      , Avatar.renderWithDefault Avatar.Sm $ _.avatar =<< currentUser
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
              , T.z10
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
                          , HE.onClick $ const ToggleMenu
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
                  Just _ ->
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
                          [ HH.button
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
          , let is =
                  [ Icons.reading
                  , chose
                  , done
                  , doneSingle
                  , noteList
                  , onlineArticle
                  , sharing
                  , takingNotes
                  , ideas
                  , organize
                  , reading
                  , openTabs
                  , research
                  , bookmark
                  ]
             in
              maybeElem (A.index is $ drawing `mod` A.length is) \icon ->
                icon
                  [ Icons.classes [ T.wFull, T.hFull ]
                  , HE.onClick \_ -> NextDrawing
                  ]
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
                        [ HP.alt "Listas dashboard screenshot"
                          -- Source https://imgur.com/a/V4mRN1f
                        , HP.src "https://i.imgur.com/IZs2gRv.png"
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

chose :: Icons.Icon
chose = icon
  """
  <svg data-name="Layer 1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 559.174 582.5"><path d="M512.854 498.347l-25.613 23.58 20.355-34.155c-16.037-29.082-42.265-54.216-42.265-54.216s-54.415 52.134-54.415 93.115 24.362 55.29 54.415 55.29c30.052 0 54.414-14.31 54.414-55.29 0-9.122-2.7-18.795-6.891-28.324z" fill="#f2f2f2"/><path d="M480.654 523.204v2.02c-.12 23.48-4.08 41.8-11.85 54.7-.11.19-.23.37-.34.56l-.87-.53-.83-.52c8.62-13.93 11.67-33.62 11.78-54.1.01-.66.02-1.33.01-2-.03-8.67-.56-17.44-1.41-25.96-.06-.66-.13-1.33-.2-2-1.18-11.24-2.88-21.98-4.62-31.31-.12-.66-.25-1.32-.38-1.97-3.01-15.78-6.08-27.21-6.78-29.74-.08-.31-.13-.48-.14-.52l.95-.27.01-.01.96-.27c.01.04.18.61.46 1.67 1.07 3.96 3.85 14.71 6.58 28.89.12.64.25 1.3.37 1.96 1.42 7.57 2.81 16.03 3.91 24.91q.42 3.345.75 6.6c.08.67.15 1.34.21 2q1.38 13.815 1.43 25.89z" fill="#fff"/><path d="M473.984 461.844c-.67.09-1.35.18-2.04.25a55.167 55.167 0 01-5.64.29 53.664 53.664 0 01-23.2-5.24c-.42.53-.84 1.06-1.27 1.6a55.65 55.65 0 0024.47 5.64 57.08 57.08 0 006.02-.32c.68-.07 1.36-.16 2.03-.26a55.182 55.182 0 0015.95-4.83q-.645-.825-1.26-1.62a53.48 53.48 0 01-15.06 4.49zM479.014 495.314q-1.035.06-2.07.06c-.21.01-.43.01-.64.01a53.948 53.948 0 01-44.28-23.13c-.4.59-.8 1.18-1.19 1.78a55.952 55.952 0 0045.47 23.35c.28 0 .56 0 .84-.01.7-.01 1.39-.03 2.08-.06a55.656 55.656 0 0029.65-10.4c-.32-.59-.64-1.18-.97-1.77a53.578 53.578 0 01-28.89 10.17z" fill="#fff"/><path d="M480.654 523.204a56.297 56.297 0 01-4.35.18 54.094 54.094 0 01-51.86-38.92c-.45.81-.9 1.61-1.33 2.42a56.075 56.075 0 0053.19 38.5c.75 0 1.5-.01 2.24-.05.71-.02 1.41-.06 2.11-.11a55.911 55.911 0 0036.9-18.01c-.21-.74-.45-1.47-.69-2.21a53.92 53.92 0 01-36.21 18.2z" fill="#fff"/><path data-name="Rectangle 1 - Outline" d="M420.747 125.949L142.158 90.43a18.086 18.086 0 00-2.263-.147 17.258 17.258 0 00-2.23.141 17.065 17.065 0 00-2.19.429 17.498 17.498 0 00-2.14.722 18.222 18.222 0 00-1.992.963 18.957 18.957 0 00-1.889 1.204 20.222 20.222 0 00-1.777 1.45 22.086 22.086 0 00-1.658 1.685 23.762 23.762 0 00-1.512 1.905q-.714 1.002-1.34 2.086-.626 1.087-1.162 2.26t-.977 2.427q-.458 1.295-.802 2.648t-.575 2.753q-.226 1.4-.347 2.844t-.12 2.924q.005 1.478.12 2.946t.347 2.913q.233 1.45.578 2.873t.801 2.81q.442 1.342.978 2.623.535 1.282 1.161 2.494t1.34 2.355q.713 1.147 1.512 2.208t1.659 2.023q.86.957 1.777 1.806t1.888 1.585a23.625 23.625 0 001.992 1.358 21.765 21.765 0 002.14 1.147 20.075 20.075 0 002.19.868 18.974 18.974 0 002.23.589 18.476 18.476 0 002.264.307l278.588 20.378a7.056 7.056 0 002.408-.24 7.552 7.552 0 001.161-.43 8.599 8.599 0 001.128-.631 10.035 10.035 0 001.043-.795 11.676 11.676 0 00.982-.963 13.852 13.852 0 00.918-1.127q.445-.604.854-1.288t.774-1.426q.365-.743.682-1.543.32-.8.59-1.654t.494-1.76q.235-.937.404-1.907t.29-1.97q.117-.998.175-2.024t.058-2.074q-.004-1.047-.058-2.085t-.175-2.06q-.116-1.021-.29-2.027t-.404-1.988q-.223-.951-.494-1.859-.268-.895-.59-1.764t-.682-1.686q-.364-.815-.774-1.581t-.854-1.46q-.443-.692-.918-1.311t-.982-1.16a13.498 13.498 0 00-1.043-1.005 11.325 11.325 0 00-1.128-.858 9.502 9.502 0 00-1.161-.662 8.275 8.275 0 00-1.192-.462 7.597 7.597 0 00-1.218-.26" fill="#89b0ae"/><path data-name="Rectangle 3 - Outline" d="M420.747 206.055l-278.589-6.673a17.91 17.91 0 00-4.493.459 18.344 18.344 0 00-2.19.656 19.418 19.418 0 00-2.14.939 20.721 20.721 0 00-1.992 1.165 21.902 21.902 0 00-1.889 1.402 23.58 23.58 0 00-1.777 1.635q-.86.876-1.658 1.864-.799.989-1.512 2.062-.714 1.072-1.34 2.224t-1.162 2.381q-.535 1.23-.977 2.528-.458 1.345-.802 2.732t-.575 2.812q-.227 1.424-.347 2.881t-.12 2.936q.004 1.48.12 2.935t.347 2.878q.233 1.416.578 2.801t.801 2.728q.442 1.302.978 2.528t1.161 2.373q.626 1.149 1.34 2.216.713 1.065 1.512 2.047t1.659 1.852a23.351 23.351 0 001.776 1.624 21.71 21.71 0 001.89 1.39 20.544 20.544 0 001.991 1.153 19.277 19.277 0 002.14.925 18.248 18.248 0 002.19.642 17.817 17.817 0 002.23.361 18.005 18.005 0 002.264.073l278.586-8.475a7.253 7.253 0 001.218-.141 7.574 7.574 0 001.192-.345 8.437 8.437 0 001.161-.55 9.87 9.87 0 001.128-.748 11.676 11.676 0 001.043-.903 13.635 13.635 0 00.982-1.064q.474-.568.918-1.222t.854-1.376q.41-.727.774-1.507t.682-1.613q.32-.846.59-1.722t.494-1.806q.234-.951.404-1.94t.29-1.999q.117-1.014.175-2.046t.058-2.08q-.004-1.039-.058-2.075t-.175-2.047q-.116-1.013-.29-1.997t-.404-1.946q-.224-.93-.495-1.81t-.589-1.712q-.316-.831-.681-1.61t-.774-1.5q-.41-.724-.853-1.373t-.919-1.217a13.505 13.505 0 00-.982-1.058 11.566 11.566 0 00-1.043-.896 9.784 9.784 0 00-1.127-.741 8.372 8.372 0 00-1.163-.54 7.536 7.536 0 00-1.192-.338 7.237 7.237 0 00-1.218-.132" fill="#e4e4e4"/><path data-name="Rectangle 4 - Outline" d="M420.747 281.153l-278.589 20.375a18.476 18.476 0 00-2.263.307 18.974 18.974 0 00-2.23.588 20.076 20.076 0 00-2.19.868 21.77 21.77 0 00-2.14 1.147 23.63 23.63 0 00-1.992 1.358q-.973.743-1.889 1.585-.916.847-1.777 1.806t-1.658 2.024q-.798 1.067-1.512 2.21-.714 1.142-1.34 2.354-.626 1.214-1.162 2.494t-.977 2.623q-.458 1.387-.802 2.811t-.575 2.869q-.226 1.447-.347 2.914t-.12 2.947q.005 1.48.12 2.924t.347 2.844q.233 1.404.578 2.754t.801 2.649q.442 1.254.978 2.426.535 1.174 1.161 2.26t1.34 2.087a23.758 23.758 0 001.512 1.904 22.084 22.084 0 001.659 1.686 20.224 20.224 0 001.777 1.45 18.96 18.96 0 001.888 1.204 18.222 18.222 0 001.992.963 17.492 17.492 0 002.14.722 17.066 17.066 0 002.19.429 17.26 17.26 0 002.23.14 18.092 18.092 0 002.264-.146l278.586-35.51a7.596 7.596 0 001.218-.259 8.272 8.272 0 001.192-.462 9.502 9.502 0 001.161-.662 11.325 11.325 0 001.128-.858 13.498 13.498 0 001.043-1.004q.5-.542.982-1.16.474-.62.918-1.313t.854-1.459q.41-.76.774-1.581t.682-1.685q.32-.868.59-1.776t.494-1.859q.235-.984.404-1.99t.29-2.027q.117-1.022.175-2.06t.058-2.085q-.005-1.043-.06-2.068t-.174-2.025q-.115-1.003-.289-1.973t-.404-1.907q-.223-.906-.494-1.76-.267-.853-.588-1.654t-.683-1.543q-.367-.736-.778-1.423t-.853-1.288a13.854 13.854 0 00-.918-1.126 11.677 11.677 0 00-.983-.963 10.034 10.034 0 00-1.042-.795 8.599 8.599 0 00-1.128-.632 7.553 7.553 0 00-1.162-.428 7.055 7.055 0 00-2.407-.241" fill="#e4e4e4"/><path data-name="Path 5" d="M268.9 284.886a13.972 13.972 0 0025.287-11.44l53.994-110.925-25.845-15.667-46.63 117.825q-.267.104-.526.218t-.517.238q-.257.124-.51.26t-.503.288a13.989 13.989 0 00-4.75 19.202z" fill="#9e616a"/><path data-name="Path 8" d="M337.874 205.295l-38.357-9.831s1.252-2.831 3.287-7.522 4.88-11.23 8.079-18.655 6.748-15.722 10.215-23.923 6.838-16.304 9.678-23.328 6.065-11.23 9.308-13.587a12.797 12.797 0 019.404-2.5 18.177 18.177 0 017.278 2.77 19.818 19.818 0 012.921 2.217l8.894 16.173z" fill="#ccc"/><path data-name="Ellipse 3 - Outline" d="M30.902 196.51a24.918 24.918 0 00-2.769.361 25.343 25.343 0 00-2.729.67 26.35 26.35 0 00-2.68.98 27.96 27.96 0 00-2.62 1.287q-1.248.702-2.44 1.527t-2.314 1.772q-1.122.945-2.178 2.015t-2.034 2.257q-.979 1.187-1.855 2.46t-1.645 2.62q-.76 1.353-1.423 2.778t-1.203 2.917q-.565 1.546-.989 3.126t-.706 3.185q-.284 1.606-.426 3.233t-.142 3.268q0 1.64.142 3.24t.426 3.147q.283 1.55.706 3.043t.985 2.927q.547 1.383 1.204 2.676a28.575 28.575 0 001.425 2.489 27.291 27.291 0 001.644 2.287 26.51 26.51 0 001.857 2.091 25.409 25.409 0 002.034 1.849 24.073 24.073 0 002.179 1.58 23.191 23.191 0 004.755 2.34 22.744 22.744 0 005.296 1.204 23.287 23.287 0 002.729.12 24.487 24.487 0 002.769-.195 25.37 25.37 0 002.754-.507 25.872 25.872 0 002.69-.808 26.948 26.948 0 002.618-1.104 28.6 28.6 0 002.538-1.394q1.2-.748 2.337-1.606t2.205-1.827q1.066-.97 2.06-2.046t1.908-2.258q.922-1.181 1.736-2.433t1.524-2.57q.71-1.318 1.315-2.698t1.101-2.817q.515-1.487.902-3t.645-3.048q.257-1.536.387-3.088t.13-3.117q0-1.564-.13-3.09t-.387-3.01q-.258-1.48-.645-2.918t-.902-2.82q-.5-1.339-1.101-2.597-.61-1.254-1.32-2.434a27.302 27.302 0 00-3.255-4.349 25.785 25.785 0 00-1.904-1.877 24.393 24.393 0 00-2.06-1.633 23.563 23.563 0 00-2.204-1.386 23.292 23.292 0 00-2.342-1.131 22.721 22.721 0 00-5.155-1.463 23.07 23.07 0 00-2.69-.268 24.145 24.145 0 00-2.754.046M30.902 309.663a24.918 24.918 0 00-2.769.36 25.345 25.345 0 00-2.729.67 26.353 26.353 0 00-2.68.98 27.96 27.96 0 00-2.62 1.288q-1.248.702-2.44 1.526t-2.314 1.772q-1.122.945-2.178 2.015t-2.034 2.257q-.979 1.187-1.855 2.46t-1.645 2.621q-.76 1.352-1.423 2.777t-1.203 2.917q-.565 1.547-.989 3.126t-.706 3.185q-.284 1.606-.426 3.233t-.142 3.269q0 1.64.142 3.239t.426 3.148q.283 1.549.706 3.043t.985 2.926q.547 1.383 1.204 2.676a28.575 28.575 0 001.425 2.49 27.291 27.291 0 001.644 2.286 26.51 26.51 0 001.857 2.091 25.409 25.409 0 002.034 1.85 24.077 24.077 0 002.179 1.58 23.192 23.192 0 004.755 2.34 22.743 22.743 0 005.296 1.204 23.287 23.287 0 002.729.12 24.49 24.49 0 002.769-.196 25.37 25.37 0 002.754-.507 25.867 25.867 0 002.69-.808 26.945 26.945 0 002.618-1.103 28.597 28.597 0 002.538-1.394q1.2-.75 2.337-1.606t2.205-1.827q1.066-.97 2.06-2.047t1.908-2.258q.922-1.181 1.736-2.433t1.524-2.57q.71-1.318 1.315-2.697t1.101-2.817q.515-1.488.902-3.001t.645-3.048q.257-1.536.387-3.088t.13-3.116q0-1.565-.13-3.091t-.387-3.01q-.258-1.48-.645-2.918t-.902-2.82q-.5-1.339-1.101-2.597-.61-1.254-1.32-2.434a27.302 27.302 0 00-3.255-4.349 25.789 25.789 0 00-1.904-1.876 24.395 24.395 0 00-2.06-1.634 23.563 23.563 0 00-2.204-1.385 23.292 23.292 0 00-2.342-1.132 22.72 22.72 0 00-5.155-1.462 23.068 23.068 0 00-2.69-.269 24.143 24.143 0 00-2.754.046" fill="#e4e4e4"/><path data-name="Ellipse 2 - Outline" d="M30.907 73.93a24.346 24.346 0 00-2.769.09 23.985 23.985 0 00-2.729.405 24.238 24.238 0 00-2.68.722 25.11 25.11 0 00-2.62 1.034 26.092 26.092 0 00-2.44 1.284 26.783 26.783 0 00-2.315 1.548 27.973 27.973 0 00-2.178 1.806q-1.06.974-2.038 2.065-.979 1.091-1.855 2.28t-1.645 2.462q-.764 1.292-1.426 2.647t-1.204 2.8q-.56 1.49-.985 3.028t-.706 3.116q-.284 1.583-.426 3.192t-.142 3.25q0 1.639.142 3.25t.426 3.19q.283 1.576.706 3.111t.985 3.022q.547 1.436 1.204 2.792.662 1.352 1.43 2.623t1.644 2.452q.875 1.185 1.855 2.268t2.035 2.046a27.71 27.71 0 002.167 1.799 26.543 26.543 0 002.315 1.532 25.88 25.88 0 002.44 1.269 24.942 24.942 0 002.62 1.017 23.982 23.982 0 005.41 1.084 24.49 24.49 0 005.52-.168 24.154 24.154 0 002.691-.546 24.502 24.502 0 002.618-.85 25.456 25.456 0 002.538-1.147 26.498 26.498 0 002.342-1.374 27.22 27.22 0 002.204-1.614q1.067-.867 2.06-1.847t1.91-2.072q.921-1.093 1.735-2.266t1.524-2.422q.713-1.25 1.315-2.57t1.101-2.705q.515-1.437.902-2.913t.645-2.985q.257-1.516.387-3.055t.13-3.105q0-1.563-.13-3.103t-.387-3.047q-.258-1.508-.645-2.982t-.902-2.909q-.5-1.385-1.101-2.702t-1.321-2.565q-.71-1.238-1.524-2.408t-1.73-2.253q-.91-1.082-1.904-2.057t-2.06-1.834a26.963 26.963 0 00-2.203-1.603 26.273 26.273 0 00-2.342-1.36 25.272 25.272 0 00-2.538-1.13 24.363 24.363 0 00-2.618-.834 24.06 24.06 0 00-2.69-.53 24.369 24.369 0 00-2.754-.22m0-3.013a26.747 26.747 0 0111.6 3.003 30.442 30.442 0 019.302 7.515 35.431 35.431 0 016.187 10.863 39.624 39.624 0 010 26.337 35.726 35.726 0 01-6.187 10.904 30.728 30.728 0 01-9.304 7.57 26.872 26.872 0 01-11.6 3.078 26.519 26.519 0 01-11.835-2.36 29.925 29.925 0 01-9.845-7.259 35.052 35.052 0 01-6.732-11.115 39.585 39.585 0 010-27.623A35.35 35.35 0 019.22 80.68a30.2 30.2 0 019.845-7.323 26.617 26.617 0 0111.843-2.435z" fill="#89b0ae"/><path data-name="Path 1" d="M30.904 114.074L21.422 94.32l2.251-1.466 7.81 16.228 32.793-38.544 1.985 2.115z" fill="#89b0ae"/><g data-name="Group 1"><path data-name="Path 1-2" d="M375.99 504.06l-23.473-.09 7.57 61.334 15.903.08z" fill="#9e616a"/><path data-name="Path 2" d="M380.046 580.817l-51.254-.271v-.648a20.102 20.102 0 01.383-3.918 19.598 19.598 0 012.955-7.12 20.124 20.124 0 015.544-5.516 19.61 19.61 0 017.134-2.924 20.11 20.11 0 013.92-.36l31.318.153z" fill="#2f2e41"/></g><g data-name="Group 2"><path data-name="Path 3" d="M300.832 503.77l-23.406-.092 7.545 61.251 15.86.08z" fill="#9e616a"/><path data-name="Path 4" d="M304.876 580.418l-51.118-.268v-.648a20.12 20.12 0 01.383-3.912 19.592 19.592 0 012.947-7.11 20.066 20.066 0 015.53-5.511 19.54 19.54 0 017.116-2.917 20.032 20.032 0 013.911-.361l31.231.153z" fill="#2f2e41"/></g><path data-name="Path 6" d="M299.837 253.08l-39.359 146.883 14.407 140.799 31.9.88-6.946-127.134 39.794-63.445 8.631 191.444 28.936-2.046 16.096-164.603a186.93 186.93 0 0011.954-40.024 178.217 178.217 0 002.827-32.7 152.93 152.93 0 00-1.697-22.048 80.712 80.712 0 00-1.59-8.085l-11.735-11.896z" fill="#2f2e41"/><path d="M351.985 277.646c-6.619 0-12.83-.213-18.461-.632a138.618 138.618 0 01-17.135-2.256 61.013 61.013 0 01-13.975-4.4 22.425 22.425 0 01-9.064-7.153l-.269-.39L303.84 192.8l11.602-78.721a21.455 21.455 0 0121.89-18.333l36.333 1.113a20.364 20.364 0 0119.702 21.592l-4.77 78.29 17.593 77.223-1.246.174c-.501.067-2.432.321-5.456.662-3.953.444-7.99.845-11.998 1.19-5.67.489-11.218.873-16.49 1.143a383.55 383.55 0 01-19.014.512z" fill="#ccc"/><path data-name="Path 9" d="M331.823 81.835h-.029a5.804 5.804 0 01-5.8-5.808V43.634a43.29 43.29 0 013.415-16.894A43.987 43.987 0 01352.52 3.507a43.314 43.314 0 0160.388 39.74v32.442a5.814 5.814 0 01-.46 2.272 5.91 5.91 0 01-3.113 3.12 5.833 5.833 0 01-2.273.469l-75.244.285z" fill="#2f2e41"/><path data-name="Ellipse 1" d="M389.074 32.525A31.93 31.93 0 01352.15 78.52a31.559 31.559 0 01-18.983-15.208 31.93 31.93 0 0136.897-45.983 31.559 31.559 0 0119.009 15.197z" fill="#9e616a"/><path data-name="Path 10" d="M315.72 46.27a34.244 34.244 0 012.7-13.361 34.774 34.774 0 0118.277-18.37 34.26 34.26 0 0113.362-2.768l6.483-.033h.219a34.186 34.186 0 0134.165 34.207v.65l-13.715.055-4.677-13.066-.935 13.09-7.084.032-2.358-6.592-.473 6.6-45.958.206z" fill="#2f2e41"/><path data-name="Path 11" d="M260.504 114.415a15.047 15.047 0 00-5.785 2.835 14.648 14.648 0 00-3.36 3.767 15.288 15.288 0 00-2.21 9.537 14.635 14.635 0 001.351 4.857 14.89 14.89 0 0021.381 6.27q.197-.127.39-.258t.38-.267q.188-.137.37-.278t.36-.286l75.682 26.936a11.406 11.406 0 0013.081-4.086l17.943-24.843-21.693-14.608-11.384 12.932-69.262-13.778q-.114-.269-.235-.535t-.254-.531q-.134-.263-.276-.524t-.301-.517a14.877 14.877 0 00-16.178-6.623z" fill="#9e616a"/><path data-name="Path 12" d="M348.231 128.768c2.264-2.826 5.43-6.494 8.646-10.133s6.48-7.244 8.941-9.942 4.119-4.486 4.119-4.486a17.834 17.834 0 0132.398 13.157 17.575 17.575 0 01-2.66 6.518s-2.469 3.231-6.196 8.054-8.713 11.242-13.743 17.625-10.11 12.732-14.025 17.418-6.673 7.715-7.06 7.456L344.99 133.83c-.388-.26.977-2.237 3.241-5.062z" fill="#ccc"/><path data-name="Path 13" d="M356.339 80.814a5.712 5.712 0 00.24 3.136 5.842 5.842 0 00.766 1.439 5.909 5.909 0 00.949 1.02 5.795 5.795 0 003.765 1.368q.129-.001.258-.007t.257-.018q.13-.011.26-.03t.258-.04l4.868-.885 6.705-14.325v13.121l22.132-3.984v-55.68l-37.103.179.917 1.065a25.52 25.52 0 015.778 12.557 41.6 41.6 0 01-.267 14.15 75.016 75.016 0 01-3.95 13.82 110.558 110.558 0 01-5.278 11.583 5.834 5.834 0 00-.555 1.531z" fill="#2f2e41"/><path d="M99.343 582.5H557.97a1.204 1.204 0 000-2.408H99.343a1.204 1.204 0 100 2.408z" fill="#cacaca"/></svg>
  """

done :: Icons.Icon
done = icon
  """
  <svg data-name="Layer 1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 811.76 620"><path d="M74.913 619.032l-.038.246a43.47 43.47 0 01-.13-.726c-.074-.39-.14-.78-.201-1.173a95.551 95.551 0 013.908-45.763q1.443-4.308 3.286-8.456a93.747 93.747 0 0119.648-28.71 40.444 40.444 0 019.113-6.992c.492-.26.988-.508 1.494-.736a16.671 16.671 0 019.998-1.449 13.636 13.636 0 013.56 1.195c.394.192.774.403 1.147.628a19.756 19.756 0 018.647 13.23 30.863 30.863 0 01-.435 12.03q-.12.566-.259 1.126a62.705 62.705 0 01-2.489 7.73c-9.969 25.65-31.392 48.407-57.25 57.82z" fill="#f2f2f2"/><path d="M125.484 526.55q-11.668 16.767-22.044 34.384-10.363 17.603-19.371 35.958-5.04 10.269-9.638 20.746c-.34.773.806 1.446 1.15.663q8.212-18.703 17.824-36.749 9.606-18.027 20.562-35.29 6.13-9.658 12.666-19.049c.488-.7-.663-1.36-1.149-.662z" fill="#fff"/><path d="M133.807 552.424a36.426 36.426 0 01-23.946 13.878 34.62 34.62 0 01-7.994.159c-.852-.081-.858 1.246-.013 1.326a37.75 37.75 0 0027.368-8.648 35.948 35.948 0 005.735-6.053c.511-.68-.642-1.338-1.15-.662zM83.883 593.632a30.495 30.495 0 01-3.364-6.22 32.17 32.17 0 01-2.067-15.796q1.443-4.308 3.286-8.456a.758.758 0 01.062.749 29.115 29.115 0 00-1.855 6.461 30.748 30.748 0 005.09 22.604.54.54 0 01.088.52.723.723 0 01-1.24.138zM110.697 526.618a30.873 30.873 0 001.36 19.18.667.667 0 00.905.247.68.68 0 00.247-.905 29.381 29.381 0 01-1.232-18.173.664.664 0 00-1.28-.349z" fill="#fff"/><path d="M74.75 586.997l.08.236c-.15-.191-.303-.388-.451-.583-.248-.31-.487-.626-.724-.945a95.551 95.551 0 01-17.797-42.342q-.723-4.484-1.018-9.015a93.747 93.747 0 014.064-34.55 40.444 40.444 0 014.821-10.425c.315-.46.64-.91.981-1.346a16.671 16.671 0 018.181-5.927 13.635 13.635 0 013.708-.596c.438-.013.873-.003 1.308.023a19.756 19.756 0 0113.802 7.7 30.863 30.863 0 015.203 10.855q.156.557.294 1.117a62.704 62.704 0 011.387 8.002c3.087 27.345-5.313 57.45-23.84 77.796z" fill="#f2f2f2"/><path d="M76.574 481.608q-2.544 20.268-3.549 40.689-1 20.402-.452 40.84.308 11.436 1.103 22.85c.058.842 1.385.906 1.325.053q-1.415-20.378-1.285-40.823.133-20.426 1.815-40.803.942-11.4 2.369-22.753c.106-.847-1.22-.896-1.326-.053z" fill="#fff"/><path d="M95.963 500.654a36.426 36.426 0 01-14.759 23.414 34.62 34.62 0 01-7.005 3.853c-.792.325-.18 1.503.604 1.181a37.75 37.75 0 0020.22-20.372 35.948 35.948 0 002.266-8.023c.137-.84-1.19-.888-1.326-.053zM70.895 560.337a30.496 30.496 0 01-5.87-3.946 32.17 32.17 0 01-9.167-13.028q-.723-4.484-1.018-9.015a.758.758 0 01.402.635 29.115 29.115 0 001.36 6.584 30.748 30.748 0 0015.007 17.652.54.54 0 01.32.42.723.723 0 01-1.034.698zM63.51 488.537a30.873 30.873 0 0010.114 16.353.667.667 0 00.916-.202.68.68 0 00-.201-.916 29.381 29.381 0 01-9.533-15.52.664.664 0 00-1.295.285z" fill="#fff"/><path d="M75.324 619.195l.194.155c-.23-.08-.464-.164-.694-.248a33.847 33.847 0 01-1.119-.406 95.551 95.551 0 01-37.799-26.092q-3.024-3.389-5.713-7.048a93.747 93.747 0 01-15.183-31.3 40.444 40.444 0 01-1.552-11.38c.018-.558.049-1.11.102-1.663a16.672 16.672 0 013.7-9.4 13.636 13.636 0 012.804-2.498c.362-.248.734-.473 1.114-.686a19.756 19.756 0 0115.776-.946 30.863 30.863 0 0110.231 6.345q.431.385.85.783a62.704 62.704 0 015.477 5.995c17.328 21.378 26.463 51.268 21.812 78.39z" fill="#f2f2f2"/><path d="M20.102 529.413q8.772 18.448 18.923 36.195 10.145 17.73 21.615 34.655 6.418 9.47 13.235 18.66c.503.677 1.655.017 1.145-.67q-12.167-16.408-23.069-33.705-10.889-17.282-20.445-35.357-5.347-10.113-10.258-20.447c-.367-.77-1.51-.099-1.146.67z" fill="#fff"/><path d="M46.697 535.019a36.426 36.426 0 01.174 27.676 34.62 34.62 0 01-3.828 7.02c-.492.7.658 1.363 1.146.67a37.75 37.75 0 006.065-28.055 35.948 35.948 0 00-2.412-7.98c-.337-.783-1.48-.108-1.145.669zM57.717 598.808a30.495 30.495 0 01-7.07-.164 32.17 32.17 0 01-14.741-6.04q-3.024-3.389-5.713-7.048a.758.758 0 01.681.319 29.116 29.116 0 004.692 4.815 30.748 30.748 0 0022.151 6.791.54.54 0 01.495.181.723.723 0 01-.495 1.146zM12.827 542.287a30.873 30.873 0 0017.329 8.331c.358.048.66-.339.663-.663a.68.68 0 00-.663-.663 29.381 29.381 0 01-16.391-7.943c-.615-.593-1.554.344-.938.938z" fill="#fff"/><path d="M165.861 444.249a10.743 10.743 0 001.582-16.397l4.167-93.018-21.215 2.382 1.232 90.984a10.8 10.8 0 0014.234 16.049zM204.564 601.708l12.075 2.122 11.934-45.563-15.821-3.133-8.188 46.574z" fill="#ffb8b8"/><path d="M237.954 619.136l-37.949-6.671 2.578-14.662 23.287 4.094a14.887 14.887 0 0112.084 17.24z" fill="#2f2e41"/><path fill="#ffb8b8" d="M171.422 607.752l12.26-.001 5.832-47.288-18.094.001.002 47.288z"/><path d="M207.326 619.134l-38.53.002-.002-14.887h23.644a14.887 14.887 0 0114.888 14.885zM223.574 594.803a4.75 4.75 0 01-.572-.034l-14.43-1.188a4.88 4.88 0 01-4.243-5.66l13.325-74.68-9.003-47.474a1.627 1.627 0 00-3.22.16l-11.255 126.627a4.924 4.924 0 01-5.21 4.438l-13.595-.507a4.888 4.888 0 01-4.536-4.63l-.914-151.768 70.481-8.81 4.924 76.04-.02.081-16.99 83.675a4.886 4.886 0 01-4.742 3.73z" fill="#2f2e41"/><circle cx="193.159" cy="249.997" r="24.561" fill="#ffb8b8"/><path d="M216.301 451.179a20.11 20.11 0 01-10.857-3.106c-11.897-7.436-25.41-4.48-32.407-2.057a4.88 4.88 0 01-4.22-.48 4.81 4.81 0 01-2.225-3.552L153.87 328.358c-2.132-19.038 9.336-36.937 27.268-42.56q1.01-.316 2.055-.6a39.569 39.569 0 0132.972 5.722 40.203 40.203 0 0117.167 29.353l10.71 114.387a4.807 4.807 0 01-1.527 4.007c-3.755 3.473-14.652 12.51-26.214 12.512z" fill="#89b0ae"/><path d="M177.56 352.724l-28.703-3.156a5.717 5.717 0 01-4.906-7.133l7.306-27.847a15.879 15.879 0 0131.557 3.563l1.084 28.676a5.717 5.717 0 01-6.338 5.897z" fill="#89b0ae"/><path d="M243.927 440.266a10.743 10.743 0 00-.405-16.468l-7.073-92.842-20.789 4.68 12.203 90.414a10.8 10.8 0 0016.064 14.216z" fill="#ffb8b8"/><path d="M207.69 346.888a5.711 5.711 0 01-1.818-4.4l1.084-28.675a15.879 15.879 0 0131.557-3.563l7.306 27.846a5.717 5.717 0 01-4.906 7.134l-28.703 3.156a5.711 5.711 0 01-4.52-1.498z" fill="#89b0ae"/><path d="M191.698 275.483a5.683 5.683 0 01-1.297-.151l-.125-.03c-21.594-3.304-26.366-15.812-27.414-21.036-1.084-5.408.15-10.628 2.94-12.655-1.52-4.803-1.277-9.061.728-12.663 3.495-6.28 11.08-8.403 12.098-8.663 6.058-4.47 13.306-1.486 14.625-.881 11.719-4.335 16.198-.727 17.008.08 5.238.94 8.431 2.963 9.491 6.015 1.991 5.73-4.305 12.86-4.574 13.16l-.14.156-9.38.447a6.358 6.358 0 00-5.981 7.317 29.604 29.604 0 00.96 3.355c1.602 5.007 2.802 9.283 1.254 10.91a2.51 2.51 0 01-2.625.455c-1.467-.392-2.462-.31-2.959.244-.77.86-.535 3.035.663 6.126a5.739 5.739 0 01-1.046 5.847 5.568 5.568 0 01-4.226 1.967z" fill="#2f2e41"/><path d="M481.76 354h-121a17.02 17.02 0 01-17-17V155.168a17.02 17.02 0 0117-17h121a17.02 17.02 0 0117 17V337a17.02 17.02 0 01-17 17zm-121-213.832a15.017 15.017 0 00-15 15V337a15.017 15.017 0 0015 15h121a15.017 15.017 0 0015-15V155.168a15.017 15.017 0 00-15-15z" fill="#3f3d56"/><path d="M463.26 225.584h-84a8.51 8.51 0 01-8.5-8.5v-40a8.51 8.51 0 018.5-8.5h84a8.51 8.51 0 018.5 8.5v40a8.51 8.51 0 01-8.5 8.5z" fill="#89b0ae"/><path d="M457.26 263.084h-72a8 8 0 110-16h72a8 8 0 010 16zM457.26 293.084h-72a8 8 0 110-16h72a8 8 0 010 16zM457.26 323.084h-72a8 8 0 110-16h72a8 8 0 010 16z" fill="#ccc"/><path d="M776.76 456h-121a17.02 17.02 0 01-17-17V257.168a17.02 17.02 0 0117-17h121a17.02 17.02 0 0117 17V439a17.02 17.02 0 01-17 17zm-121-213.832a15.017 15.017 0 00-15 15V439a15.017 15.017 0 0015 15h121a15.017 15.017 0 0015-15V257.168a15.017 15.017 0 00-15-15z" fill="#3f3d56"/><path d="M752.26 285.084h-72a8 8 0 110-16h72a8 8 0 010 16zM752.26 315.084h-72a8 8 0 110-16h72a8 8 0 010 16z" fill="#ccc"/><path d="M758.26 391.584h-84a8.51 8.51 0 01-8.5-8.5v-40a8.51 8.51 0 018.5-8.5h84a8.51 8.51 0 018.5 8.5v40a8.51 8.51 0 01-8.5 8.5z" fill="#e6e6e6"/><path d="M752.26 427.084h-72a8 8 0 110-16h72a8 8 0 010 16z" fill="#ccc"/><path d="M702.76 167h-121a17.02 17.02 0 01-17-17V37.168a17.02 17.02 0 0117-17h121a17.02 17.02 0 0117 17V150a17.02 17.02 0 01-17 17zm-121-144.832a15.017 15.017 0 00-15 15V150a15.017 15.017 0 0015 15h121a15.017 15.017 0 0015-15V37.168a15.017 15.017 0 00-15-15z" fill="#3f3d56"/><path d="M678.26 131.584h-72a8 8 0 110-16h72a8 8 0 010 16zM678.26 101.584h-72a8 8 0 110-16h72a8 8 0 010 16zM678.26 71.584h-72a8 8 0 110-16h72a8 8 0 010 16z" fill="#ccc"/><circle cx="488.76" cy="143" r="23" fill="#89b0ae"/><path d="M486.48 151.774a2.385 2.385 0 01-1.435-.477l-.026-.019-5.405-4.138a2.401 2.401 0 112.92-3.813l3.502 2.685 8.274-10.79a2.401 2.401 0 013.366-.445l-.051.07.053-.069a2.404 2.404 0 01.444 3.367l-9.731 12.692a2.402 2.402 0 01-1.911.937z" fill="#fff"/><circle cx="788.76" cy="246" r="23" fill="#3f3d56"/><path d="M786.48 254.774a2.385 2.385 0 01-1.435-.477l-.026-.019-5.405-4.138a2.401 2.401 0 112.92-3.813l3.502 2.685 8.274-10.79a2.401 2.401 0 013.366-.445l-.051.07.053-.069a2.404 2.404 0 01.444 3.367l-9.731 12.692a2.402 2.402 0 01-1.911.937z" fill="#fff"/><circle cx="715.76" cy="23" r="23" fill="#3f3d56"/><path d="M713.48 31.774a2.385 2.385 0 01-1.435-.477l-.026-.019-5.405-4.138a2.401 2.401 0 112.92-3.813l3.502 2.685 8.274-10.79a2.401 2.401 0 013.366-.445l-.051.07.053-.069a2.404 2.404 0 01.444 3.367l-9.731 12.692a2.402 2.402 0 01-1.911.937z" fill="#fff"/><path d="M382 620H1a1 1 0 010-2h381a1 1 0 110 2z" fill="#3f3d56"/></svg>
  """

doneSingle :: Icons.Icon
doneSingle = icon
  """
  <svg data-name="Layer 1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 680.839 584.232" height=""><path data-name="Path 438" d="M51.125 536.144a24.215 24.215 0 0023.383-4.119c8.19-6.874 10.758-18.196 12.846-28.681l6.18-31.017-12.938 8.908c-9.304 6.407-18.818 13.019-25.26 22.298s-9.252 21.947-4.078 31.988" fill="#e6e6e6"/><path data-name="Path 439" d="M53.123 575.855c-1.629-11.864-3.304-23.881-2.16-35.872 1.016-10.65 4.265-21.049 10.88-29.58a49.206 49.206 0 0112.624-11.44c1.262-.796 2.424 1.204 1.167 1.997a46.78 46.78 0 00-18.504 22.326c-4.029 10.246-4.676 21.416-3.982 32.3.42 6.582 1.31 13.121 2.206 19.653a1.198 1.198 0 01-.808 1.422 1.163 1.163 0 01-1.423-.808z" fill="#f2f2f2"/><path data-name="Path 442" d="M64.844 556.818a17.825 17.825 0 0015.531 8.019c7.865-.373 14.418-5.86 20.317-11.07l17.452-15.41-11.55-.552c-8.306-.398-16.826-.77-24.738 1.793s-15.207 8.727-16.654 16.916" fill="#e6e6e6"/><path data-name="Path 443" d="M48.52 582.674c7.84-13.87 16.932-29.287 33.18-34.215a37.026 37.026 0 0113.956-1.441c1.482.128 1.112 2.412-.367 2.284a34.398 34.398 0 00-22.272 5.893c-6.28 4.274-11.17 10.217-15.307 16.519-2.536 3.86-4.806 7.884-7.077 11.903-.725 1.284-2.847.357-2.113-.943z" fill="#f2f2f2"/><path data-name="Path 141" d="M675.815 411.433H243.6a5.03 5.03 0 01-5.023-5.024V5.024A5.03 5.03 0 01243.6 0h432.215a5.03 5.03 0 015.024 5.024v401.384a5.03 5.03 0 01-5.024 5.024z" fill="#fff"/><path data-name="Path 141" d="M675.815 411.433H243.6a5.03 5.03 0 01-5.023-5.024V5.024A5.03 5.03 0 01243.6 0h432.215a5.03 5.03 0 015.024 5.024v401.384a5.03 5.03 0 01-5.024 5.024zM243.6 2.005a3.018 3.018 0 00-3.011 3.012v401.391a3.018 3.018 0 003.011 3.012h432.215a3.017 3.017 0 003.012-3.012V5.024a3.018 3.018 0 00-3.012-3.012z" fill="#cacaca"/><path data-name="Path 142" d="M447.83 104.301a3.41 3.41 0 000 6.821h187.142a3.41 3.41 0 000-6.82z" fill="#e4e4e4"/><path data-name="Path 143" d="M447.83 124.766a3.41 3.41 0 000 6.821h95.54a3.41 3.41 0 000-6.82z" fill="#e4e4e4"/><path data-name="Path 142" d="M284.26 234.82a3.41 3.41 0 000 6.822h350.894a3.41 3.41 0 000-6.821z" fill="#e4e4e4"/><path data-name="Path 143" d="M284.26 255.286a3.41 3.41 0 000 6.82h259.292a3.41 3.41 0 000-6.82z" fill="#e4e4e4"/><path data-name="Path 142" d="M284.26 275.288a3.41 3.41 0 000 6.82h350.894a3.41 3.41 0 000-6.82z" fill="#e4e4e4"/><path data-name="Path 143" d="M284.26 295.753a3.41 3.41 0 000 6.821h259.292a3.41 3.41 0 000-6.821z" fill="#e4e4e4"/><path data-name="Path 142" d="M284.26 316.288a3.41 3.41 0 000 6.82h350.894a3.41 3.41 0 000-6.82z" fill="#e4e4e4"/><path data-name="Path 143" d="M284.26 336.753a3.41 3.41 0 000 6.821h259.292a3.41 3.41 0 000-6.821z" fill="#e4e4e4"/><path d="M339.839 166.944a49 49 0 1149-49 49.056 49.056 0 01-49 49z" fill="#89b0ae"/><path d="M191.098 352.216a12.248 12.248 0 00-14.953-11.362l-16.197-22.825-16.271 6.46 23.325 31.912a12.314 12.314 0 0024.096-4.185z" fill="#a0616a"/><path d="M159.531 350.525l-49.007-63.578 18.362-57.711c1.346-14.51 10.425-18.56 10.812-18.726l.59-.253 15.978 42.612-11.732 31.286 28.796 48.432z" fill="#3f3d56"/><path d="M329.727 154.536a12.248 12.248 0 00-10.172 15.787l-21.505 17.912 7.699 15.724 30.013-25.723a12.314 12.314 0 00-6.035-23.7z" fill="#a0616a"/><path d="M330.481 186.138l-59.598 53.777-58.958-13.846c-14.57-.22-19.312-8.958-19.506-9.33l-.298-.569 41.249-19.226 32.1 9.279 46.06-32.455z" fill="#3f3d56"/><path fill="#a0616a" d="M227.248 568.437l16.013-.001 7.617-61.764-23.633.001.003 61.764z"/><path d="M274.143 583.304l-50.326.002v-19.444l36.206-.002a14.12 14.12 0 0114.12 14.119v5.325z" fill="#2f2e41"/><path fill="#a0616a" d="M163.247 568.437l16.013-.001 7.618-61.764-23.633.001.002 61.764z"/><path d="M210.143 583.304l-50.326.002-.001-19.444 36.207-.002a14.12 14.12 0 0114.12 14.119v5.325zM157.552 342.991l1.306 91.429 1.307 120.164 28.734-2.612 14.368-165.878 18.286 165.878h29.665l2.988-167.184-10.449-36.572-86.205-5.225z" fill="#2f2e41"/><path d="M213.793 355.27c-31.265.002-60.044-14.15-60.433-14.343l-.322-.162-2.624-62.966c-.761-2.225-15.743-46.131-18.28-60.086-2.571-14.14 34.688-26.548 39.213-27.999l1.027-11.374 41.753-4.5 5.292 14.554 14.98 5.617a7.41 7.41 0 014.592 8.704l-8.326 33.857L251 348.584l-4.378.19c-10.493 4.739-21.817 6.495-32.828 6.495z" fill="#3f3d56"/><circle cx="454.467" cy="294.46" r="30.063" transform="rotate(-28.663 15.686 723.534)" fill="#a0616a"/><path d="M170.536 165.677c5.73 6.103 16.37 2.827 17.116-5.51a10.072 10.072 0 00-.013-1.946c-.385-3.693-2.519-7.046-2.008-10.945a5.74 5.74 0 011.05-2.687c4.566-6.114 15.283 2.734 19.592-2.8 2.642-3.394-.464-8.737 1.564-12.53 2.676-5.006 10.601-2.536 15.572-5.278 5.53-3.05 5.2-11.535 1.559-16.696-4.44-6.294-12.224-9.652-19.91-10.136s-15.321 1.594-22.498 4.39c-8.154 3.178-16.24 7.57-21.257 14.74-6.103 8.719-6.69 20.44-3.638 30.636 1.857 6.202 8.192 13.779 12.87 18.762z" fill="#2f2e41"/><path d="M382 584.079H1a1 1 0 010-2h381a1 1 0 010 2z" fill="#cacaca"/><path d="M337.01 136.451a3.488 3.488 0 01-2.382-.935l-16.157-15.007a3.5 3.5 0 014.763-5.13l13.686 12.713 27.076-27.077a3.5 3.5 0 114.95 4.95l-29.461 29.462a3.493 3.493 0 01-2.476 1.024z" fill="#fff"/></svg>
  """

noteList :: Icons.Icon
noteList = icon
  """
  <svg data-name="Layer 1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 790 701.992"><path d="M678.923 580.033c-13.33 39.042-30.472 78-52.433 115.079-.487.835-.98 1.653-1.477 2.485l-54.369-17.2c.313-.787.656-1.668 1.024-2.63 21.929-56.71 154.232-402.236 148.294-470.361.639 5.576 21.336 190.12-41.039 372.627z" fill="#e6e6e6"/><path d="M636.98 698.43a100.93 100.93 0 01-2.261 2.238l-40.786-12.904c.551-.667 1.193-1.453 1.932-2.341 12.079-14.752 47.991-58.919 83.058-105.39 37.686-49.94 74.403-102.533 79.548-124.144-1.033 4.914-33.216 154.51-121.492 242.54z" fill="#e6e6e6"/><path d="M657.367 123.114c-.016 0-.03.005-.045.005V35.69A35.69 35.69 0 00621.632 0h-273.72a35.69 35.69 0 00-35.69 35.69v44.82a2.892 2.892 0 00-2.83 2.89v22.2a2.891 2.891 0 002.83 2.888v23.257h-.145a2.895 2.895 0 00-2.895 2.895v42.93a2.895 2.895 0 002.895 2.894h.145v17.05h-.045a2.895 2.895 0 00-2.895 2.896v43.35a2.895 2.895 0 002.895 2.894h.045V665.68a35.69 35.69 0 0035.69 35.69h273.72a35.69 35.69 0 0035.69-35.69V208.13c.015 0 .03.004.045.004a5.155 5.155 0 005.155-5.155v-74.71a5.155 5.155 0 00-5.155-5.155z" fill="#3f3d56"/><path d="M636.012 45.68v610a27.41 27.41 0 01-27.41 27.4h-247.66a27.41 27.41 0 01-27.41-27.4v-610a27.41 27.41 0 0127.41-27.41h37.06v4.81a22.58 22.58 0 0022.57 22.57h126a22.57 22.57 0 0022.52-22.57v-4.76h39.51a27.41 27.41 0 0127.41 27.36z" fill="#fff"/><path d="M234.411 342.282a10.743 10.743 0 00-14.423 7.958l-88.254 47.988 15.517 17.464 81.96-52.599a10.8 10.8 0 005.2-20.811zM211.903 687.551l10.997-7.113-22.203-45.803-16.231 10.498 27.437 42.418z" fill="#ffb8b8"/><path d="M250.714 676.931l-34.563 22.356-8.637-13.354 21.708-14.041a15.31 15.31 0 0121.17 4.54l.322.5z" fill="#2f2e41"/><path fill="#ffb8b8" d="M88.511 689.206h13.097l6.233-50.518h-19.33v50.518z"/><path d="M126.867 701.367H85.705v-15.904h25.853a15.31 15.31 0 0115.31 15.31v.594z" fill="#2f2e41"/><path d="M389 98.992h-27a2.5 2.5 0 010-5h27a2.5 2.5 0 010 5zM389 105.992h-27a2.5 2.5 0 010-5h27a2.5 2.5 0 010 5zM389 112.992h-27a2.5 2.5 0 010-5h27a2.5 2.5 0 010 5zM598.292 106.042h-.91l-.35-.29a7.61 7.61 0 001.78-4.89 7.44 7.44 0 10-7.4 7.48 7.73 7.73 0 004.88-1.78l.34.29v.92l5.74 5.75 1.72-1.72zm-6.88 0a5.18 5.18 0 115.16-5.2v.02a5.15 5.15 0 01-5.12 5.18h-.04z" fill="#3f3d56"/><path d="M444.005 190.992H395.5a11 11 0 010-22h48.505a11 11 0 010 22z" fill="#89b0ae"/><path d="M592.5 214.992h-202a6 6 0 010-12h202a6 6 0 010 12zM464.7 238.992h-74.2a6 6 0 010-12h74.2a6 6 0 010 12z" fill="#e6e6e6"/><path d="M444.005 456.992H395.5a11 11 0 010-22h48.505a11 11 0 010 22z" fill="#89b0ae"/><path d="M592.5 480.992h-202a6 6 0 010-12h202a6 6 0 010 12zM464.7 504.992h-74.2a6 6 0 010-12h74.2a6 6 0 010 12z" fill="#e6e6e6"/><path d="M444.005 579.992H395.5a11 11 0 010-22h48.505a11 11 0 010 22z" fill="#89b0ae"/><path d="M592.5 603.992h-202a6 6 0 010-12h202a6 6 0 010 12zM464.7 627.992h-74.2a6 6 0 010-12h74.2a6 6 0 010 12z" fill="#e6e6e6"/><path d="M621 394.492H235a8.51 8.51 0 01-8.5-8.5v-105a8.51 8.51 0 018.5-8.5h386a8.51 8.51 0 018.5 8.5v105a8.51 8.51 0 01-8.5 8.5z" fill="#89b0ae"/><path d="M96.232 519.412c3.69 25.89-15.85 140.59-13.68 140.58l28.06-.15c1-1.92 13.88-63.81 17.92-73.74 5.64-13.78 11.33-27.73 13.55-42.45 2.09-13.82 1.06-27.9-.3-41.81a2.64 2.64 0 00-3.06-3l-37.65-1.94c-7.76-.4-9.6-2.1-8.42 5.52.82 5.69 2.76 11.23 3.58 16.99z" fill="#2f2e41"/><path d="M103.632 456.462c-7.65 11-15.67 23.66-13.13 36.83 1.93 10 9.55 17.75 16.74 25 28.74 28.86 85.41 156.2 85.41 156.2 27.348-17.5-.652.5 27.1-18-2.46-.19-58.35-201.1-62.98-201.37-18.73-1.24-34.89-3.07-53.14 1.34z" fill="#2f2e41"/><path d="M133.89 351.763c-3.886-3.465-31.192-14.151-31.192-14.151a4.684 4.684 0 00-1.509.045c-1.554.392-2.263 2.323-1.931 3.885s-2.391 27.224-3.093 38.894c-1.23 20.367 10.765 66.503 10.66 67.914a21.076 21.076 0 01-6.21 13.292c.935 1.16 60.266 3.485 64.302 4.858.814-1.034-4.594-74.213-4.843-78.347-.415-6.276-21.054-31.637-26.184-36.39z" fill="#e6e6e6"/><path d="M91.573 510.55a10.743 10.743 0 003.841-16.018l25.033-138.54-23.36-.308-17.384 136.998a10.8 10.8 0 0011.87 17.869z" fill="#ffb8b8"/><path d="M91.372 366.272a4.43 4.43 0 01-2.76-.72c-1.39-1.19 9.95-19.4 12.19-19.67 7.29-.84 14.79-1.12 21.86.85s13.72 6.45 16.88 13.08c1.32 2.76-8.25 16.65-12.6 14.61-5.21-2.44-28.71-7.98-35.57-8.15z" fill="#e6e6e6"/><circle cx="117.102" cy="309.88" r="26.239" fill="#ffb8b8"/><path d="M91.33 353.264a21.925 21.925 0 0023.085-28.726c-1.044-3.14-2.79-6.017-3.745-9.184s-.983-6.912 1.138-9.451c4.15-4.968 12.148-1.843 18.545-2.831 6.817-1.053 12.047-7.592 12.592-14.468s-3.038-13.68-8.373-18.052-12.216-6.524-19.08-7.199a51.594 51.594 0 00-52.955 70.496c2.868 7.175 7.552 13.858 14.206 17.788s15.441 4.685 22.073.718" fill="#2f2e41"/><path d="M372.277 320.492h-67.054c-4.258 0-7.723-4.934-7.723-11s3.465-11 7.723-11h67.054c4.259 0 7.723 4.935 7.723 11s-3.465 11-7.723 11z" fill="#fff"/><path d="M552.5 344.492h-249a6 6 0 010-12h249a6 6 0 010 12zM424.7 368.492H303.5a6 6 0 010-12h121.2a6 6 0 010 12z" fill="#e6e6e6"/><path fill="#3f3d56" d="M0 699.992h790v2H0z"/></svg>
  """

onlineArticle :: Icons.Icon
onlineArticle = icon
  """
  <svg data-name="Layer 1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 808.841 681.894"><path d="M670.666 550.26L402.32 427.757l138.175-302.67L808.84 247.59z" fill="#f2f2f2"/><path d="M711.544 374.425l-203.145-92.74a1.586 1.586 0 011.317-2.885l203.145 92.74a1.586 1.586 0 01-1.317 2.885zM722.84 349.68l-203.145-92.739a1.586 1.586 0 111.318-2.885l203.144 92.74a1.586 1.586 0 11-1.317 2.885zM734.136 324.937l-203.144-92.74a1.586 1.586 0 111.317-2.885l203.144 92.74a1.586 1.586 0 01-1.317 2.885zM745.432 300.192l-203.144-92.739a1.586 1.586 0 111.317-2.885l203.145 92.739a1.586 1.586 0 01-1.318 2.885zM701.335 396.787l-203.145-92.74a1.586 1.586 0 111.318-2.885l203.144 92.74a1.586 1.586 0 01-1.317 2.885zM735.258 263.895l-158.682-72.441a3.965 3.965 0 01-2.23-4.779 3.816 3.816 0 015.2-2.252l158.634 72.419a4 4 0 012.261 4.85 3.816 3.816 0 01-5.183 2.203z" fill="#fff"/><path d="M124.691 247.59l268.345-122.504 138.175 302.67-268.345 122.505z" fill="#f2f2f2"/><path d="M221.988 374.425l203.145-92.74a1.586 1.586 0 10-1.318-2.885l-203.144 92.74a1.586 1.586 0 101.317 2.885zM210.692 349.68l203.144-92.739a1.586 1.586 0 00-1.317-2.885l-203.145 92.74a1.586 1.586 0 101.318 2.885zM199.395 324.937l203.145-92.74a1.586 1.586 0 00-1.317-2.885l-203.145 92.74a1.586 1.586 0 001.317 2.885zM188.1 300.192l203.144-92.739a1.586 1.586 0 10-1.317-2.885l-203.145 92.739a1.586 1.586 0 001.317 2.885zM232.197 396.787l203.144-92.74a1.586 1.586 0 00-1.317-2.885l-203.145 92.74a1.586 1.586 0 101.318 2.885zM198.274 263.895l158.682-72.441a3.965 3.965 0 002.23-4.779 3.816 3.816 0 00-5.2-2.252l-158.634 72.419a4 4 0 00-2.261 4.85 3.816 3.816 0 005.183 2.203z" fill="#fff"/><path d="M26.825 604.978c1.694-26.244 16.317-52.87 40.66-62.823a121.17 121.17 0 004.933 83.044c4.43 10.101 10.41 20.874 7.436 31.495-1.85 6.61-7.022 11.888-12.948 15.349-5.927 3.46-12.62 5.34-19.231 7.184l-1.253 1.168c-12.055-23.373-21.291-49.173-19.597-75.417z" fill="#f0f0f0"/><path d="M67.76 542.633a103.567 103.567 0 00-22.249 59.71 44.6 44.6 0 001.334 13.91 25.58 25.58 0 007.053 11.467c3.05 2.975 6.513 5.665 8.785 9.33a15.663 15.663 0 011.522 12.718c-1.501 5.294-4.814 9.733-8.205 13.961-3.765 4.695-7.75 9.511-9.112 15.52-.165.728-1.285.43-1.12-.298 2.37-10.453 12-16.883 16.208-26.422 1.964-4.45 2.598-9.52.246-13.956-2.056-3.88-5.625-6.656-8.738-9.642a27.296 27.296 0 01-7.315-10.955 41.231 41.231 0 01-1.863-13.808 100.479 100.479 0 015.517-30.915 105.403 105.403 0 0117.12-31.443c.463-.583 1.277.243.818.823z" fill="#fff"/><path d="M45.407 595.322a15.538 15.538 0 01-12.77-15.548.58.58 0 011.159-.01 14.388 14.388 0 0011.909 14.438c.734.13.433 1.249-.298 1.12zM52.154 626.415a29.949 29.949 0 0012.329-18.01c.167-.728 1.287-.43 1.12.297a31.151 31.151 0 01-12.868 18.716c-.619.418-1.197-.587-.581-1.003zM54.834 562.905a8.795 8.795 0 008.295-.916c.613-.426 1.191.579.582 1.002a9.856 9.856 0 01-9.175 1.034.599.599 0 01-.411-.709.582.582 0 01.709-.411z" fill="#fff"/><path d="M145.98 584.194c-.376.277-.752.554-1.127.841a115.831 115.831 0 00-14.1 12.42c-.342.343-.684.697-1.015 1.05a122.112 122.112 0 00-24.324 37.81 118.587 118.587 0 00-5.49 17.161c-1.92 8.081-3.361 16.996-7.712 23.771a20.346 20.346 0 01-1.467 2.047l-42.801 2.54c-.1-.044-.2-.077-.301-.12l-1.704.18c.05-.307.11-.624.16-.931.029-.178.067-.357.096-.535.022-.119.045-.238.058-.346l.022-.11c.014-.108.037-.208.051-.306q.489-2.66 1.017-5.323c0-.01 0-.01.008-.02 2.717-13.518 6.586-26.929 12.367-39.325.174-.373.347-.756.54-1.13a113.158 113.158 0 019.234-15.992 100.037 100.037 0 016.199-8.002 83.187 83.187 0 0119.857-16.798c14.87-9.016 32.458-13.176 49.158-9.198.427.102.845.204 1.273.316z" fill="#f0f0f0"/><path d="M145.915 584.743a103.567 103.567 0 00-53.714 34.28 44.6 44.6 0 00-7.31 11.909 25.58 25.58 0 00-1.272 13.402c.643 4.211 1.788 8.444 1.397 12.739a15.663 15.663 0 01-6.443 11.07c-4.386 3.324-9.703 4.873-14.957 6.208-5.832 1.481-11.914 2.928-16.62 6.905-.57.482-1.284-.43-.714-.912 8.186-6.92 19.746-6.255 28.85-11.338 4.247-2.371 7.806-6.037 8.599-10.995.693-4.335-.485-8.701-1.173-12.96a27.296 27.296 0 01.756-13.15 41.231 41.231 0 016.825-12.147 100.479 100.479 0 0123.018-21.363 105.404 105.404 0 0132.6-14.797c.72-.187.873.962.158 1.149z" fill="#fff"/><path d="M96.345 613.354a15.538 15.538 0 01-.836-20.103c.46-.586 1.393.102.932.69a14.388 14.388 0 00.816 18.698c.508.545-.407 1.258-.913.715zM83.011 642.242a29.949 29.949 0 0020.688-6.958c.571-.48 1.286.433.715.913a31.151 31.151 0 01-21.542 7.195c-.746-.038-.603-1.189.14-1.15zM123.388 593.146a8.795 8.795 0 007.175 4.263c.747.029.603 1.18-.139 1.15a9.856 9.856 0 01-7.948-4.698.599.599 0 01.099-.814.582.582 0 01.813.099zM666.417 552.805H265.161V100.222h401.256z" fill="#fff"/><path d="M666.417 552.805H265.161V100.222h401.256zm-399.098-2.157H664.26V102.38H267.319z" fill="#cacaca"/><path d="M617.67 312.132H313.908a2.157 2.157 0 110-4.314H617.67a2.157 2.157 0 010 4.314zM617.67 275.132H313.908a2.157 2.157 0 110-4.314H617.67a2.157 2.157 0 010 4.314zM617.67 238.132H313.908a2.157 2.157 0 110-4.314H617.67a2.157 2.157 0 010 4.314zM617.67 201.132H313.908a2.157 2.157 0 110-4.314H617.67a2.157 2.157 0 010 4.314zM617.67 345.57H313.908a2.157 2.157 0 110-4.314H617.67a2.157 2.157 0 010 4.314zM617.67 511.57H313.908a2.157 2.157 0 110-4.314H617.67a2.157 2.157 0 010 4.314z" fill="#e4e4e4"/><path d="M544.905 476.94H386.674a3.993 3.993 0 01-3.99-3.989v-93.077a3.993 3.993 0 013.99-3.989h158.23a3.993 3.993 0 013.99 3.99v93.076a3.993 3.993 0 01-3.99 3.99z" fill="#fff"/><path d="M492.064 423.767a8.801 8.801 0 00-12.646-.226l-44.53 44.53a6.288 6.288 0 01-8.87.014l-6.594-6.582a8.78 8.78 0 00-12.42 0l-12.764 12.778h143.099v-1.821z" fill="#89b0ae"/><path d="M544.905 476.94H386.674a3.993 3.993 0 01-3.99-3.989v-93.077a3.993 3.993 0 013.99-3.989h158.23a3.993 3.993 0 013.99 3.99v93.076a3.993 3.993 0 01-3.99 3.99zm-158.231-98.395a1.331 1.331 0 00-1.33 1.33v93.077a1.331 1.331 0 001.33 1.33h158.23a1.331 1.331 0 001.33-1.33v-93.078a1.331 1.331 0 00-1.33-1.33z" fill="#3f3d56"/><path d="M163.438 461.382a10.056 10.056 0 00-8.672-12.75l-8.29-34.76-12.708 13.542 9.821 30.932a10.11 10.11 0 0019.849 3.036z" fill="#ffb6b6"/><path d="M141.276 448.775a4.505 4.505 0 01-2.708-2.476L120.99 403.3a46.373 46.373 0 01-1.648-32.602l11.264-35.283a14.497 14.497 0 1128.873 2.642l-14.939 56.815 10.006 44.622a4.515 4.515 0 01-2.2 4.68l-7.417 4.237a4.506 4.506 0 01-2.623.575 4.455 4.455 0 01-1.029-.212z" fill="#e4e4e4"/><path fill="#ffb6b6" d="M203.799 669.082h12.259l5.833-47.288h-18.095l.003 47.288z"/><path d="M200.671 665.08l24.144-.002h.001a15.387 15.387 0 0115.387 15.387v.5l-39.531.001z" fill="#2f2e41"/><path fill="#ffb6b6" d="M134.799 669.082h12.259l5.833-47.288h-18.095l.003 47.288z"/><path d="M131.671 665.08l24.144-.002h.001a15.387 15.387 0 0115.387 15.387v.5l-39.531.001zM211.18 430.565l-71.896-6.326s-7.103 45.326-6.103 63.326 2 46 2 46l-8 122 31-4 4-101 13-55 15 70 10 90 27-3-9-109s8-98-7-113z" fill="#2f2e41"/><circle cx="182.181" cy="277.565" r="20" fill="#ffb8b8"/><path d="M204.083 322.222l-14.285-11.37s-15.469-11.937-24.543-3.112l-4.074 5.825s-23.098 8.657-23.098 19.657l1.098 60.343s-8 31 4 33l69.902 5.657-.902-57.657z" fill="#e4e4e4"/><path d="M166.68 295.065s-21-15-10-33c7.632-12.487 23.445-11.499 32.415-9.722a15.154 15.154 0 0111.086 8.722c1.5 3.5 1.5 7-4.5 7-12 0-6 9-6 9s-8 2-6 9-17 9-17 9z" fill="#2f2e41"/><path d="M276.239 427.61a10.056 10.056 0 00-13.945-6.581l-24.792-25.736-4.072 18.118 24.168 21.661a10.11 10.11 0 0018.641-7.462z" fill="#ffb6b6"/><path d="M250.745 428.001a4.505 4.505 0 01-3.59-.758l-36.977-28.117a46.373 46.373 0 01-17.974-27.25l-8.211-36.116a14.497 14.497 0 1126.215-12.384l15.98 56.532 31.276 33.361a4.515 4.515 0 01.48 5.149l-4.237 7.416a4.506 4.506 0 01-1.968 1.828 4.455 4.455 0 01-.994.34z" fill="#e4e4e4"/><path d="M758.656 30.177H181.1a.838.838 0 010-1.676h577.556a.838.838 0 010 1.676z" fill="#cacaca"/><circle cx="199.967" cy="9.221" r="9.221" fill="#3f3d56"/><circle cx="231.821" cy="9.221" r="9.221" fill="#3f3d56"/><circle cx="263.675" cy="9.221" r="9.221" fill="#3f3d56"/><path d="M739.57 5.64h-22.632a1.677 1.677 0 010-3.353h22.633a1.677 1.677 0 010 3.353zM739.57 11.927h-22.632a1.677 1.677 0 010-3.353h22.633a1.677 1.677 0 010 3.353zM739.57 18.214h-22.632a1.677 1.677 0 010-3.353h22.633a1.677 1.677 0 010 3.353z" fill="#3f3d56"/><path d="M382 681.222H1a1 1 0 110-2h381a1 1 0 010 2z" fill="#cacaca"/><path d="M584.576 161.965H347.299a5.393 5.393 0 01-5.459-4.654 5.19 5.19 0 015.163-5.724h237.204a5.442 5.442 0 015.538 4.724 5.19 5.19 0 01-5.17 5.654z" fill="#89b0ae"/></svg>
  """

sharing :: Icons.Icon
sharing = icon
  """
  <svg data-name="Layer 1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 871.172 793.827"><path d="M531.48 529.174H339.692a2.533 2.533 0 01-2.53-2.53V315.626a2.533 2.533 0 012.53-2.53H531.48a2.533 2.533 0 012.53 2.53v211.018a2.533 2.533 0 01-2.53 2.53z" fill="#fff"/><path d="M531.48 529.174H339.692a2.533 2.533 0 01-2.53-2.53V315.626a2.533 2.533 0 012.53-2.53H531.48a2.533 2.533 0 012.53 2.53v211.018a2.533 2.533 0 01-2.53 2.53zM339.692 314.108a1.52 1.52 0 00-1.518 1.519v211.017a1.52 1.52 0 001.518 1.518H531.48a1.52 1.52 0 001.518-1.518V315.627a1.52 1.52 0 00-1.518-1.519z" fill="#3f3d56"/><path d="M422.429 393.556H374.86a2.533 2.533 0 01-2.53-2.53v-47.567a2.533 2.533 0 012.53-2.53h47.568a2.533 2.533 0 012.53 2.53v47.567a2.533 2.533 0 01-2.53 2.53zM374.86 341.94a1.52 1.52 0 00-1.518 1.519v47.567a1.52 1.52 0 001.518 1.518h47.568a1.52 1.52 0 001.518-1.518v-47.567a1.52 1.52 0 00-1.518-1.519z" fill="#f2f2f2"/><path d="M408.766 399.376h-47.568a2.28 2.28 0 01-2.277-2.278v-47.567a2.28 2.28 0 012.277-2.277h47.568a2.28 2.28 0 012.277 2.277v47.567a2.28 2.28 0 01-2.277 2.278z" fill="#89b0ae"/><path d="M509.973 422.653H361.198a2.277 2.277 0 110-4.554h148.775a2.277 2.277 0 010 4.554zM474.045 438.34H361.198a2.277 2.277 0 110-4.554h112.847a2.277 2.277 0 010 4.554z" fill="#ccc"/><path d="M509.467 364.965h-65.785a2.277 2.277 0 010-4.554h65.785a2.277 2.277 0 110 4.554zM493.05 380.652h-49.368a2.277 2.277 0 010-4.554h49.368a2.277 2.277 0 010 4.554z" fill="#89b0ae"/><path d="M509.973 454.028H361.198a2.277 2.277 0 010-4.555h148.775a2.277 2.277 0 110 4.555zM474.045 469.715H361.198a2.277 2.277 0 010-4.555h112.847a2.277 2.277 0 110 4.555zM509.973 485.402H361.198a2.277 2.277 0 110-4.554h148.775a2.277 2.277 0 010 4.554zM509.973 501.09H361.198a2.277 2.277 0 010-4.555h148.775a2.277 2.277 0 010 4.554z" fill="#ccc"/><path d="M297.172 622.652a147.994 147.994 0 01-93.82 137.75c-2.17.87-4.38 1.68-6.6 2.43q-3.72 1.275-7.52 2.34-2.64.735-5.33 1.38a148.92 148.92 0 01-81.61-3.48c-2.07-.69-4.12-1.43-6.15-2.2-2.73-1.05-5.43-2.18-8.08-3.39-.2-.08-.39-.17-.58-.26a123.9 123.9 0 01-3.08-1.47h-.01q-2.115-1.02-4.19-2.12-4.77-2.52-9.33-5.37-2.37-1.47-4.67-3.05a147.939 147.939 0 0153-267.52q3.915-.81 7.91-1.4c1.67-.26 3.36-.48 5.05-.67 1.21-.14 2.42-.27 3.65-.37h.01q4.935-.45 9.96-.56l1.39-.03c.26 0 .52-.01.78 0h.01c.4-.01.8-.01 1.21-.01 1.68 0 3.36.03 5.03.09q5.52.18 10.92.76c2.08.22 4.13.49 6.18.79.23.04.47.07.7.11.68.11 1.37.22 2.05.34 1.44.23 2.87.5 4.3.79 1.34.27 2.67.55 4 .85a144.127 144.127 0 0114.54 4.13 148.41 148.41 0 0177.94 61.93q.765 1.215 1.5 2.46c.38.62.74 1.24 1.1 1.87q.195.315.36.63c.08.12.14.24.21.36.66 1.15 1.3 2.32 1.92 3.49a147.344 147.344 0 0117.25 69.4z" fill="#fff"/><path d="M149.187 771.656a149.813 149.813 0 01-47.21-7.635 160.635 160.635 0 01-6.19-2.214 144.687 144.687 0 01-8.14-3.416 10.384 10.384 0 01-.59-.264c-.996-.46-1.969-.921-2.959-1.41l-.14-.065a125.306 125.306 0 01-4.224-2.136 150.374 150.374 0 01-9.392-5.406 120.901 120.901 0 01-4.707-3.074 148.939 148.939 0 0153.366-269.324 141.961 141.961 0 017.965-1.41c1.58-.245 3.293-.473 5.084-.674 1.218-.141 2.44-.272 3.68-.373 3.296-.3 6.67-.49 10.03-.563l1.66-.032c.19-.003.38-.005.57.002.355-.01.763-.01 1.182-.01 1.692 0 3.396.03 5.066.09 3.688.12 7.387.378 10.99.765 1.909.202 3.943.462 6.219.795l.961.153c.605.097 1.215.196 1.818.302 1.296.207 2.676.46 4.325.795 1.346.271 2.683.553 4.021.854a145.761 145.761 0 0114.643 4.16 149.83 149.83 0 0178.465 62.347q.77 1.22 1.513 2.48c.377.614.742 1.243 1.107 1.883.117.188.24.402.355.619.056.088.105.175.154.263l.057.1a118.974 118.974 0 011.94 3.525 149.044 149.044 0 01-77.088 208.55c-2.118.848-4.356 1.672-6.646 2.446-2.48.85-5.028 1.643-7.57 2.355-1.76.49-3.566.958-5.367 1.39a149.99 149.99 0 01-34.948 4.132zm-64.566-16.904h.005l.22.104c1.021.505 2.023.982 3.055 1.458.182.086.352.167.533.24 2.631 1.2 5.33 2.332 8.067 3.384 2.172.824 4.171 1.54 6.107 2.185a147.79 147.79 0 0081.06 3.456 151.33 151.33 0 005.295-1.37 138.115 138.115 0 007.465-2.323c2.259-.764 4.462-1.575 6.552-2.413a146.161 146.161 0 0093.192-136.822v-.001a146.852 146.852 0 00-17.133-68.929 115.27 115.27 0 00-1.905-3.461l-.073-.131c-.032-.057-.063-.115-.1-.171l-.055-.092q-.149-.284-.324-.568a72.191 72.191 0 00-1.103-1.873 98.744 98.744 0 00-1.493-2.45 147.827 147.827 0 00-77.418-61.517 143.546 143.546 0 00-14.439-4.1c-1.32-.3-2.642-.578-3.975-.846-1.622-.329-2.975-.577-4.26-.783-.609-.107-1.21-.204-1.804-.3l-.96-.152c-2.222-.325-4.23-.582-6.113-.78a146.535 146.535 0 00-10.848-.756c-1.649-.06-3.33-.089-4.997-.089-.407.001-.794 0-1.186.01-.228-.007-.385-.005-.542-.003l-1.641.033c-3.313.072-6.641.26-9.89.556l-.101.004c-1.138.095-2.336.224-3.535.363-1.767.199-3.452.422-5.011.665-2.637.39-5.28.857-7.862 1.391a146.939 146.939 0 00-52.643 265.712 118.577 118.577 0 004.638 3.03 148.179 148.179 0 009.27 5.335c1.31.694 2.637 1.367 3.952 2.004z" fill="#cbcbcb"/><path d="M275.818 590.515a18.172 18.172 0 01-27.838 1.207l-39.047 14.13-14.374-21.634 55.422-19.18a18.27 18.27 0 0125.837 25.477z" fill="#9f616a"/><path fill="#ccc" d="M153.806 592.529l80.444-26.59 13.556 29.59-75 23-19-26z"/><path d="M183.902 766.552a148.92 148.92 0 01-81.61-3.48l1.8-17.02 1.67-15.9 25.05-38.73 8.03-12.41 33.09 12.03 3.37 21.25 5.37 33.89z" fill="#2f2e41"/><path d="M203.352 760.402c-2.17.87-4.38 1.68-6.6 2.43q-3.72 1.275-7.52 2.34-2.64.735-5.33 1.38a148.92 148.92 0 01-81.61-3.48c-2.07-.69-4.12-1.43-6.15-2.2a115.185 115.185 0 01-.54 17.75 209.818 209.818 0 01-7.54-21.14c-.15-.48-.29-.96-.43-1.44-.7-2.42-1.32-4.73-1.88-6.9-.64-2.51-1.19-4.84-1.65-6.97-1.83-8.36-2.41-13.53-2.41-13.53l19.55-40.6 3.91.45 1.62.18 9.8 1.12 14.24 1.63 23.07 2.63 21.42 18.24 19.19 16.35s1.6 5.44 4.11 14.45c1.36 4.84 2.97 10.71 4.75 17.31z" fill="#2f2e41"/><circle cx="141.372" cy="455.742" r="34.591" fill="#9f616a"/><path d="M165.45 528.611s-8.313-25.698-27.864-25.698c-4.888 0-12.776.406-19.188 4.59-25.816 16.846-41.52 45.48-42.724 76.282l-6.011 153.882s33.087 12.031 45.118 7.52 24.063-34.591 24.063-34.591-3.008 40.606 9.024 39.102 57.15-4.512 58.654-10.527-23.024-135.363-23.024-135.363z" fill="#ccc"/><path d="M124.556 465.883c3.104-.185 6.134.864 9.215 1.273 11.025 1.463 22.495-6.537 24.93-17.39a13.342 13.342 0 011.274-4.154c1.697-2.77 5.562-3.298 8.747-2.661s6.206 2.128 9.45 2.298c5.01.263 9.818-2.82 12.488-7.068s3.425-9.472 3.065-14.475l-2.402 2.52a12.197 12.197 0 01-1.087-6.629 7.692 7.692 0 00-7.286 1.865c-2.118.225-.52-3.928-2.093-5.363a3.64 3.64 0 00-2.56-.479c-4.505.043-8.149-3.402-11.698-6.176a49.711 49.711 0 00-20.996-9.577c-5.087-.992-10.495-1.151-15.32.743-3.961 1.556-7.277 4.389-10.345 7.339-7.552 7.262-14.24 15.794-17.382 25.79a43.415 43.415 0 00-.208 25.175c1.266 4.273 5.082 18.906 10.667 19.247 7.02.428 2.805-11.757 11.541-12.278z" fill="#2f2e41"/><path opacity=".1" style="isolation:isolate" d="M139.162 574.153l-35.421 51.951-11.328 120.67 46.749-172.621z"/><ellipse cx="452.891" cy="594.908" rx="2.642" ry=".991" transform="rotate(-54.493 319.14 728.004)" fill="#3f3d56"/><path d="M242.047 598.366a4.624 4.624 0 01-1.078-6.45l40.286-56.465a4.624 4.624 0 016.45-1.078l-45.658 63.993z" fill="#89b0ae"/><path d="M243.702 604.01l-2.331-1.664a3.743 3.743 0 01-.873-5.22l45.869-64.29a3.16 3.16 0 014.408-.736l2.805 2.001-49.879 69.908z" fill="#2f2e41"/><path fill="#d0cde1" d="M281.912 543.629l3.07-4.302 1.075.767-3.07 4.302zM277.691 549.545l3.07-4.303 1.075.768-3.07 4.302z"/><path d="M100.454 786.135a18.172 18.172 0 00-6.89-26.999l2.245-41.464-24.866-7.51-2.346 58.601a18.27 18.27 0 0031.857 17.372z" fill="#9f616a"/><path d="M100.321 752.029H64.35L54.45 641.224l.033-.1 33.67-98.883a26.49 26.49 0 1148.235 21.398l-39.617 71.347z" fill="#ccc"/><path d="M870.172 271.495a148.135 148.135 0 11-151.39-147.96l1.39-.03c.66-.01 1.33-.01 2-.01 1.68 0 3.36.03 5.03.09q5.52.18 10.92.76c2.08.22 4.13.49 6.18.79.23.04.47.07.7.11a146.198 146.198 0 0124.89 6.11 148.077 148.077 0 01100.28 140.14z" fill="#fff"/><path d="M722.034 420.77c-82.171.004-149.055-66.806-149.133-148.994a149.136 149.136 0 01145.859-149.24l1.39-.031c2.356-.034 4.752-.003 7.088.08 3.684.121 7.383.378 10.99.766 1.91.202 3.945.462 6.219.795l.726.114a148.835 148.835 0 01125.999 147.235c.077 82.233-66.761 149.197-148.995 149.275zm.122-296.275c-.66 0-1.32 0-1.97.01l-1.382.03c-79.423 1.745-143.978 67.797-143.903 147.239.077 81.08 66.07 146.992 147.133 146.996h.141c81.13-.077 147.073-66.143 146.997-147.274a146.835 146.835 0 00-124.323-145.262l-.719-.113c-2.22-.325-4.229-.582-6.113-.781-3.56-.383-7.21-.637-10.848-.755-1.649-.06-3.33-.09-4.997-.09z" fill="#cbcbcb"/><ellipse cx="771.108" cy="331.157" rx=".991" ry="2.642" transform="rotate(-57.656 640.674 453.975)" fill="#3f3d56"/><path d="M604.6 270.88a4.624 4.624 0 016.381-1.432l58.601 37.109a4.624 4.624 0 011.433 6.38L604.6 270.882z" fill="#89b0ae"/><path d="M599.057 272.844l1.843-2.912a3.16 3.16 0 014.36-.979l66.723 42.252a3.742 3.742 0 011.16 5.164l-1.532 2.42-72.554-45.945z" fill="#2f2e41"/><path fill="#d0cde1" d="M608.284 275.56l.707-1.117 4.465 2.828-.707 1.116zM614.424 279.447l.707-1.116 4.465 2.827-.707 1.116z"/><circle cx="722.567" cy="97.079" r="41.652" fill="#ffb8b8"/><path d="M762.232 414.015a148.694 148.694 0 01-101.75-7.95l3.2-85.5.24-6.65.09-2.16.11-3.18.08-2.03s.13-.01.38-.02c.68-.05 2.25-.16 4.5-.29 2.61-.15 6.12-.34 10.21-.51.97-.04 1.97-.08 3-.13 21.54-.8 55.18-.97 59.63 5.52l.56 3.82.9 6.17 1.84 12.61s20.92 41.6 17.01 80.3z" fill="#2f2e41"/><path d="M772.806 198.373l-8.204 75.78-15.624 34.56-5.79 12.816-83.813-1.016 8.636-58.414c-12.191-31.493 16.762-44.192 16.762-44.192l30.033-61.534s8-4 28.89-5.007c19.418-.936 4.11 2.007 4.11 2.007 25.397-2.54 25 45 25 45z" fill="#ccc"/><path d="M747.602 278.152l-15.624 34.561c-5.019-9.387-8.839-20.018-8.839-26.296 0-17.27 17.27-8.127 17.27-8.127s-13.968-76.447-13.968-99.305c0-15.182 12.658 52.197 21.161 99.167z" opacity=".2" style="isolation:isolate"/><path d="M770.57 71.703A46.684 46.684 0 00758.32 54.45a9.108 9.108 0 005.123-4.317c1.603-3.173.754-7.153-1.426-9.993a18.192 18.192 0 00-7.463-5.37 14.751 14.751 0 006.796 1.307c2.833-.295 5.665-2.518 5.497-5.277-.16-2.628-2.746-4.405-5.066-5.776l-4.515-2.669a23.513 23.513 0 001.176-2.668A18.51 18.51 0 00754.437 0c4.929 5.018 3.67 12.783-1.2 19.085-.156.203-.325.401-.488.602l-2.895-1.711c-2.406-1.423-4.908-2.878-7.703-3.214s-5.979.771-7.12 3.265c-1.222 2.668.167 6.074-1.424 8.552-1.393 2.169-4.434 2.63-7.07 2.528-2.634-.103-5.436-.52-7.78.646-3.26 1.62-4.33 6.003-2.88 9.27a9.85 9.85 0 002.805 3.57c-6.976.974-13.847 3.046-20.599 5.152-7.961 2.485-16.071 5.123-22.725 10.152-6.654 5.028-11.695 12.966-11.061 21.282s8.895 21.415 17.13 20.085l1.253-3.924a26.476 26.476 0 0130.278 5.383c.095-5.236 8.064-6.975 12.036-3.562s4.737 9.185 5.045 14.413c.308 5.228.6 10.92 4.043 14.865 3.305 3.787 8.935 4.96 13.833 3.833 4.898-1.128 9.132-4.235 12.64-7.835 12.724-13.06 16.822-33.818 10.016-50.734z" fill="#2f2e41"/><path d="M637.72 315.51a18.172 18.172 0 0026.999-6.89l113.465 2.244L785.693 286l-130.6-2.346a18.27 18.27 0 00-17.373 31.856z" fill="#ffb8b8"/><path d="M748.029 149.27s-26.413 2.032-26.413 24.89 23.873 104.13 23.873 104.13-17.27-9.144-17.27 8.127 28.953 67.557 32 34.032c0 0 43.684 21.334 41.145-18.794S771.394 176.7 771.394 176.7s-3.047-28.954-23.365-27.43z" fill="#ccc"/></svg>
  """

takingNotes :: Icons.Icon
takingNotes = icon
  """
  <svg data-name="Layer 1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 737.39 544"><path d="M273.316 335.182L120.67 367.357a17.02 17.02 0 01-20.14-13.129L74.334 229.96a17.02 17.02 0 0113.129-20.14l152.646-32.176a17.02 17.02 0 0120.14 13.129l26.194 124.269a17.02 17.02 0 01-13.128 20.14zM87.876 211.775a15.017 15.017 0 00-11.584 17.772l26.194 124.269a15.017 15.017 0 0017.771 11.584l152.646-32.175a15.017 15.017 0 0011.584-17.772l-26.194-124.269a15.017 15.017 0 00-17.771-11.584z" fill="#3f3d56"/><path data-name="Path 411" d="M230.625 218.618l-77.038 16.239a2.73 2.73 0 01-3.23-1.863 2.63 2.63 0 012-3.254l78.42-16.53c3.061 1.726 2.094 4.935-.153 5.409z" fill="#ccc"/><path data-name="Path 412" d="M233.474 232.131l-77.039 16.239a2.73 2.73 0 01-3.23-1.863 2.63 2.63 0 012-3.254l78.42-16.53c3.062 1.726 2.094 4.935-.152 5.409z" fill="#ccc"/><path data-name="Path 413" d="M141.403 259.574l-23.511 4.956a2.962 2.962 0 01-3.506-2.285l-5.848-27.746a2.962 2.962 0 012.285-3.505l23.511-4.956a2.963 2.963 0 013.506 2.285l5.848 27.745a2.962 2.962 0 01-2.285 3.506z" fill="#ccc"/><path data-name="Path 414" d="M239.081 259.847l-117.574 24.782a2.73 2.73 0 01-3.23-1.863 2.63 2.63 0 011.999-3.254l118.958-25.074c3.062 1.726 2.094 4.935-.153 5.409z" fill="#ccc"/><path data-name="Path 415" d="M241.93 273.364l-117.574 24.782a2.73 2.73 0 01-3.23-1.863 2.63 2.63 0 012-3.254l118.957-25.074c3.062 1.726 2.094 4.935-.152 5.409z" fill="#ccc"/><path data-name="Path 416" d="M244.778 286.872l-117.575 24.783a2.73 2.73 0 01-3.23-1.863 2.63 2.63 0 012-3.255l118.957-25.074c3.062 1.726 2.094 4.935-.152 5.409z" fill="#ccc"/><path data-name="Path 417" d="M247.627 300.388L130.052 325.17a2.73 2.73 0 01-3.23-1.863 2.63 2.63 0 012-3.255l118.957-25.074c3.062 1.726 2.094 4.935-.152 5.409z" fill="#ccc"/><path data-name="Path 418" d="M250.474 313.897L132.9 338.68a2.73 2.73 0 01-3.23-1.863 2.63 2.63 0 011.999-3.254l118.957-25.075c3.062 1.726 2.095 4.936-.152 5.41z" fill="#ccc"/><path d="M269.229 252.03a10.527 10.527 0 00-.683 1.51l-48.134 11.945-8.373-8.663-14.725 10.928 13.333 16.776a8 8 0 009.289 2.428l52.23-21.342a10.497 10.497 0 10-2.937-13.582zM104.932 358.323a10.743 10.743 0 006.378-15.188l33.125-92.067-23.004-4.077-25.327 91.78a10.8 10.8 0 008.828 19.552z" fill="#ffb8b8"/><path d="M140.228 267.41l-22.713-8.716a4.817 4.817 0 01-2.47-6.863l11.916-21.126a13.377 13.377 0 0124.95 9.66l-5.256 23.597a4.817 4.817 0 01-6.427 3.449z" fill="#89b0ae"/><path fill="#ffb8b8" d="M191.493 531.175h12.259l5.833-47.288-18.094.001.002 47.287z"/><path d="M188.366 527.173l24.143-.001h.001a15.386 15.386 0 0115.387 15.386v.5l-39.53.001z" fill="#2f2e41"/><path fill="#ffb8b8" d="M122.493 531.175h12.259l5.833-47.288-18.094.001.002 47.287z"/><path d="M119.366 527.173l24.143-.001h.001a15.386 15.386 0 0115.387 15.386v.5l-39.53.001zM135.345 512.142h-15.514a4.5 4.5 0 01-4.495-4.713l7.37-158.332.463-.013 74.92-1.953 14.66 158.423a4.5 4.5 0 01-4.12 4.91l-17.427 1.394a4.502 4.502 0 01-4.767-3.582L164.145 395.5a1.454 1.454 0 00-1.47-1.2h-.005a1.454 1.454 0 00-1.467 1.212l-21.442 113a4.51 4.51 0 01-4.416 3.63z" fill="#2f2e41"/><circle cx="163.765" cy="183.324" r="24.561" fill="#ffb8b8"/><path d="M155.53 367.651c-11.707 0-23.73-3.077-33.699-12.175a4.552 4.552 0 01-1.47-3.85c1.068-9.585 6.243-59.08 3.329-92.143a40.02 40.02 0 0112.955-33.304 39.598 39.598 0 0133.962-9.834c.306.054.612.112.918.17 19.664 3.806 33.326 22.004 31.78 42.332-2.256 29.66-4.278 69.779-1.103 92.467a4.493 4.493 0 01-2.414 4.637c-7.225 3.65-25.327 11.7-44.259 11.7z" fill="#89b0ae"/><path d="M187.579 269.178a4.812 4.812 0 01-2.11-3.412l-2.992-23.99a13.377 13.377 0 0125.754-7.247l9.856 22.16a4.817 4.817 0 01-3.11 6.598l-23.437 6.522a4.811 4.811 0 01-3.961-.631z" fill="#89b0ae"/><path d="M155.19 207.538a17.598 17.598 0 01-6.796-1.23c-.952-.367-1.94-.668-2.889-1.035-8.387-3.24-13.911-12.172-14.109-21.16s4.457-17.713 11.37-23.463 15.864-8.701 24.829-9.384c9.657-.735 20.53 1.714 25.926 9.756 1.437 2.14 2.441 4.738 1.59 7.322a4.686 4.686 0 01-1.32 2.028c-2.41 2.144-4.813.532-7.27.39-3.376-.196-6.408 2.537-7.498 5.739s-.61 6.727.27 9.992a24.962 24.962 0 011.258 6.076 6.108 6.108 0 01-2.531 5.379c-2.11 1.275-4.88.537-6.992-.736s-3.935-3.044-6.216-3.981-5.344-.73-6.671 1.348a7.379 7.379 0 00-.844 2.436c-1.19 5.42-.916 5.102-2.106 10.523z" fill="#2f2e41"/><path d="M495.39 174h-156a17.02 17.02 0 01-17-17V30a17.02 17.02 0 0117-17h156a17.02 17.02 0 0117 17v127a17.02 17.02 0 01-17 17zm-156-159a15.017 15.017 0 00-15 15v127a15.017 15.017 0 0015 15h156a15.017 15.017 0 0015-15V30a15.017 15.017 0 00-15-15z" fill="#3f3d56"/><path data-name="Path 411" d="M477.658 51.138h-78.73a2.73 2.73 0 01-2.777-2.49 2.63 2.63 0 012.627-2.771h80.144c2.64 2.32 1.031 5.261-1.265 5.261z" fill="#ccc"/><path data-name="Path 412" d="M477.658 64.948h-78.73a2.73 2.73 0 01-2.777-2.49 2.63 2.63 0 012.627-2.771h80.144c2.64 2.32 1.031 5.261-1.265 5.261z" fill="#ccc"/><path data-name="Path 413" d="M381.907 72.811H357.88a2.962 2.962 0 01-2.959-2.959V41.497a2.962 2.962 0 012.96-2.959h24.027a2.963 2.963 0 012.96 2.959v28.355a2.962 2.962 0 01-2.96 2.959z" fill="#3f3d56"/><path data-name="Path 414" d="M477.43 93.224H357.27a2.73 2.73 0 01-2.776-2.489 2.63 2.63 0 012.627-2.772h121.572c2.64 2.32 1.031 5.261-1.265 5.261z" fill="#ccc"/><path data-name="Path 415" d="M477.43 107.038H357.27a2.73 2.73 0 01-2.776-2.489 2.63 2.63 0 012.627-2.772h121.572c2.64 2.32 1.031 5.261-1.265 5.261z" fill="#ccc"/><path data-name="Path 416" d="M477.43 120.843H357.27a2.73 2.73 0 01-2.776-2.489 2.63 2.63 0 012.627-2.772h121.572c2.64 2.32 1.031 5.261-1.265 5.261z" fill="#ccc"/><path data-name="Path 417" d="M477.43 134.656H357.27a2.73 2.73 0 01-2.776-2.489 2.63 2.63 0 012.627-2.772h121.572c2.64 2.32 1.031 5.261-1.265 5.261z" fill="#ccc"/><path data-name="Path 418" d="M477.43 148.462H357.27a2.73 2.73 0 01-2.776-2.489 2.63 2.63 0 012.627-2.772h121.572c2.64 2.32 1.031 5.261-1.265 5.261z" fill="#ccc"/><path d="M461.39 0a5.006 5.006 0 015 5v16a5.006 5.006 0 01-5 5h-88a5.006 5.006 0 01-5-5V5a5.006 5.006 0 015-5" fill="#89b0ae"/><path d="M720.39 331h-156a17.02 17.02 0 01-17-17V187a17.02 17.02 0 0117-17h156a17.02 17.02 0 0117 17v127a17.02 17.02 0 01-17 17zm-156-159a15.017 15.017 0 00-15 15v127a15.017 15.017 0 0015 15h156a15.017 15.017 0 0015-15V187a15.017 15.017 0 00-15-15z" fill="#3f3d56"/><path data-name="Path 411" d="M702.658 208.138h-78.73a2.73 2.73 0 01-2.777-2.49 2.63 2.63 0 012.627-2.771h80.144c2.64 2.32 1.031 5.261-1.265 5.261z" fill="#3f3d56"/><path data-name="Path 412" d="M702.658 221.948h-78.73a2.73 2.73 0 01-2.777-2.49 2.63 2.63 0 012.627-2.771h80.144c2.64 2.32 1.031 5.261-1.265 5.261z" fill="#3f3d56"/><path data-name="Path 413" d="M606.907 229.811H582.88a2.962 2.962 0 01-2.959-2.959v-28.355a2.962 2.962 0 012.96-2.959h24.027a2.963 2.963 0 012.96 2.959v28.355a2.962 2.962 0 01-2.96 2.959z" fill="#ccc"/><path data-name="Path 413" d="M701.907 306.811H677.88a2.962 2.962 0 01-2.959-2.959v-16.355a2.962 2.962 0 012.96-2.959h24.027a2.963 2.963 0 012.96 2.959v16.355a2.962 2.962 0 01-2.96 2.959z" fill="#3f3d56"/><path data-name="Path 414" d="M702.43 250.224H582.27a2.73 2.73 0 01-2.776-2.489 2.63 2.63 0 012.627-2.772h121.572c2.64 2.32 1.031 5.261-1.265 5.261z" fill="#ccc"/><path data-name="Path 415" d="M702.43 264.038H582.27a2.73 2.73 0 01-2.776-2.489 2.63 2.63 0 012.627-2.772h121.572c2.64 2.32 1.031 5.261-1.265 5.261z" fill="#ccc"/><path data-name="Path 416" d="M702.43 277.843H582.27a2.73 2.73 0 01-2.776-2.489 2.63 2.63 0 012.627-2.772h121.572c2.64 2.32 1.031 5.261-1.265 5.261z" fill="#ccc"/><path data-name="Path 417" d="M639.43 291.656H582.27a2.73 2.73 0 01-2.776-2.489 2.63 2.63 0 012.627-2.772h58.572c2.64 2.32 1.031 5.261-1.265 5.261z" fill="#ccc"/><path data-name="Path 418" d="M639.43 305.462H582.27a2.73 2.73 0 01-2.776-2.489 2.63 2.63 0 012.627-2.772h58.572c2.64 2.32 1.031 5.261-1.265 5.261z" fill="#ccc"/><path d="M686.39 183h-88a5.006 5.006 0 01-5-5v-16a5.006 5.006 0 015-5h88a5.006 5.006 0 015 5v16a5.006 5.006 0 01-5 5z" fill="#89b0ae"/><path d="M382 544H1a1 1 0 010-2h381a1 1 0 010 2z" fill="#3f3d56"/><circle cx="96.764" cy="23.324" r="6.467" fill="#f2f2f2"/><circle cx="96.764" cy="49.098" r="6.467" fill="#f2f2f2"/><circle cx="96.764" cy="74.871" r="6.467" fill="#f2f2f2"/><circle cx="96.764" cy="100.644" r="6.467" fill="#f2f2f2"/><circle cx="123.764" cy="23.324" r="6.467" fill="#f2f2f2"/><circle cx="123.764" cy="49.098" r="6.467" fill="#f2f2f2"/><circle cx="123.764" cy="74.871" r="6.467" fill="#f2f2f2"/><circle cx="123.764" cy="100.644" r="6.467" fill="#f2f2f2"/><circle cx="150.764" cy="23.324" r="6.467" fill="#f2f2f2"/><circle cx="150.764" cy="49.098" r="6.467" fill="#f2f2f2"/><circle cx="150.764" cy="74.871" r="6.467" fill="#f2f2f2"/><circle cx="150.764" cy="100.644" r="6.467" fill="#f2f2f2"/><circle cx="177.764" cy="23.324" r="6.467" fill="#f2f2f2"/><circle cx="177.764" cy="49.098" r="6.467" fill="#f2f2f2"/><circle cx="177.764" cy="74.871" r="6.467" fill="#f2f2f2"/><circle cx="177.764" cy="100.644" r="6.467" fill="#f2f2f2"/><circle cx="204.764" cy="23.324" r="6.467" fill="#f2f2f2"/><circle cx="204.764" cy="49.098" r="6.467" fill="#f2f2f2"/><circle cx="204.764" cy="74.871" r="6.467" fill="#f2f2f2"/><circle cx="204.764" cy="100.644" r="6.467" fill="#f2f2f2"/><circle cx="231.764" cy="23.324" r="6.467" fill="#f2f2f2"/><circle cx="231.764" cy="49.098" r="6.467" fill="#f2f2f2"/><circle cx="231.764" cy="74.871" r="6.467" fill="#f2f2f2"/><circle cx="231.764" cy="100.644" r="6.467" fill="#f2f2f2"/><circle cx="601.764" cy="381.324" r="6.467" fill="#f2f2f2"/><circle cx="601.764" cy="407.098" r="6.467" fill="#f2f2f2"/><circle cx="601.764" cy="432.871" r="6.467" fill="#f2f2f2"/><circle cx="628.764" cy="381.324" r="6.467" fill="#f2f2f2"/><circle cx="628.764" cy="407.098" r="6.467" fill="#f2f2f2"/><circle cx="628.764" cy="432.871" r="6.467" fill="#f2f2f2"/><circle cx="655.764" cy="381.324" r="6.467" fill="#f2f2f2"/><circle cx="655.764" cy="407.098" r="6.467" fill="#f2f2f2"/><circle cx="655.764" cy="432.871" r="6.467" fill="#f2f2f2"/><circle cx="682.764" cy="381.324" r="6.467" fill="#f2f2f2"/><circle cx="682.764" cy="407.098" r="6.467" fill="#f2f2f2"/><circle cx="682.764" cy="432.871" r="6.467" fill="#f2f2f2"/></svg>
  """

ideas :: Icons.Icon
ideas = icon
  """
  <svg data-name="Layer 1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1101.648 863.943"><path d="M503.801 614.899l.006-1.848c3.673.011 7.403-.052 11.085-.187l.068 1.846c-3.707.136-7.46.2-11.159.189zm-11.154-.25c-3.693-.157-7.44-.39-11.136-.694l.151-1.841c3.673.302 7.395.534 11.063.689zm33.453-.572l-.14-1.843c3.667-.281 7.378-.642 11.03-1.07l.217 1.835c-3.677.432-7.414.794-11.107 1.078zm-55.69-1.26a280.918 280.918 0 01-11.045-1.583l.299-1.823c3.622.594 7.314 1.123 10.972 1.572zm77.85-1.342l-.289-1.825a275.726 275.726 0 0010.91-1.954l.363 1.81c-3.638.73-7.334 1.391-10.984 1.97zm-99.868-2.269a278.441 278.441 0 01-10.882-2.47l.445-1.794c3.564.886 7.2 1.711 10.81 2.454zm121.748-2.111l-.436-1.795c3.58-.87 7.185-1.822 10.716-2.832l.508 1.777a276.053 276.053 0 01-10.788 2.85zm-143.404-3.269a279.35 279.35 0 01-10.646-3.344l.59-1.752c3.48 1.172 7.039 2.29 10.574 3.323zm164.856-2.869l-.58-1.754a274.045 274.045 0 0010.452-3.692l.651 1.73a274.664 274.664 0 01-10.523 3.716zm-186.033-4.26a277.553 277.553 0 01-10.364-4.208l.73-1.697a273.896 273.896 0 0010.295 4.18zm206.89-3.584l-.72-1.702a275.519 275.519 0 0010.094-4.513l.789 1.672a279.294 279.294 0 01-10.163 4.543zm-227.436-5.247a275.419 275.419 0 01-9.984-5.032l.865-1.631a275 275 0 009.916 4.997zm247.574-4.248l-.855-1.637a277.68 277.68 0 009.704-5.317l.92 1.602a279.928 279.928 0 01-9.77 5.352zm-267.327-6.213a276.442 276.442 0 01-9.539-5.818l.994-1.557a276.072 276.072 0 009.475 5.78zm286.647-4.882l-.984-1.564a276.633 276.633 0 009.252-6.082l1.045 1.523a276.66 276.66 0 01-9.313 6.123zm-305.48-7.13a277.56 277.56 0 01-9.038-6.563l1.116-1.472a278.118 278.118 0 008.976 6.519zm323.857-5.486l-1.107-1.48a276.634 276.634 0 008.74-6.807l1.164 1.435a278.914 278.914 0 01-8.797 6.852zm-341.659-7.996a279.731 279.731 0 01-8.478-7.263l1.23-1.378c2.747 2.45 5.58 4.877 8.422 7.214zm358.975-6.057l-1.221-1.386c2.76-2.43 5.51-4.95 8.17-7.488l1.275 1.337a280.275 280.275 0 01-8.224 7.537zm-375.632-8.8a283.486 283.486 0 01-7.866-7.914l1.336-1.277a279.143 279.143 0 007.815 7.862zm391.775-6.6l-1.328-1.286a277.419 277.419 0 007.55-8.12l1.377 1.232a281.021 281.021 0 01-7.6 8.174zM296.56 520.92a278.801 278.801 0 01-7.206-8.516l1.434-1.164a274.627 274.627 0 007.158 8.457zm422.05-7.114l-1.427-1.174a278.044 278.044 0 006.877-8.702l1.473 1.116a280.7 280.7 0 01-6.923 8.76zm-436.114-10.195a279.3 279.3 0 01-6.498-9.06l1.523-1.046a276.257 276.257 0 006.455 9zm449.604-7.598l-1.516-1.055a279.426 279.426 0 006.159-9.231l1.557.994a280.622 280.622 0 01-6.2 9.292zM269.87 485.24a279.688 279.688 0 01-5.747-9.546l1.601-.92a278.239 278.239 0 005.71 9.482zm474.254-8.056l-1.595-.93c1.86-3.191 3.676-6.455 5.398-9.702l1.631.866a278.571 278.571 0 01-5.434 9.766zm-485.358-11.26a277.736 277.736 0 01-4.957-9.972l1.67-.788a276.995 276.995 0 004.924 9.905zm495.83-8.486l-1.667-.8a275.628 275.628 0 004.598-10.11l1.697.73a277.923 277.923 0 01-4.629 10.18zm-505.337-11.644a275.677 275.677 0 01-4.133-10.333l1.728-.651a274.823 274.823 0 004.106 10.262zm514.18-8.899l-1.726-.66a277.638 277.638 0 003.765-10.457l1.75.59a277.57 277.57 0 01-3.79 10.527zm-522.036-11.954a274.708 274.708 0 01-3.292-10.663l1.776-.508a273.852 273.852 0 003.27 10.59zm529.176-9.22l-1.774-.518a274.269 274.269 0 002.897-10.699l1.792.446a275.372 275.372 0 01-2.915 10.772zm-535.325-12.23c-.876-3.601-1.69-7.266-2.418-10.894l1.812-.364a275.398 275.398 0 002.4 10.821zm540.717-9.42l-1.81-.374a275.223 275.223 0 002.02-10.898l1.823.3a277.634 277.634 0 01-2.033 10.972zm-545.11-12.457c-.58-3.65-1.095-7.368-1.53-11.053l1.836-.216c.431 3.66.943 7.354 1.52 10.98zm548.733-9.562l-1.834-.226a275.87 275.87 0 001.136-11.025l1.842.153c-.305 3.684-.69 7.418-1.144 11.098zm-551.346-12.595c-.286-3.692-.501-7.44-.64-11.141l1.847-.07c.137 3.677.35 7.4.635 11.068zm553.19-9.638l-1.845-.08c.156-3.68.242-7.409.254-11.081l.001-.612 1.848.006-.001.608a284.48 284.48 0 01-.257 11.159zM229.26 337.164l-1.848-.006.002-.31c.011-3.604.093-7.254.243-10.843l1.846.077a281.8 281.8 0 00-.242 10.776zm550.381-10.198c-.14-3.669-.357-7.392-.647-11.066l1.843-.145c.29 3.699.509 7.447.65 11.14zM230.186 315.02l-1.841-.15c.302-3.7.683-7.434 1.132-11.102l1.834.225a277.722 277.722 0 00-1.125 11.027zm547.72-10.15a274.344 274.344 0 00-1.532-10.977l1.825-.292a276.159 276.159 0 011.541 11.051zm-545.029-11.85l-1.823-.298a275.613 275.613 0 012.023-10.974l1.809.371a275.166 275.166 0 00-2.009 10.901zM774.4 282.985a275.593 275.593 0 00-2.413-10.818l1.795-.439c.878 3.59 1.695 7.254 2.429 10.891zm-537.067-11.677l-1.793-.444a276.28 276.28 0 012.904-10.776l1.774.517c-1.03 3.533-2 7.134-2.885 10.703zm531.805-9.853a273.757 273.757 0 00-3.282-10.587l1.754-.583a275.596 275.596 0 013.303 10.659zm-525.605-11.427l-1.75-.588a277.371 277.371 0 013.777-10.532l1.726.658a274.754 274.754 0 00-3.752 10.462zm518.613-9.606a275.51 275.51 0 00-4.117-10.258l1.7-.723a277.349 277.349 0 014.146 10.328zM251.461 229.27l-1.699-.73a277.313 277.313 0 014.618-10.183l1.667.797a274.795 274.795 0 00-4.586 10.116zm502.038-9.192a275.843 275.843 0 00-4.934-9.9l1.635-.858a274.424 274.424 0 014.968 9.967zm-492.46-10.844l-1.633-.863a278.313 278.313 0 015.424-9.773l1.597.93a276.146 276.146 0 00-5.387 9.706zm482.193-8.752a277.178 277.178 0 00-5.72-9.477l1.563-.986a279.548 279.548 0 015.758 9.54zm-471.031-10.433l-1.558-.993a279.185 279.185 0 016.19-9.3l1.517 1.056a276.938 276.938 0 00-6.15 9.237zm459.214-8.286a278.99 278.99 0 00-6.465-8.992l1.478-1.108a278.886 278.886 0 016.507 9.052zm-446.551-9.932l-1.474-1.114a279.957 279.957 0 016.914-8.768l1.427 1.173a278.042 278.042 0 00-6.867 8.71zm433.265-7.788a277.395 277.395 0 00-7.167-8.45l1.384-1.223a278.688 278.688 0 017.216 8.506zm-419.187-9.348l-1.38-1.228a279.907 279.907 0 017.59-8.183l1.33 1.283a279.3 279.3 0 00-7.54 8.128zm404.519-7.26a279.784 279.784 0 00-7.824-7.854l1.282-1.33a281.677 281.677 0 017.876 7.906zm-389.122-8.687l-1.277-1.335a281.107 281.107 0 018.217-7.547l1.222 1.385a279.283 279.283 0 00-8.162 7.497zm373.166-6.703a278.102 278.102 0 00-8.43-7.205l1.172-1.428a279.415 279.415 0 018.486 7.254zm-356.55-7.956l-1.166-1.433a278.398 278.398 0 018.79-6.862l1.108 1.479a276.725 276.725 0 00-8.731 6.816zm339.405-6.112a278.29 278.29 0 00-8.983-6.509l1.053-1.517a280.686 280.686 0 019.045 6.552zm-321.676-7.165l-1.048-1.522a278.38 278.38 0 019.307-6.133l.986 1.563a275.211 275.211 0 00-9.245 6.092zm303.452-5.488a276.073 276.073 0 00-9.482-5.768l.929-1.598a280.058 280.058 0 019.545 5.807zm-284.728-6.318l-.922-1.601c3.201-1.843 6.486-3.647 9.764-5.362l.857 1.636a276.323 276.323 0 00-9.699 5.327zm265.537-4.833a276.14 276.14 0 00-9.922-4.986l.796-1.667a276.112 276.112 0 019.989 5.02zM387.01 88.75l-.79-1.67a278.91 278.91 0 0110.158-4.555l.722 1.7a275.51 275.51 0 00-10.09 4.525zm225.895-4.144a274.986 274.986 0 00-10.3-4.168l.66-1.727a276.485 276.485 0 0110.368 4.197zM407.36 80.112l-.653-1.728c3.462-1.307 7-2.561 10.518-3.728l.582 1.753a274.543 274.543 0 00-10.447 3.703zm184.78-3.423a274.378 274.378 0 00-10.579-3.31l.517-1.774a276.486 276.486 0 0110.649 3.332zm-163.743-3.557l-.51-1.775a275.973 275.973 0 0110.785-2.864l.438 1.795a275.5 275.5 0 00-10.713 2.844zm142.46-2.63a273.39 273.39 0 00-10.812-2.442l.37-1.81c3.634.743 7.297 1.57 10.885 2.458zm-120.928-2.62l-.365-1.811c3.625-.73 7.32-1.396 10.982-1.981l.292 1.824a277.41 277.41 0 00-10.909 1.968zm99.214-1.825a276.367 276.367 0 00-10.974-1.56l.224-1.835c3.674.448 7.391.976 11.047 1.572zm-77.327-1.669l-.218-1.834c3.677-.436 7.413-.803 11.105-1.09l.143 1.841c-3.667.286-7.378.65-11.03 1.083zm55.326-1.01a278.831 278.831 0 00-11.064-.677l.077-1.846c3.704.153 7.45.382 11.137.681zm-33.229-.714l-.07-1.846A283.51 283.51 0 01505 60.616l-.003 1.847c-3.684-.005-7.413.062-11.084.201z" fill="#3f3d56"/><path fill="#f1f1f1" d="M1032.068 458.105L855.972 763.116l-58.213 100.827-11.091-6.402-110.761-63.947-82.683-47.74-300.639-173.568L334.648 0l697.42 458.105z"/><path d="M209.861 580.564c8.849-36.155 37.512-65.298 73.892-73.773a100.828 100.828 0 0158.178 3.74c18.935 7.143 35.177 19.847 50.292 33.006 29.198 25.42 57.48 51.881 92.75 68.843 31.505 15.151 66.98 21.586 101.237 12.551a105.324 105.324 0 0042.173-21.733c12.509-10.829 21.31-25.006 27.149-40.38 12.238-32.225 11.995-67.474 21.34-100.45 4.814-16.995 12.458-33.05 24.665-46 12.068-12.804 27.428-22.186 43.72-28.625 38.636-15.27 80.454-14.031 121.267-13.492 38.572.509 78.1-.156 114.23-15.346 16.1-6.769 31.129-16.337 43.007-29.248 12.655-13.754 21.085-30.668 27.121-48.224 3.116-9.063 5.598-18.323 7.83-27.638.831-3.467-4.512-4.946-5.344-1.473-4.622 19.284-10.366 38.59-20.527 55.762a106.897 106.897 0 01-39.147 38.307c-34.041 19.557-74.227 22.53-112.63 22.445-41.215-.092-83.376-3.16-123.41 8.777-32.58 9.714-62.564 29.176-77.65 60.544-15.017 31.225-15.542 66.658-23.018 99.943-3.713 16.531-9.265 32.808-19.222 46.686-9.926 13.833-24.14 24.17-39.876 30.475-33.643 13.48-71.16 9.527-103.817-4.772-36.612-16.03-65.937-42.735-95.574-68.81-14.277-12.56-29.292-24.79-46.62-32.906a105.261 105.261 0 00-147.36 70.318c-.849 3.464 4.495 4.942 5.344 1.473z" fill="#3f3d56"/><path d="M602.42 716.49H509.6a324.642 324.642 0 010-110.67h92.82a199.133 199.133 0 000 110.67z" fill="#ff6584"/><path d="M255.208 605.603h92.82a324.642 324.642 0 000-110.67h-92.82a199.133 199.133 0 010 110.67z" fill="#89b0ae"/><path d="M274.878 541.49a22.218 22.218 0 0119.23 1.445c4.877 2.725 9.301 6.053 14.961 6.922a23.132 23.132 0 0013.582-2.076c5.407-2.643 8.745-7.732 13.374-11.382 2.597-2.048-1.064-5.665-3.637-3.637-4.52 3.565-7.745 8.75-13.252 10.94-5.897 2.345-11.636 1.148-16.984-1.933-4.31-2.483-8.328-5.326-13.278-6.407a27.154 27.154 0 00-15.363 1.169c-3.075 1.143-1.744 6.115 1.367 4.958zM274.878 566.345a22.218 22.218 0 0119.23 1.445c4.877 2.726 9.301 6.054 14.961 6.922a23.132 23.132 0 0013.582-2.075c5.407-2.644 8.745-7.733 13.374-11.383 2.597-2.048-1.064-5.664-3.637-3.636-4.52 3.565-7.745 8.75-13.252 10.94-5.897 2.344-11.636 1.148-16.984-1.933-4.31-2.484-8.328-5.327-13.278-6.407a27.154 27.154 0 00-15.363 1.168c-3.075 1.143-1.744 6.115 1.367 4.959zM577.119 638.34a22.218 22.218 0 00-19.23 1.446c-4.877 2.725-9.302 6.053-14.961 6.921a23.131 23.131 0 01-13.582-2.075c-5.407-2.644-8.745-7.733-13.374-11.383-2.597-2.048 1.064-5.664 3.636-3.636 4.521 3.565 7.745 8.75 13.253 10.94 5.896 2.344 11.636 1.148 16.983-1.933 4.311-2.484 8.329-5.326 13.278-6.407a27.154 27.154 0 0115.364 1.168c3.075 1.143 1.744 6.116-1.367 4.959zM577.119 662.338a22.218 22.218 0 00-19.23 1.446c-4.877 2.725-9.302 6.053-14.961 6.922a23.131 23.131 0 01-13.582-2.076c-5.407-2.644-8.745-7.732-13.374-11.382-2.597-2.048 1.064-5.665 3.636-3.637 4.521 3.565 7.745 8.75 13.253 10.94 5.896 2.345 11.636 1.148 16.983-1.933 4.311-2.483 8.329-5.326 13.278-6.407a27.154 27.154 0 0115.364 1.168c3.075 1.143 1.744 6.116-1.367 4.96zM577.119 686.337a22.218 22.218 0 00-19.23 1.445c-4.877 2.725-9.302 6.053-14.961 6.922a23.131 23.131 0 01-13.582-2.076c-5.407-2.643-8.745-7.732-13.374-11.382-2.597-2.048 1.064-5.664 3.636-3.636 4.521 3.565 7.745 8.749 13.253 10.94 5.896 2.344 11.636 1.148 16.983-1.933 4.311-2.484 8.329-5.327 13.278-6.407a27.154 27.154 0 0115.364 1.168c3.075 1.143 1.744 6.115-1.367 4.959z" fill="#f2f2f2"/><path d="M760.137 207.995c-19.865-33.565-59.166-35.13-59.166-35.13s-38.297-4.897-62.864 46.224c-22.898 47.649-54.501 93.654-5.088 104.809l8.926-27.78 5.527 29.848a193.34 193.34 0 0021.143.361c52.917-1.708 103.314.5 101.69-18.49-2.156-25.242 8.946-67.545-10.168-99.842z" fill="#2f2e41"/><path d="M624.524 526.725L557.53 573.25s-39.08 40.94-11.165 40.94 39.08-35.357 39.08-35.357l85.603-37.22z" fill="#ffb8b8"/><path d="M651.508 345.283s-58.62 181.442-40.01 190.747 59.55 14.887 59.55 14.887z" fill="#d0cde1"/><path d="M651.508 345.283s-58.62 181.442-40.01 190.747 59.55 14.887 59.55 14.887z" opacity=".1"/><path d="M855.972 763.116l-58.213 100.827-11.091-6.402c-21.196-81.312-46.763-178.222-46.763-178.222l-63.998 114.275-82.683-47.74c19.79-42.606 53.628-142.833 53.628-142.833l150.744-76.298s48.382 29.775 50.242 102.353 1.86 115.381 1.86 115.381 2.46 7.071 6.274 18.659z" fill="#2f2e41"/><circle cx="688.727" cy="250.375" r="48.385" fill="#ffb8b8"/><path d="M687.796 290.385s11.166 35.358 0 48.385 61.411-18.61 61.411-18.61-26.053-37.218-18.609-53.967z" fill="#ffb8b8"/><path d="M756.6 298.093a47.688 47.688 0 01-4.601 6.25c-1.861 1.86-13.027 3.721-14.888 7.443s-29.301 9.027-29.301 9.027l-9.778 4s-46.524 11.165-46.524 20.47-3.722 37.219-3.722 37.219-14.887 16.748-5.583 57.69c3.585 15.774 8.583 46.376 5.78 78.809-4.47 51.741-17.646 107.575-3.919 111.006 22.331 5.583 85.604 7.444 106.074-20.47s22.331-52.106 44.663-53.967-3.722-78.16-3.722-78.16l27.914-182.372s-46.625-16.22-62.393 3.055z" fill="#d0cde1"/><path d="M700.402 588.734h92.82a324.643 324.643 0 000-110.67h-92.82a199.133 199.133 0 010 110.67z" fill="#89b0ae"/><path d="M719.705 515.777a22.218 22.218 0 0119.23 1.445c4.877 2.726 9.301 6.054 14.96 6.922a23.132 23.132 0 0013.583-2.075c5.407-2.644 8.744-7.733 13.373-11.383 2.597-2.048-1.064-5.664-3.636-3.636-4.521 3.565-7.745 8.75-13.253 10.94-5.896 2.344-11.635 1.148-16.983-1.933-4.311-2.484-8.329-5.326-13.278-6.407a27.154 27.154 0 00-15.363 1.168c-3.075 1.143-1.745 6.116 1.367 4.959zM719.705 539.775a22.218 22.218 0 0119.23 1.446c4.877 2.725 9.301 6.053 14.96 6.922a23.131 23.131 0 0013.583-2.076c5.407-2.644 8.744-7.732 13.373-11.382 2.597-2.048-1.064-5.665-3.636-3.637-4.521 3.565-7.745 8.75-13.253 10.94-5.896 2.345-11.635 1.148-16.983-1.933-4.311-2.484-8.329-5.326-13.278-6.407a27.154 27.154 0 00-15.363 1.168c-3.075 1.143-1.745 6.116 1.367 4.96z" fill="#f2f2f2"/><path d="M853.42 411.347l-66.994 46.523s-39.08 40.941-11.165 40.941 39.08-35.358 39.08-35.358l85.603-37.219z" fill="#ffb8b8"/><path d="M738.328 204.76L699.5 184.422l-24.36 3.78a43.55 43.55 0 00-35.797 33.417l-4.556 20.12 27.616-1.063 7.714-18v17.704l12.743-.49 7.395-28.659 4.623 30.508 45.299-.925z" fill="#2f2e41"/><path d="M805.966 298.76l13.027-3.722s132.127 53.967 128.405 100.49 1.86 46.524-20.47 48.385-87.465-20.47-85.604-24.192 14.888-22.331 14.888-22.331l-50.246-39.08z" fill="#d0cde1"/><circle cx="737.111" cy="250.375" r="7.444" fill="#ffb8b8"/><path d="M810.25 374.622c48.566-2.62 97.806 4.275 145.587-7.478 3.462-.851 1.995-6.198-1.474-5.345-47.26 11.625-96.07 4.688-144.113 7.28-3.548.192-3.57 5.736 0 5.543z" fill="#3f3d56"/><path d="M1024.624 455.42l77.024-51.795a324.642 324.642 0 00-61.756-91.837l-77.025 51.796a199.133 199.133 0 0161.757 91.837z" fill="#89b0ae"/><path d="M1005.17 391.242a22.218 22.218 0 0116.764-9.531c5.568-.46 11.096-.168 16.278-2.605a23.131 23.131 0 0010.112-9.302c3.012-5.21 2.942-11.296 4.746-16.908 1.013-3.148-4.044-4.107-5.046-.988-1.763 5.48-1.545 11.582-4.893 16.473-3.585 5.236-9.015 7.446-15.172 7.873-4.963.345-9.883.228-14.594 2.093a27.154 27.154 0 00-12.097 9.542c-1.913 2.665 1.965 6.049 3.902 3.353zM1019.04 411.867a22.218 22.218 0 0116.764-9.53c5.568-.46 11.096-.168 16.277-2.606a23.132 23.132 0 0010.113-9.3c3.012-5.212 2.942-11.297 4.746-16.91 1.013-3.148-4.044-4.106-5.046-.987-1.763 5.48-1.545 11.582-4.893 16.473-3.585 5.236-9.015 7.446-15.172 7.873-4.963.345-9.884.228-14.594 2.093a27.154 27.154 0 00-12.097 9.542c-1.913 2.665 1.965 6.048 3.902 3.352z" fill="#f2f2f2"/><path fill="#3f3d56" d="M.328 50.564l389.292 1.273-.328 100.229L0 150.793z"/><circle cx="46.687" cy="100.679" r="27.272" fill="#89b0ae"/><path fill="#fff" d="M103.98 77.872l112.83.37-.026 8.02-112.83-.369zM103.916 97.123l248.655.813-.027 8.021-248.654-.813zM103.854 116.373l248.655.813-.027 8.021-248.654-.813z"/><path fill="#3f3d56" d="M.328 317.564l389.292 1.273-.328 100.229L0 417.793z"/><circle cx="46.687" cy="367.679" r="27.272" fill="#89b0ae"/><path fill="#fff" d="M103.98 344.872l112.83.37-.026 8.02-112.83-.369zM103.916 364.123l248.655.813-.027 8.021-248.654-.813zM103.854 383.373l248.655.813-.027 8.021-248.654-.813z"/><path fill="#3f3d56" d="M483.297 284.392l-389.292 1.273-.328-100.228 389.292-1.273z"/><circle cx="436.61" cy="234.278" r="27.272" fill="#89b0ae"/><path fill="#fff" d="M379.343 219.493l-112.83.369-.027-8.02 112.83-.37zM379.406 238.743l-248.654.814-.027-8.021 248.655-.813zM379.47 257.994l-248.656.813-.026-8.021 248.655-.813z"/></svg>
  """

organize :: Icons.Icon
organize = icon
  """
  <svg data-name="Layer 1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1020 612"><path d="M279.76 611.197l-26.493-74.179s29.141 14.571 30.466 33.116c0 0-6.623-92.723-19.87-112.592 0 0 21.195 2.649 45.038 105.969 0 0 3.974-119.215-31.791-153.655 0 0 49.01 39.738 52.984 125.838v75.503zM776.59 611.197l-6.37-17.836s7.007 3.504 7.326 7.963c0 0-1.593-22.295-4.778-27.072 0 0 5.096.637 10.83 25.48 0 0 .955-28.665-7.645-36.946 0 0 11.784 9.555 12.74 30.257v18.154z" fill="#f2f2f2"/><circle cx="658.583" cy="341.339" r="116.457" fill="#f2f2f2"/><path fill="#f2f2f2" d="M0 8.835h274.677v152.598H0zM372.661 8.835h274.677v152.598H372.661zM745.323 8.835H1020v152.598H745.323z"/><path d="M706.086 611.616c-.184-.301-4.53-7.558-6.037-22.627-1.382-13.824-.493-37.126 11.592-69.631 22.894-61.578-5.276-111.263-5.564-111.758l1.39-.806c.073.125 7.342 12.793 11.636 32.964a143.813 143.813 0 01-5.956 80.16c-22.856 61.473-5.864 90.574-5.69 90.86z" fill="#3f3d56"/><circle cx="697.134" cy="393.543" r="10.441" fill="#89b0ae"/><circle cx="730.063" cy="432.094" r="10.441" fill="#3f3d56"/><circle cx="707.575" cy="457.795" r="10.441" fill="#89b0ae"/><circle cx="734.882" cy="479.48" r="10.441" fill="#89b0ae"/><circle cx="699.543" cy="513.213" r="10.441" fill="#3f3d56"/><path d="M712.394 612s-10.441-25.7 20.882-44.976zM699.553 611.534s-4.752-27.33-41.53-27.097zM.402 0h273.874v13.654H.402zM373.063 0H647.74v13.654H373.063zM745.724 0h273.874v13.654H745.724z" fill="#3f3d56"/><path fill="#fff" d="M20.882 34.937h49.795v35.339H20.882zM116.457 34.937h137.339v4.016H116.457zM116.457 51h137.339v4.016H116.457zM116.457 67.063h137.339v4.016H116.457zM393.543 34.937h49.795v35.339h-49.795zM489.118 34.937h137.339v4.016H489.118zM489.118 51h137.339v4.016H489.118zM489.118 67.063h137.339v4.016H489.118zM766.205 34.937H816v35.339h-49.795zM861.78 34.937h137.339v4.016H861.78zM861.78 51h137.339v4.016H861.78zM861.78 67.063h137.339v4.016H861.78z"/><circle cx="45.78" cy="53.008" r="16.063" fill="#89b0ae"/><circle cx="791.102" cy="53.008" r="16.063" fill="#89b0ae"/><path fill="#3f3d56" d="M126.714 161.433h1.606v4.819h-1.606zM128.32 359.008h-1.606v-9.638h1.606zm0-19.276h-1.606v-9.638h1.606zm0-19.275h-1.606v-9.638h1.606zm0-19.276h-1.606v-9.638h1.606zm0-19.275h-1.606v-9.638h1.606zm0-19.276h-1.606v-9.638h1.606zm0-19.276h-1.606v-9.637h1.606zm0-19.275h-1.606v-9.638h1.606zm0-19.276h-1.606v-9.638h1.606zm0-19.275h-1.606v-9.638h1.606zM132.336 374.268h-5.622v-5.622h1.606v4.015h4.016v1.607zM868.16 374.268h-9.68v-1.607h9.68zm-19.363 0h-9.681v-1.607h9.681zm-19.363 0h-9.683v-1.607h9.683zm-19.364 0h-9.682v-1.607h9.682zm-19.364 0h-9.682v-1.607h9.682zm-19.364 0h-9.682v-1.607h9.682zm-19.363 0h-9.683v-1.607h9.683zm-19.365 0h-9.681v-1.607h9.681zm-19.363 0h-9.683v-1.607h9.683zm-19.364 0h-9.682v-1.607h9.682zm-19.364 0h-9.682v-1.607h9.682zm-19.364 0h-9.682v-1.607h9.682zm-19.363 0h-9.683v-1.607h9.683zm-19.365 0h-9.681v-1.607h9.681zm-19.363 0h-9.682v-1.607h9.682zm-19.364 0h-9.682v-1.607h9.682zm-19.364 0h-9.682v-1.607h9.682zm-19.364 0h-9.682v-1.607h9.682zm-19.363 0h-9.682v-1.607h9.682zm-19.364 0h-9.682v-1.607h9.682zm-19.364 0h-9.682v-1.607h9.682zm-19.364 0h-9.682v-1.607h9.682zm-19.364 0h-9.682v-1.607h9.682zm-19.363 0h-9.683v-1.607h9.683zm-19.364 0h-9.682v-1.607h9.682zm-19.364 0h-9.682v-1.607h9.682zm-19.364 0h-9.682v-1.607h9.682zm-19.364 0h-9.682v-1.607h9.682zm-19.364 0h-9.682v-1.607h9.682zm-19.363 0h-9.683v-1.607h9.683zm-19.364 0h-9.682v-1.607h9.682zm-19.364 0H258.2v-1.607h9.682zm-19.364 0h-9.682v-1.607h9.682zm-19.364 0h-9.681v-1.607h9.681zm-19.364 0h-9.681v-1.607h9.681zm-19.363 0h-9.682v-1.607h9.682zm-19.364 0h-9.682v-1.607h9.682zm-19.364 0h-9.682v-1.607h9.682zM883.465 374.268h-5.622v-1.607h4.015v-4.015h1.607v5.622zM883.465 359.008h-1.607v-9.638h1.607zm0-19.276h-1.607v-9.638h1.607zm0-19.275h-1.607v-9.638h1.607zm0-19.276h-1.607v-9.638h1.607zm0-19.275h-1.607v-9.638h1.607zm0-19.276h-1.607v-9.638h1.607zm0-19.276h-1.607v-9.637h1.607zm0-19.275h-1.607v-9.638h1.607zm0-19.276h-1.607v-9.638h1.607zm0-19.275h-1.607v-9.638h1.607zM881.858 161.433h1.606v4.819h-1.606z"/><path fill="#3f3d56" d="M372.661 325.276h274.677v285.921H372.661z"/><path fill="#fff" d="M393.543 362.622h49.795v35.339h-49.795zM489.118 362.622h137.339v4.016H489.118zM489.118 378.685h137.339v4.016H489.118zM489.118 394.748h137.339v4.016H489.118zM393.543 489.921h49.795v35.339h-49.795zM489.118 489.921h137.339v4.016H489.118zM489.118 505.984h137.339V510H489.118zM489.118 522.047h137.339v4.016H489.118z"/><path fill="#89b0ae" d="M418.652 389.229l-17.045-22.726 4.038-3.028 13.885 18.514 35.393-28.665 3.196 3.906-39.467 31.999zM418.652 515.324l-17.045-22.726 4.038-3.028 13.885 18.513 35.393-28.665 3.196 3.907-39.467 31.999z"/><path fill="#3f3d56" d="M7.228 610.394h991.89V612H7.228zM544.574 170.268s26.504-11.55 38.953.65-33.331 8.184-38.953-.65z"/><path d="M590.34 115.497c-8.908-15.05-26.529-15.751-26.529-15.751s-17.171-2.196-28.186 20.725c-10.267 21.364-24.437 41.991-2.281 46.993l4.002-12.456 2.478 13.383a86.686 86.686 0 009.48.162c23.726-.766 46.322.224 45.595-8.29-.967-11.318 4.01-30.285-4.56-44.766z" fill="#2f2e41"/><path d="M602.802 388.323s-11.244 20.882-18.472 20.882-6.425 13.653-4.82 16.866 5.623 10.44-.802 20.882c-2.245 3.647-1.843 7.784-.37 11.623 4.126 10.759 20.178 7.5 19.666-4.012l-.02-.383c-.804-12.85 5.621-32.929 12.047-37.748s12.047-17.67 12.047-17.67zM525.7 383.504s11.244 20.882 18.472 20.882 6.425 13.653 4.82 16.866-5.623 10.44.802 20.882c2.245 3.647 1.843 7.784.37 11.624-4.126 10.758-20.178 7.498-19.666-4.013l.02-.383c.804-12.85-5.621-32.929-12.047-37.748s-12.047-17.67-12.047-17.67z" fill="#ffb8b8"/><path d="M534.534 280.7l-4.818 10.442s-70.678 23.291-64.252 50.598 37.748 69.874 42.567 68.268 30.52-6.425 31.322-20.079-18.472-37.748-18.472-37.748l26.504-24.094 38.567-.804 32.913 24.898s-34.535 30.52-19.275 42.567 37.748 20.079 40.96 15.26 39.355-76.3 33.733-82.725-62.646-30.52-62.646-30.52l-15.26-21.684-59.433-3.213z" fill="#2f2e41"/><path d="M548.991 148.181s4.016 26.504-4.016 29.717 16.063 16.866 16.063 16.866h13.654l8.835-18.473s-8.032-14.456-3.213-28.11-31.323 0-31.323 0z" fill="#ffb8b8"/><path d="M548.991 148.181s4.016 26.504-4.016 29.717 16.063 16.866 16.063 16.866h13.654l8.835-18.473s-8.032-14.456-3.213-28.11-31.323 0-31.323 0z" opacity=".1"/><path d="M543.37 170.67s8.834 9.637 10.44 11.243 8.835 10.441 24.095-2.41 16.866 2.41 16.866 2.41v96.378s-12.85-5.622-23.292 0-32.126-1.606-32.126-1.606l-4.015-99.59z" fill="#fff"/><path d="M549.269 170.67s-7.506-3.213-13.931 0-28.914 19.275-27.307 26.503 22.488 57.827 8.031 81.921-16.866 30.52-16.866 30.52l35.338-18.472s27.308-16.866 24.898-27.307-10.163-93.166-10.163-93.166zM581.127 170.67s16.856 1.606 17.66 3.212 25.299 8.433 23.692 15.661-23.693 71.882-14.055 83.93 25.701 28.913 20.079 30.52-15.26-2.41-24.898 0-32.126-17.67-29.716-39.355.019-88.347 7.238-93.969z" fill="#89b0ae"/><path d="M540.96 324.07v8.032s-4.016 18.473 8.834 17.67 4.82-22.489 4.82-22.489l-.804-7.228zM577.101 324.874s-8.834 18.472 0 20.079 15.26-10.441 15.26-13.654-15.26-6.425-15.26-6.425z" fill="#ffb8b8"/><ellipse cx="588.747" cy="137.339" rx="1.606" ry="3.213" fill="#ffb8b8"/><ellipse cx="541.361" cy="137.339" rx="1.606" ry="3.213" fill="#ffb8b8"/><path d="M602.802 181.11s20.079 0 19.276 8.032-27.307 146.173-27.307 146.173-11.244-10.44-20.882-8.031zM524.897 183.52s-20.079 4.015-20.079 16.063 28.913 127.7 32.93 127.7 19.275-2.409 19.275-6.425-32.126-137.338-32.126-137.338z" fill="#89b0ae"/><path d="M529.314 207.213l18.07 70.67s-8.432-61.836-18.07-70.67zM599.603 197.283l-18.276 92.397s5.841-87.067 18.276-92.397z" opacity=".2"/><circle cx="565.054" cy="137.74" r="23.291" fill="#ffb8b8"/><path fill="#2f2e41" d="M586.401 114.046l-17.409-9.119-24.041 3.73-4.974 21.969 12.382-.476 3.459-8.071v7.938l5.713-.22 3.316-12.85 2.073 13.679 20.31-.414-.829-16.166z"/></svg>
  """

reading :: Icons.Icon
reading = icon
  """
  <svg data-name="Layer 1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 837.5 606.999"><path d="M510 0H9a9.014 9.014 0 00-9 9v296a9.014 9.014 0 009 9h501a9.014 9.014 0 009-9V9a9.014 9.014 0 00-9-9zm7 305a7.008 7.008 0 01-7 7H9a7.008 7.008 0 01-7-7V9a7.008 7.008 0 017-7h501a7.008 7.008 0 017 7z" fill="#3f3d56"/><path fill="#3f3d56" d="M1 34h517v2H1z"/><circle cx="18" cy="17.5" r="5" fill="#ccc"/><circle cx="33" cy="17.5" r="5" fill="#ccc"/><circle cx="48" cy="17.5" r="5" fill="#ccc"/><path d="M380.5 433.5h-242a8.51 8.51 0 01-8.5-8.5V154a8.51 8.51 0 018.5-8.5h242a8.51 8.51 0 018.5 8.5v271a8.51 8.51 0 01-8.5 8.5z" fill="#89b0ae"/><path d="M330.5 284.5h-142a12.5 12.5 0 010-25h142a12.5 12.5 0 010 25zM330.5 331h-142a12.5 12.5 0 010-25h142a12.5 12.5 0 010 25zM330.5 378h-142a12.5 12.5 0 110-25h142a12.5 12.5 0 010 25zM287 223h-55a12.5 12.5 0 010-25h55a12.5 12.5 0 010 25z" fill="#fff"/><rect x="43" y="71" width="433" height="21" rx="10.5" fill="#ccc"/><path d="M739.617 476.023a9.69 9.69 0 003.466-14.45l22.582-124.975-21.073-.278-15.682 123.584a9.743 9.743 0 0010.707 16.119zM674.215 470.097l-8 38 1 85h15l7-83 11-35-26-5z" fill="#a0616a"/><path fill="#a0616a" d="M724.215 470.097l-8 38 1 85h15l7-83 11-35-26-5z"/><path d="M662.308 590.733l23.62 1.072-.675 14.872-38.491-1.747a14.887 14.887 0 0115.546-14.197zM712.308 590.733l23.62 1.072-.675 14.872-38.491-1.747a14.887 14.887 0 0115.546-14.197z" fill="#2f2e41"/><circle cx="710.715" cy="259.18" r="24.561" fill="#a0616a"/><path d="M764.993 319.93c-8.127-12.753-35.166-44.528-75.278-18.333 0 0 9.5 79.5-13.5 122.5 0 0 53 16 82 1 0 0-10-67 0-80a53.904 53.904 0 008.013-13.726 12.891 12.891 0 00-1.235-11.442z" fill="#ccc"/><path d="M755.215 319.097l9.813 4.09a3.718 3.718 0 012.285 3.277l.902 21.633-22-2z" fill="#ccc"/><path d="M680.215 422.097s-22 41-18 79l31 5 22-53-5 54 32 1s21-78 14-85z" fill="#2f2e41"/><path d="M674.617 466.023a9.69 9.69 0 003.466-14.45l22.582-124.975-21.073-.278-15.682 123.584a9.743 9.743 0 0010.707 16.119z" fill="#a0616a"/><path d="M699.215 302.097l-9-1s-8-2-11 9-3 33-3 33l28-1z" fill="#ccc"/><path d="M714.65 271.005c.881-3.062.626-7.205-2.342-8.363-1.547-.604-3.272-.128-4.924.041a13.141 13.141 0 01-9.08-2.5c-3.071-2.282-4.968-5.77-6.769-9.147l-2.724-5.105a22.18 22.18 0 01-1.545-3.327c-1.516-4.507.13-9.726 3.499-13.082a18.976 18.976 0 0112.963-5.082 34.984 34.984 0 0113.851 2.967 61.26 61.26 0 0120.653 13.875c3.832 3.92 7.32 9.027 6.52 14.45-.623 4.222-3.7 7.602-6.635 10.7l-10.645 11.237c-1.885 1.99-3.9 4.061-6.531 4.826s-6.024-.359-6.741-3.004c0 0-.433-5.425.45-8.486z" fill="#2f2e41"/><path d="M837.5 606a1.003 1.003 0 01-1 1h-288a1 1 0 010-2h288a1.003 1.003 0 011 1z" fill="#3f3d56"/></svg>
  """

openTabs :: Icons.Icon
openTabs = icon
  """
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 653.146 396.47"><g data-name="Group 22" transform="translate(-288 -252.29)"><g data-name="Group 20" transform="rotate(-33 -10518.57 -1553.572)"><path data-name="Path 561" d="M-1971.735 6182h-200.386a2.881 2.881 0 00-2.879 2.879v135.787a2.881 2.881 0 002.879 2.879h200.386a2.876 2.876 0 002.189-1.01.669.669 0 00.063-.079 2.7 2.7 0 00.413-.7 2.808 2.808 0 00.218-1.093v-135.784a2.882 2.882 0 00-2.883-2.879zm2.06 138.666a2.039 2.039 0 01-.34 1.129 2.129 2.129 0 01-.779.7 2.042 2.042 0 01-.941.228h-200.386a2.059 2.059 0 01-2.057-2.057v-135.787a2.059 2.059 0 012.057-2.057h200.386a2.06 2.06 0 012.06 2.057z" fill="#3f3d56"/><path data-name="Rectangle 99" fill="#3f3d56" d="M-2174.59 6193.538h205.323v.823h-205.323z"/><circle data-name="Ellipse 88" cx="2.469" cy="2.469" r="2.469" transform="translate(-2170.064 6185.703)" fill="#3f3d56"/><circle data-name="Ellipse 89" cx="2.469" cy="2.469" r="2.469" transform="translate(-2162.966 6185.703)" fill="#3f3d56"/><circle data-name="Ellipse 90" cx="2.469" cy="2.469" r="2.469" transform="translate(-2155.868 6185.703)" fill="#3f3d56"/><path data-name="Path 583" d="M-2100.846 6234.843h-51.916a2.525 2.525 0 010-5.049h51.916a2.525 2.525 0 010 5.049z" fill="#89b0ae"/><path data-name="Path 584" d="M-2068.794 6236.997h-89.134a.33.33 0 110-.66h89.134a.33.33 0 010 .66z" fill="#3f3d56"/><path data-name="Path 585" d="M-2100.846 6255.989h-51.916a2.525 2.525 0 010-5.049h51.916a2.525 2.525 0 010 5.049z" fill="#89b0ae"/><path data-name="Path 586" d="M-2068.794 6258.142h-89.134a.33.33 0 110-.66h89.134a.33.33 0 110 .66z" fill="#3f3d56"/><path data-name="Path 587" d="M-2100.846 6277.132h-51.916a2.525 2.525 0 010-5.049h51.916a2.525 2.525 0 010 5.049z" fill="#f2f2f2"/><path data-name="Path 588" d="M-2068.794 6279.286h-89.134a.33.33 0 110-.66h89.134a.33.33 0 010 .66z" fill="#3f3d56"/></g><path data-name="Path 552" d="M545.677 391.462a10.091 10.091 0 011.411.787l44.852-19.143 1.6-11.815 17.922-.11-1.059 27.1-59.199 15.658a10.6 10.6 0 01-.448 1.208 10.235 10.235 0 11-5.079-13.682z" fill="#a0616a"/><path data-name="Path 553" d="M617.98 637.021h-12.26l-5.832-47.288h18.094z" fill="#a0616a"/><path data-name="Path 554" d="M596.963 633.518h23.644V648.4h-38.531a14.887 14.887 0 0114.887-14.887z" fill="#2f2e41"/><path data-name="Path 555" d="M665.66 633.557l-12.2 1.2-10.441-46.488 18.007-1.774z" fill="#a0616a"/><path data-name="Path 556" d="M644.398 632.137l23.53-2.318 1.459 14.816-38.344 3.776a14.887 14.887 0 0113.356-16.274z" fill="#2f2e41"/><circle data-name="Ellipse 84" cx="24.561" cy="24.561" r="24.561" transform="translate(596.832 262.013)" fill="#a0616a"/><path data-name="Path 557" d="M602.98 627.458a4.471 4.471 0 01-4.415-3.7c-6.341-35.219-27.088-150.402-27.584-153.593a1.432 1.432 0 01-.016-.222v-8.588a1.489 1.489 0 01.279-.872l2.74-3.838a1.479 1.479 0 011.144-.625c15.622-.732 66.784-2.879 69.256.209 2.482 3.1 1.605 12.507 1.4 14.36l.01.193 22.985 147a4.512 4.512 0 01-3.715 5.135l-14.356 2.365a4.521 4.521 0 01-5.025-3.093c-4.44-14.188-19.329-61.918-24.489-80.387a.5.5 0 00-.981.139c.258 17.606.881 62.523 1.1 78.037l.023 1.671a4.518 4.518 0 01-4.093 4.536l-13.843 1.254c-.14.014-.276.019-.42.019z" fill="#2f2e41"/><path data-name="Path 99" d="M605.805 320.793c-4.286 2.548-6.851 7.23-8.323 12a113.683 113.683 0 00-4.884 27.159l-1.556 27.6-19.255 73.17c16.689 14.121 26.315 10.911 48.781-.639s25.032 3.851 25.032 3.851l4.492-62.258 6.418-68.032a30.169 30.169 0 00-4.862-4.674 49.659 49.659 0 00-42.442-9z" fill="#89b0ae"/><path data-name="Path 558" d="M599.551 406.239a10.527 10.527 0 011.5.7l44.348-22.2.736-12.026 18.294-1.261.98 27.413-59.266 19.6a10.5 10.5 0 11-6.593-12.232z" fill="#a0616a"/><path data-name="Path 101" d="M656.19 333.95c10.911 3.851 12.834 45.574 12.834 45.574-12.837-7.06-28.241 4.493-28.241 4.493s-3.209-10.912-7.06-25.032a24.53 24.53 0 015.134-23.106s6.421-5.782 17.333-1.929z" fill="#89b0ae"/><path data-name="Path 102" d="M643.415 293.07c-3.06-2.448-7.235 2-7.235 2l-2.448-22.031s-15.3 1.833-25.094-.612-11.323 8.875-11.323 8.875a78.583 78.583 0 01-.306-13.771c.612-5.508 8.568-11.017 22.645-14.689s21.421 12.241 21.421 12.241c9.794 4.895 5.4 30.435 2.34 27.987z" fill="#2f2e41"/><g data-name="Group 19" transform="translate(2910 -5674.784)"><path data-name="Path 561" d="M-1971.735 6182h-200.386a2.881 2.881 0 00-2.879 2.879v135.787a2.881 2.881 0 002.879 2.879h200.386a2.876 2.876 0 002.189-1.01.669.669 0 00.063-.079 2.7 2.7 0 00.413-.7 2.808 2.808 0 00.218-1.093v-135.784a2.882 2.882 0 00-2.883-2.879zm2.06 138.666a2.039 2.039 0 01-.34 1.129 2.129 2.129 0 01-.779.7 2.042 2.042 0 01-.941.228h-200.386a2.059 2.059 0 01-2.057-2.057v-135.787a2.059 2.059 0 012.057-2.057h200.386a2.06 2.06 0 012.06 2.057z" fill="#3f3d56"/><path data-name="Rectangle 99" fill="#3f3d56" d="M-2174.59 6193.538h205.323v.823h-205.323z"/><circle data-name="Ellipse 88" cx="2.469" cy="2.469" r="2.469" transform="translate(-2170.064 6185.703)" fill="#3f3d56"/><circle data-name="Ellipse 89" cx="2.469" cy="2.469" r="2.469" transform="translate(-2162.966 6185.703)" fill="#3f3d56"/><circle data-name="Ellipse 90" cx="2.469" cy="2.469" r="2.469" transform="translate(-2155.868 6185.703)" fill="#3f3d56"/><path data-name="Path 583" d="M-2100.846 6234.843h-51.916a2.525 2.525 0 010-5.049h51.916a2.525 2.525 0 010 5.049z" fill="#89b0ae"/><path data-name="Path 584" d="M-2068.794 6236.997h-89.134a.33.33 0 110-.66h89.134a.33.33 0 010 .66z" fill="#3f3d56"/><path data-name="Path 585" d="M-2100.846 6255.989h-51.916a2.525 2.525 0 010-5.049h51.916a2.525 2.525 0 010 5.049z" fill="#89b0ae"/><path data-name="Path 586" d="M-2068.794 6258.142h-89.134a.33.33 0 110-.66h89.134a.33.33 0 110 .66z" fill="#3f3d56"/><path data-name="Path 587" d="M-2100.846 6277.132h-51.916a2.525 2.525 0 010-5.049h51.916a2.525 2.525 0 010 5.049z" fill="#f2f2f2"/><path data-name="Path 588" d="M-2068.794 6279.286h-89.134a.33.33 0 110-.66h89.134a.33.33 0 010 .66z" fill="#3f3d56"/></g><path data-name="Path 561" d="M531.868 371.216H291.454A3.457 3.457 0 00288 374.67v162.911a3.457 3.457 0 003.454 3.454h240.414a3.451 3.451 0 002.626-1.212.8.8 0 00.075-.095 3.236 3.236 0 00.5-.836 3.37 3.37 0 00.261-1.311V374.67a3.457 3.457 0 00-3.462-3.454zm2.472 166.365a2.445 2.445 0 01-.408 1.355 2.554 2.554 0 01-.935.84 2.45 2.45 0 01-1.129.273H291.454a2.47 2.47 0 01-2.467-2.467V374.67a2.47 2.47 0 012.467-2.467h240.414a2.471 2.471 0 012.472 2.468z" fill="#3f3d56"/><path data-name="Rectangle 99" fill="#3f3d56" d="M288.492 385.058H534.83v.987H288.492z"/><circle data-name="Ellipse 88" cx="2.962" cy="2.962" r="2.962" transform="translate(293.922 375.659)" fill="#3f3d56"/><circle data-name="Ellipse 89" cx="2.962" cy="2.962" r="2.962" transform="translate(302.438 375.659)" fill="#3f3d56"/><circle data-name="Ellipse 90" cx="2.962" cy="2.962" r="2.962" transform="translate(310.954 375.659)" fill="#3f3d56"/><path data-name="Path 589" d="M477.415 471.109h-82.26a3.739 3.739 0 110-7.478h82.26a3.739 3.739 0 010 7.478z" fill="#f2f2f2"/><path data-name="Path 590" d="M423.666 455.218h-28.51a3.739 3.739 0 110-7.478h28.51a3.739 3.739 0 010 7.478z" fill="#f2f2f2"/><circle data-name="Ellipse 91" cx="15.891" cy="15.891" transform="translate(342.006 443.883)" fill="#89b0ae" r="15.891"/><path data-name="Path 591" d="M485.66 435.47H337.5a7.957 7.957 0 00-7.946 7.946v32.717a7.957 7.957 0 007.945 7.945H485.66a7.957 7.957 0 007.945-7.946v-32.718a7.957 7.957 0 00-7.945-7.946zm7.011 40.662a7.019 7.019 0 01-7.011 7.011H337.5a7.019 7.019 0 01-7.011-7.011v-32.718a7.019 7.019 0 017.011-7.011h148.16a7.019 7.019 0 017.011 7.011z" fill="#3f3d56"/></g></svg>
  """

research :: Icons.Icon
research = icon
  """
  <svg data-name="Layer 1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 802.027 590.916"><path fill="#e6e6e6" d="M464.906 218.808h337.121v9.889H464.906z"/><path fill="#e6e6e6" d="M492.774 136.101h85.404v85.404h-85.404z"/><path fill="#ccc" d="M509.855 153.182h51.242v51.242h-51.242z"/><path fill="#e6e6e6" d="M684.259 136.101h21.576v85.404h-21.576z"/><path fill="#ccc" d="M684.259 149.586h21.576v9.889h-21.576zM684.259 192.737h21.576v9.889h-21.576z"/><path fill="#e6e6e6" d="M650.997 136.101h21.576v85.404h-21.576z"/><path fill="#ccc" d="M650.997 149.586h21.576v9.889h-21.576zM650.997 192.737h21.576v9.889h-21.576z"/><path fill="#e6e6e6" d="M705.09 145.321l19.335-9.573 37.893 76.537-19.336 9.573z"/><path fill="#ccc" d="M711.072 157.405l19.336-9.573 4.388 8.862-19.336 9.573zM730.219 196.077l19.336-9.573 4.387 8.862-19.336 9.573z"/><path fill="#e6e6e6" d="M524.688 170.382l10.866 18.819 10.865 18.819h-43.461l10.865-18.819 10.865-18.819z"/><path fill="#e6e6e6" d="M543.567 173.978l10.865 18.819 10.866 18.819h-43.461l10.865-18.819 10.865-18.819z"/><circle cx="551.209" cy="162.171" r="6.293" fill="#e6e6e6"/><path fill="#e6e6e6" d="M802.027 92.95H464.906v-9.89h337.121z"/><path fill="#e6e6e6" d="M774.158 85.757h-85.404V.353h85.404z"/><path fill="#ccc" d="M757.078 68.676h-51.242V17.434h51.242z"/><path fill="#e6e6e6" d="M582.673 85.757h-21.576V.353h21.576z"/><path fill="#ccc" d="M582.673 23.727h-21.576v-9.89h21.576zM582.673 66.879h-21.576v-9.89h21.576z"/><path fill="#e6e6e6" d="M615.936 85.757H594.36V.353h21.576z"/><path fill="#ccc" d="M615.936 23.727H594.36v-9.89h21.576zM615.936 66.879H594.36v-9.89h21.576z"/><path fill="#e6e6e6" d="M523.95 86.11l-19.335-9.572L542.508 0l19.335 9.573z"/><path fill="#ccc" d="M551.473 30.52l-19.336-9.573 4.387-8.862 19.336 9.573zM532.326 69.192l-19.336-9.573 4.388-8.862 19.336 9.573z"/><path fill="#e6e6e6" d="M742.244 34.634l-10.865 18.819-10.865 18.819h43.461l-10.866-18.819-10.865-18.819z"/><path fill="#e6e6e6" d="M723.365 38.23L712.5 57.049l-10.865 18.819h43.461l-10.865-18.819-10.866-18.819z"/><circle cx="715.724" cy="26.424" r="6.293" fill="#e6e6e6"/><path fill="#e6e6e6" d="M802.027 364.445H464.906v-9.89h337.121z"/><path fill="#e6e6e6" d="M774.158 357.253h-85.404v-85.404h85.404z"/><path fill="#ccc" d="M757.078 340.172h-51.242V288.93h51.242z"/><path fill="#e6e6e6" d="M582.673 357.253h-21.576v-85.404h21.576z"/><path fill="#ccc" d="M582.673 295.222h-21.576v-9.889h21.576zM582.673 338.373h-21.576v-9.889h21.576z"/><path fill="#e6e6e6" d="M615.936 357.253H594.36v-85.404h21.576z"/><path fill="#ccc" d="M615.936 295.222H594.36v-9.889h21.576zM615.936 338.373H594.36v-9.889h21.576z"/><path fill="#e6e6e6" d="M523.95 357.606l-19.336-9.573 37.893-76.538 19.336 9.574z"/><path fill="#ccc" d="M551.472 302.015l-19.336-9.573 4.388-8.862 19.336 9.573zM532.327 340.686l-19.336-9.573 4.387-8.862 19.336 9.573z"/><path fill="#e6e6e6" d="M742.244 306.129l-10.865 18.819-10.865 18.82h43.461l-10.866-18.82-10.865-18.819z"/><path fill="#e6e6e6" d="M723.365 309.725L712.5 328.544l-10.865 18.82h43.461l-10.865-18.82-10.866-18.819z"/><circle cx="715.724" cy="297.919" r="6.293" fill="#e6e6e6"/><ellipse cx="397.129" cy="562.711" rx="397.129" ry="28.205" fill="#e6e6e6"/><path d="M707.387 578.506c-64.755-11.525-135.568-9.61-209.847 0 33.855-28.581 63.075-57.163 29.22-85.744 66.038 13.624 75.288 12.245 148.751 0-17.039 28.581 14.836 57.163 31.876 85.744z" fill="#89b0ae"/><path d="M707.387 578.506c-64.755-11.525-135.568-9.61-209.847 0 33.855-28.581 63.075-57.163 29.22-85.744 66.038 13.624 75.288 12.245 148.751 0-17.039 28.581 14.836 57.163 31.876 85.744z" opacity=".2"/><path d="M691.592 566.312c-55.007-9.2-115.16-7.67-178.257 0 28.759-22.814 53.58-45.628 24.82-68.442 56.098 10.875 63.956 9.774 126.36 0-14.474 22.814 12.603 45.628 27.077 68.442z" fill="#89b0ae"/><circle cx="670.64" cy="500.244" r="29.309" fill="#2f2e41"/><path fill="#a0616a" d="M537.591 466.249l20.308 3.385 10.154 29.333-29.333 18.052-33.847-50.77H537.591z"/><circle cx="595.13" cy="463.993" r="47.385" fill="#a0616a"/><path d="M333.386 291.377S218.308 303.787 198 301.53s-29.334-4.513-29.334-4.513-13.538 18.051-6.769 25.949a48.822 48.822 0 0013.538 11.282s12.41-3.385 21.436 0 100.411 18.051 116.206 4.513 20.308-47.385 20.308-47.385z" fill="#2f2e41"/><path d="M341.283 542.967s-75.59-21.436-89.128-116.205v-9.026s-14.957 26.239-20.308 31.59c-6.205 6.205-28.205 62.052-27.077 67.693s0 7.897 0 7.897l-32.718-3.384v-10.154s17.487-52.462 18.615-64.872 43.436-98.719 43.436-98.719 15.795-32.718 42.872 0 29.334 55.283 29.334 55.283l34.974 67.692zM172.052 293.633l-36.103-12.41s-33.846-24.82-25.949 0 44 78.975 54.154 73.334 22.366-19.644 19.645-21.668-20.773-16.691-11.747-39.256z" fill="#2f2e41"/><path d="M176.416 504.717l-29.144 14.233s-35.244 5.422-15.589 15.588 73.876 21.011 76.588 11.522 1.635-25.236-1.216-24.817-22.506 2.451-30.64-16.526z" fill="#2f2e41"/><path d="M326.616 468.506s-14.666 63.18 10.154 76.718 198.565 15.795 212.103-9.026 6.77-24.82 6.77-24.82l-36.103-40.616 29.333-2.256s0-9.026-10.153-10.154-41.744-12.41-68.821-4.513-42.872-15.795-42.872-15.795z" fill="#575a89"/><path fill="#3f3d56" d="M307.437 271.069l-19.18 104.924 132.001-15.795 23.692-94.77-136.513 5.641z"/><path fill="#fff" d="M311.386 275.018l-15.795 93.641 120.154-14.102 23.128-85.18-127.487 5.641z"/><path fill="#b3b3b3" d="M292.77 377.121l-4.513-1.128-2.256 4.512 36.103 100.411 3.395-1.298 1.117-6.599-33.846-95.898z"/><path fill="#d0cde1" d="M288.257 374.864l36.103 104.924 136.513-20.308-40.615-99.282-132.001 14.666z"/><path fill="#3f3d56" d="M306.309 371.48v5.641l100.41-11.282-1.128-5.641-99.282 11.282zM308.565 382.762l15.795 44 102.667-12.41-18.051-41.744-100.411 10.154z"/><path d="M437.18 444.813l-3.384-9.025s-25.949-45.129-39.487-36.103 28.205 53.026 28.205 53.026h13.539zM351.437 451.583l-3.385-13.539s-9.025-41.744 6.77-39.487 20.307 41.743 20.307 41.743l-1.128 9.026z" fill="#a0616a"/><path d="M378.514 445.942s-29.333-4.513-30.462 2.256-6.769 108.308 21.436 111.693 168.103 18.05 153.437-16.924-37.231-30.461-37.231-30.461l-95.898 10.154zM467.078 458.916l-29.897-20.872-19.744 15.231 24.821 27.077 24.82-21.436z" fill="#575a89"/><path opacity=".2" d="M386.976 479.224l4.512 42.872 77.847-4.513-67.693-1.128-14.666-37.231z"/><circle cx="618.521" cy="472.603" r="56.41" fill="#2f2e41"/><path fill="#89b0ae" d="M328.309 307.736l69.949-2.435 6.769-25.77-72.205 2.179-4.513 26.026z"/><path fill="#e6e6e6" d="M310.258 322.403l106.051-3.385 1.128-4.513-106.051 3.734-1.128 4.164zM308.001 333.685l106.052-3.385 1.128-4.513-106.052 3.734-1.128 4.164zM306.873 342.71l106.051-3.384 1.129-4.513-106.052 3.733-1.128 4.164z"/></svg>
  """

bookmark :: Icons.Icon
bookmark = icon
  """
  <svg data-name="Layer 1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 737.913 622.439"><path d="M737.913 483.928c-.155 3.822-4.02 82.723-57.024 125.835q-3.325 2.704-6.909 5.21l-1.802-1.76h1.046a4.662 4.662 0 004.661-4.662V453.028a980.426 980.426 0 003.108-17.537c5.169-31.023 7.562-57.23 3.687-68.117 1.679 4.205 35.365 90.222 17.071 177.866 18.117-24.809 35.313-50.85 36.162-61.312z" fill="#89b0ae"/><path d="M671.971 9.56H33.745a7.778 7.778 0 00-7.768 7.77v591.472a7.772 7.772 0 007.768 7.769h638.226a7.751 7.751 0 007.666-6.557 6.683 6.683 0 00.103-1.212V17.33a7.778 7.778 0 00-7.769-7.77zm4.662 599.242a4.662 4.662 0 01-4.662 4.661H33.745a4.662 4.662 0 01-4.66-4.661V17.33a4.668 4.668 0 014.66-4.662h638.226a4.668 4.668 0 014.662 4.662z" fill="#e6e6e6"/><path d="M374.079 68.02c-1.3 0-2.358 2.256-2.358 5.03v207.96c0 2.774 1.058 5.031 2.358 5.031h225.564c1.3 0 2.357-2.257 2.357-5.031V73.05c0-2.774-1.057-5.03-2.357-5.03z" fill="#e6e6e6"/><path data-name="Path 40" d="M107.63 67.619a3.755 3.755 0 000 7.509h177.004a3.755 3.755 0 10.123-7.51h-.123zM107.63 102.838a3.755 3.755 0 000 7.509h177.004a3.755 3.755 0 10.123-7.51h-.123zM107.63 138.057a3.755 3.755 0 000 7.509h177.004a3.755 3.755 0 10.123-7.51h-.123zM107.63 173.276a3.755 3.755 0 000 7.509h177.004a3.755 3.755 0 00.123-7.51h-.123zM107.63 208.495a3.755 3.755 0 000 7.509h177.004a3.755 3.755 0 00.123-7.51h-.123zM107.63 243.714a3.755 3.755 0 000 7.509h177.004a3.755 3.755 0 10.123-7.51h-.123zM107.63 278.933a3.755 3.755 0 000 7.509h177.004a3.755 3.755 0 10.123-7.51h-.123zM108.266 353.004c-2.554.034-4.59 1.742-4.549 3.816.04 2.025 2.054 3.66 4.549 3.693h489.033c2.554.034 4.659-1.62 4.7-3.693s-1.994-3.782-4.548-3.816h-.152zM108.266 386.004c-2.554.034-4.59 1.742-4.549 3.816.04 2.025 2.054 3.66 4.549 3.693h489.033c2.554.034 4.659-1.62 4.7-3.693s-1.994-3.782-4.548-3.816h-.152zM108.266 419.004c-2.554.034-4.59 1.742-4.549 3.816.04 2.025 2.054 3.66 4.549 3.693h489.033c2.554.034 4.659-1.62 4.7-3.693s-1.994-3.782-4.548-3.816h-.152zM108.266 452.004c-2.554.034-4.59 1.742-4.549 3.816.04 2.025 2.054 3.66 4.549 3.693h489.033c2.554.034 4.659-1.62 4.7-3.693s-1.994-3.782-4.548-3.816h-.152zM108.266 485.004c-2.554.034-4.59 1.742-4.549 3.816.04 2.025 2.054 3.66 4.549 3.693h489.033c2.554.034 4.659-1.62 4.7-3.693s-1.994-3.782-4.548-3.816h-.152zM108.266 518.004c-2.554.034-4.59 1.742-4.549 3.816.04 2.025 2.054 3.66 4.549 3.693h489.033c2.554.034 4.659-1.62 4.7-3.693s-1.994-3.782-4.548-3.816h-.152zM108.266 551.004c-2.554.034-4.59 1.742-4.549 3.816.04 2.025 2.054 3.66 4.549 3.693h489.033c2.554.034 4.659-1.62 4.7-3.693s-1.994-3.782-4.548-3.816h-.152z" fill="#e6e6e6"/><ellipse cx="301.043" cy="755.879" rx="70" ry="5.339" transform="rotate(-.092 -86281.697 144638.377)" fill="#e6e6e6"/><path d="M61.855 418.607l-3.11 18.57a10.604 10.604 0 01-14.604 8.01 10.604 10.604 0 01-5.41-14.361l7.288-15.138L56.627 343.6l19.553 5.25zM46.613 579.563l-15.425 2.685 13.05-104.885 25.093 2.137-22.718 100.063zM110.992 579.46l-15.425 2.685 13.05-104.885 25.093 2.136-22.718 100.064z" fill="#ffb8b8"/><circle cx="99.192" cy="268.646" r="19.357" fill="#ffb8b8"/><path fill="#ffb8b8" d="M100.977 306.53l-22.35-2.092 3.137-34.589 26.071-.042-6.858 36.723z"/><path d="M137.988 492.691L37.409 480.615c2.185-19.1 5.457-36.376 10.256-51.089 7.173-21.995 17.757-38.264 33.217-46.347l-.028-17.558-8.397-53.587a13.037 13.037 0 0112.86-15.056l17.243 1.569 21.732 26.066a34.953 34.953 0 017.836 26.722l-7.571 60.505z" fill="#2f2e41"/><path d="M80.846 356.796L49.66 346.04l12.532-36.334a14.431 14.431 0 0111.75-9.601 14.431 14.431 0 0115.952 17.558zM58.938 616.603c-12.798-.327-9.765 1.455-12.798-.327-8.97-5.27-12.655-11.765-13.83 1.618l-4.788.54c-4.228-14.353-3.037-25.327 2.606-33.524l1.584-7.451 15.428-1.089c1.367 14.4 7.634 25.87 14.496 30.87a5.328 5.328 0 011.781 2.181 5.15 5.15 0 01-4.48 7.182zM123.316 616.5c-12.797-.327-9.765 1.454-12.797-.327-8.97-5.27-12.655-11.765-13.83 1.618l-4.789.54c-4.227-14.354-3.036-25.327 2.607-33.524l1.584-7.451 15.428-1.09c1.367 14.4 7.634 25.87 14.496 30.87a5.328 5.328 0 011.781 2.182 5.15 5.15 0 01-4.48 7.182zM112.055 264.026l9.862-4.841-.002-1.197c-.016-10.428-7.996-18.962-18.029-19.282s-20.227-11.235-34.131 7.486-6.415 51.088-6.415 51.088l20.857-3.226 1.188-7.064 3.494 6.63 12.192-.623s2.188-14.29 1.708-21.114c-.713-10.11 7.19-9.932 9.276-7.857z" fill="#2f2e41"/><path d="M127.783 433.117l2.823 18.616a10.604 10.604 0 01-11.386 12.156 10.604 10.604 0 01-9.61-11.964l2.215-16.654-12.353-71.808 20.217-1.097z" fill="#ffb8b8"/><path d="M123.95 365.552l-32.987.053-.062-38.435a14.431 14.431 0 017.957-12.92 14.431 14.431 0 0120.823 11.364z" fill="#2f2e41"/><path d="M239.093 151.195a2.19 2.19 0 01-.89-.182 2.144 2.144 0 01-1.31-1.97L235.815 3.06a2.172 2.172 0 012.148-2.19L326.297 0a2.172 2.172 0 012.191 2.148l1.078 145.982a2.17 2.17 0 01-3.61 1.645l-42.601-37.81a.719.719 0 00-.971.01l-41.85 38.64a2.147 2.147 0 01-1.441.58z" fill="#89b0ae"/><path d="M298.21 50.372l-13.187.316-.316-13.187a4.397 4.397 0 10-8.792.211l.317 13.187-13.187.316a4.397 4.397 0 00.21 8.792l13.188-.317.316 13.187a4.397 4.397 0 008.791-.21l-.316-13.188 13.187-.316a4.397 4.397 0 10-.211-8.791z" fill="#fff"/></svg>
  """
