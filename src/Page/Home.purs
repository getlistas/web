module Listasio.Page.Home where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Lens (over)
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
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Capability.Resource.User (class ManageUser, getCurrentUser)
import Listasio.Component.HTML.Footer (footer)
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Login as Login
import Listasio.Component.HTML.Logo as Logo
import Listasio.Component.HTML.Register as Register
import Listasio.Component.HTML.Typed as Typed
import Listasio.Component.HTML.Utils (safeHref, whenElem)
import Listasio.Data.Avatar as Avatar
import Listasio.Data.Lens (_menuOpen)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Route (Route(..))
import Listasio.Data.Username as Username
import Listasio.Store as Store
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

type State
  = { currentUser :: Maybe ProfileWithIdAndEmail
    , menuOpen :: Boolean
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
    , menuOpen: false
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
                  , desktopLink Login "Sing in"
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
              , HH.div
                  [ HP.classes [ T.px2, T.pt2, T.pb3, T.spaceY1 ] ]
                  [ whenElem (hasUser authStatus) \_ ->
                      mobileLink Dashboard "Up next"
                  , whenElem (hasUser authStatus) \_ ->
                      mobileLink History "History"
                  , mobileLink Discover "Discover"
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
          , Icons.reading [ Icons.classes [ T.wFull, T.hFull ] ]
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
