module Listasio.Page.Home where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Lens (over)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Utils (safeHref, whenElem)
import Listasio.Data.Lens (_menuOpen)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Route (Route(..))
import Listasio.Env (UserEnv)
import Tailwind as T
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent as Mouse

data Action
  = Initialize
  | Receive { currentUser :: Maybe ProfileWithIdAndEmail }
  | Navigate Route Event
  | ToggleMenu

type State
  = { currentUser :: Maybe ProfileWithIdAndEmail
    , menuOpen :: Boolean
    }

component
  :: forall q o m r
   . MonadAff m
  => MonadAsk { userEnv :: UserEnv | r } m
  => Navigate m
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
  initialState {currentUser} = {currentUser, menuOpen: false}

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> pure unit

    Receive {currentUser} ->
      H.modify_ _ {currentUser = currentUser}

    Navigate route e -> navigate_ e route

    ToggleMenu -> H.modify_ $ over _menuOpen not

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render {currentUser, menuOpen} =
    HH.div
      [ HP.classes [ T.bgWhite ] ]
      [ home2
      , splitImageAndFeature
      , featureCardWithImage
      , featuresList
      , footer
      ]

    where
    logo =
      HH.a
        [ HP.classes [ T.border2, T.py3, T.px2, T.borderKiwi, T.block ]
        , safeHref Home
        , HE.onClick $ Just <<< Navigate Home <<< Mouse.toEvent
        ]
        [ HH.h1
            [ HP.classes [ T.text2xl, T.leadingNone, T.textGray400 ] ]
            [ HH.text "Listas" ]
        ]

    home2 =
      HH.div
        [ HP.classes [ T.relative, T.bgGray10, T.overflowHidden ] ]
        [ HH.div
            [ HP.classes [ T.hidden, T.smBlock, T.smAbsolute, T.smInset0 ] ]
            [ Icons.mesh
                [ Icons.classes
                    [ T.absolute
                    , T.bottom0
                    , T.right0
                    , T.transform
                    , T.translateX1d2
                    , T.mb48
                    , T.textGray300
                    , T.lgTop0
                    , T.lgMt28
                    , T.lgMb0
                    , T.xlTransformNone
                    , T.xlTranslateX0
                    ]
                , HP.attr (HH.AttrName "height") $ show 384
                , HP.attr (HH.AttrName "width") $ show 364
                ]
            ]
        , HH.div
            [ HP.classes [ T.relative, T.pt6, T.pb16, T.smPb24 ] ]
            [ HH.nav
                [ HP.classes
                    [ T.relative
                    , T.maxW7xl
                    , T.mxAuto
                    , T.flex
                    , T.itemsCenter
                    , T.justifyBetween
                    , T.px4
                    , T.smPx6
                    ]
                ]
                [ HH.div
                    [ HP.classes [ T.flex, T.itemsCenter, T.flex1 ] ]
                    [ HH.div
                        [ HP.classes [ T.flex, T.itemsCenter, T.justifyBetween, T.wFull, T.mdWAuto ] ]
                        [ logo
                        , HH.div
                            [ HP.classes [ T.negMr2, T.flex, T.itemsCenter, T.mdHidden ] ]
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
                        [ HH.a
                            [ HP.classes [ T.fontMedium, T.textGray300, T.hoverTextKiwi ], HP.href "#" ]
                            [ HH.text "Product" ]
                        , HH.a
                            [ HP.classes [ T.fontMedium, T.textGray300, T.hoverTextKiwi ], HP.href "#" ]
                            [ HH.text "Features" ]
                        , HH.a
                            [ HP.classes [ T.fontMedium, T.textGray300, T.hoverTextKiwi ], HP.href "#" ]
                            [ HH.text "Marketplace" ]
                        , HH.a
                            [ HP.classes [ T.fontMedium, T.textGray300, T.hoverTextKiwi ], HP.href "#" ]
                            [ HH.text "Company" ]
                        ]
                    ]
                , HH.div
                    [ HP.classes [ T.hidden, T.mdFlex ] ]
                    [ HH.a
                        [ HP.classes
                            [ T.inlineFlex
                            , T.itemsCenter
                            , T.px4
                            , T.py2
                            , T.border
                            , T.borderTransparent
                            , T.textSm
                            , T.fontMedium
                            , T.roundedMd
                            , T.textWhite
                            , T.bgGray600
                            , T.hoverBgGray700
                            ]
                        , HP.href "#"
                        ]
                        [ HH.text "Log in" ]
                    ]
                ]
            , whenElem menuOpen \_ ->
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
                          [ HH.a
                              [ HP.classes
                                  [ T.block
                                  , T.px3
                                  , T.py2
                                  , T.roundedMd
                                  , T.textBase
                                  , T.fontMedium
                                  , T.textGray700
                                  , T.hoverTextGray900
                                  , T.hoverBgGray50
                                  ]
                              , HP.href "#"
                              ]
                              [ HH.text "Product" ]
                          , HH.a
                              [ HP.classes
                                  [ T.block
                                  , T.px3
                                  , T.py2
                                  , T.roundedMd
                                  , T.textBase
                                  , T.fontMedium
                                  , T.textGray700
                                  , T.hoverTextGray900
                                  , T.hoverBgGray50
                                  ]
                              , HP.href "#"
                              ]
                              [ HH.text "Features" ]
                          , HH.a
                              [ HP.classes
                                  [ T.block
                                  , T.px3
                                  , T.py2
                                  , T.roundedMd
                                  , T.textBase
                                  , T.fontMedium
                                  , T.textGray700
                                  , T.hoverTextGray900
                                  , T.hoverBgGray50
                                  ]
                              , HP.href "#"
                              ]
                              [ HH.text "Marketplace" ]
                          , HH.a
                              [ HP.classes
                                  [ T.block
                                  , T.px3
                                  , T.py2
                                  , T.roundedMd
                                  , T.textBase
                                  , T.fontMedium
                                  , T.textGray700
                                  , T.hoverTextGray900
                                  , T.hoverBgGray50
                                  ]
                              , HP.href "#"
                              ]
                              [ HH.text "Company" ]
                          ]
                      , HH.a
                          [ HP.classes
                              [ T.block
                              , T.wFull
                              , T.px5
                              , T.py3
                              , T.textCenter
                              , T.fontMedium
                              , T.textKiwi
                              , T.bgGray10
                              , T.hoverBgGray100
                              ]
                          , HP.href "#"
                          ]
                          [ HH.text "Log in" ]
                      ]
                  ]
            , HH.main
                [ HP.classes [ T.mt16, T.smMt24 ] ]
                [ HH.div
                    [ HP.classes [ T.mxAuto, T.maxW7xl ] ]
                    [ HH.div
                        [ HP.classes [ T.lgGrid, T.lgGridCols12, T.lgGap8 ] ]
                        [ HH.div
                            [ HP.classes
                                [ T.px4
                                , T.smPx6
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
                                        [ HH.text "Digital content" ]
                                    , HH.span
                                        [ HP.classes [ T.textKiwi, T.block ] ]
                                        [ HH.text "consumption manager" ]
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
                                    [ HH.text "Keep your reading and watching lists under control with Listas" ]
                                ]
                            ]
                        , HH.div
                            [ HP.classes [ T.mt16, T.smMt24, T.lgMt0, T.lgColSpan6 ] ]
                            [ HH.div
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
                                    [ HH.div
                                        []
                                        [ HH.a
                                            [ HP.classes
                                                [ T.wFull
                                                , T.inlineFlex
                                                , T.justifyCenter
                                                , T.py2
                                                , T.px4
                                                , T.border
                                                , T.borderGray300
                                                , T.roundedMd
                                                , T.shadowSm
                                                , T.bgWhite
                                                , T.textSm
                                                , T.fontMedium
                                                , T.textGray500
                                                , T.hoverBgGray50
                                                ]
                                            , HP.href "#"
                                            ]
                                            [ HH.span
                                                [ HP.classes [ T.srOnly ] ]
                                                [ HH.text "Sign in with Google" ]
                                            , Icons.google [ Icons.classes [ T.w5, T.h5 ] ]
                                            ]
                                        ]
                                    , HH.div
                                        [ HP.classes [ T.mt6, T.relative ] ]
                                        [ HH.div
                                            [ HP.classes [ T.absolute, T.inset0, T.flex, T.itemsCenter ] ]
                                            [ HH.div
                                                [ HP.classes [ T.wFull, T.borderT, T.borderGray300 ] ]
                                                []
                                            ]
                                        , HH.div
                                            [ HP.classes [ T.relative, T.flex, T.justifyCenter, T.textSm ] ]
                                            [ HH.span
                                                [ HP.classes [ T.px2, T.bgWhite, T.textGray500 ] ]
                                                [ HH.text "Or" ]
                                            ]
                                        ]
                                    , HH.div
                                        [ HP.classes [ T.mt6 ] ]
                                        [ HH.form
                                            [ HP.classes [ T.spaceY6 ] ]
                                            [ HH.div
                                                []
                                                [ HH.label
                                                    [ HP.classes [ T.srOnly ], HP.for "name" ]
                                                    [ HH.text "Full name" ]
                                                , HH.input
                                                    [ HP.classes
                                                        [ T.block
                                                        , T.wFull
                                                        , T.shadowSm
                                                        , T.focusRingIndigo500
                                                        , T.focusBorderIndigo500
                                                        , T.smTextSm
                                                        , T.borderGray300
                                                        , T.roundedMd
                                                        ]
                                                    , HP.required true
                                                    , HP.placeholder "Full name"
                                                    , HP.autocomplete true
                                                    , HP.id_ "name"
                                                    , HP.name "name"
                                                    , HP.type_ HP.InputText
                                                    ]
                                                ]
                                            , HH.div
                                                []
                                                [ HH.label
                                                    [ HP.classes [ T.srOnly ], HP.for "mobile-or-email" ]
                                                    [ HH.text "Mobile number or email" ]
                                                , HH.input
                                                    [ HP.classes
                                                        [ T.block
                                                        , T.wFull
                                                        , T.shadowSm
                                                        , T.focusRingIndigo500
                                                        , T.focusBorderIndigo500
                                                        , T.smTextSm
                                                        , T.borderGray300
                                                        , T.roundedMd
                                                        ]
                                                    , HP.required true
                                                    , HP.placeholder "Mobile number or email"
                                                    , HP.autocomplete true
                                                    , HP.id_ "mobile-or-email"
                                                    , HP.name "mobile-or-email"
                                                    , HP.type_ HP.InputText
                                                    ]
                                                ]
                                            , HH.div
                                                []
                                                [ HH.label
                                                    [ HP.classes [ T.srOnly ], HP.for "password" ]
                                                    [ HH.text "Password" ]
                                                , HH.input
                                                    [ HP.classes
                                                        [ T.block
                                                        , T.wFull
                                                        , T.shadowSm
                                                        , T.focusRingIndigo500
                                                        , T.focusBorderIndigo500
                                                        , T.smTextSm
                                                        , T.borderGray300
                                                        , T.roundedMd
                                                        ]
                                                    , HP.required true
                                                    , HP.autocomplete true
                                                    , HP.placeholder "Password"
                                                    , HP.type_ HP.InputPassword
                                                    , HP.name "password"
                                                    , HP.id_ "password"
                                                    ]
                                                ]
                                            , HH.div
                                                []
                                                [ HH.button
                                                    [ HP.classes
                                                        [ T.wFull
                                                        , T.flex
                                                        , T.justifyCenter
                                                        , T.py2
                                                        , T.px4
                                                        , T.border
                                                        , T.borderTransparent
                                                        , T.roundedMd
                                                        , T.shadowSm
                                                        , T.textSm
                                                        , T.fontMedium
                                                        , T.textWhite
                                                        , T.bgKiwi
                                                        , T.hoverBgKiwiDark
                                                        , T.focusOutlineNone
                                                        , T.focusRing2
                                                        , T.focusRingOffset2
                                                        , T.focusRingKiwi
                                                        ]
                                                    , HP.type_ HP.ButtonSubmit
                                                    ]
                                                    [ HH.text "Create your account" ]
                                                ]
                                            ]
                                        ]
                                    ]
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
                                            [ HP.classes [ T.fontMedium, T.textGray900, T.hoverUnderline ], HP.href "#" ]
                                            [ HH.text "Terms" ]
                                        , HH.text ", "
                                        , HH.a
                                            [ HP.classes [ T.fontMedium, T.textGray900, T.hoverUnderline ], HP.href "#" ]
                                            [ HH.text "Data Policy" ]
                                        , HH.text " and "
                                        , HH.a
                                            [ HP.classes [ T.fontMedium, T.textGray900, T.hoverUnderline ], HP.href "#" ]
                                            [ HH.text "Cookies Policy" ]
                                        , HH.text "."
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
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
                , T.px4
                , T.textCenter
                , T.smMaxW3xl
                , T.smPx6
                , T.lgPx8
                , T.lgMaxW7xl
                ]
            ]
            [ HH.h2
                [ HP.classes [ T.textBase, T.fontSemibold, T.trackingWider, T.textDurazno, T.uppercase ] ]
                [ HH.text "Deploy faster" ]
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
                [ HH.text "Everything you need to deploy your app" ]
            , HH.p
                [ HP.classes [ T.mt5, T.maxWProse, T.mxAuto, T.textXl, T.textGray500 ] ]
                [ HH.text "Phasellus lorem quam molestie id quisque diam aenean nulla in. Accumsan in quis quis nunc, ullamcorper malesuada. Eleifend condimentum id viverra nulla." ]
            , HH.div
                [ HP.classes [ T.mt12 ] ]
                [ HH.div
                    [ HP.classes [ T.grid, T.gridCols1, T.gap8, T.smGridCols2, T.lgGridCols3 ] ]
                    [ HH.div
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
                                        [ Icons.rss [ Icons.classes [ T.h6, T.w6, T.textWhite ] ] ]
                                    ]
                                , HH.h3
                                    [ HP.classes [ T.mt8, T.textLg, T.fontMedium, T.textGray400, T.trackingTight ] ]
                                    [ HH.text "RSS Subscription" ]
                                , HH.p
                                    [ HP.classes [ T.mt5, T.textBase, T.textGray500 ] ]
                                    [ HH.text "Ac tincidunt sapien vehicula erat auctor pellentesque rhoncus. Et magna sit morbi lobortis." ]
                                ]
                            ]
                        ]
                    , HH.div
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
                                        [ Icons.userGroup
                                            [ Icons.classes [ T.h6, T.w6, T.textWhite ] ]
                                        ]
                                    ]
                                , HH.h3
                                    [ HP.classes [ T.mt8, T.textLg, T.fontMedium, T.textGray400, T.trackingTight ] ]
                                    [ HH.text "Follow other lists" ]
                                , HH.p
                                    [ HP.classes [ T.mt5, T.textBase, T.textGray500 ] ]
                                    [ HH.text "Ac tincidunt sapien vehicula erat auctor pellentesque rhoncus. Et magna sit morbi lobortis." ]
                                ]
                            ]
                        ]
                    , HH.div
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
                                        [ Icons.lineChart [ Icons.classes [ T.h6, T.w6, T.textWhite ] ]
                                        ]
                                    ]
                                , HH.div
                                    [ HP.classes [ T.mt8, T.flex, T.justifyCenter ] ]
                                    [ HH.h3
                                        [ HP.classes [ T.textLg, T.fontMedium, T.textGray400, T.trackingTight ] ]
                                        [ HH.text "Reading stats" ]
                                    , HH.span
                                        [ HP.classes [ T.ml2, T.textXs, T.fontSemibold, T.textKiwi ] ]
                                        [ HH.text "SOON" ]
                                    ]
                                , HH.p
                                    [ HP.classes [ T.mt5, T.textBase, T.textGray500 ] ]
                                    [ HH.text "Ac tincidunt sapien vehicula erat auctor pellentesque rhoncus. Et magna sit morbi lobortis." ]
                                ]
                            ]
                        ]
                    , HH.div
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
                                        [ Icons.chat [ Icons.classes [ T.h6, T.w6, T.textWhite ] ]
                                        ]
                                    ]
                                , HH.div
                                    [ HP.classes [ T.mt8, T.flex, T.justifyCenter ] ]
                                    [ HH.h3
                                        [ HP.classes [ T.textLg, T.fontMedium, T.textGray400, T.trackingTight ] ]
                                        [ HH.text "Discover other lists" ]
                                    , HH.span
                                        [ HP.classes [ T.ml2, T.textXs, T.fontSemibold, T.textKiwi ] ]
                                        [ HH.text "SOON" ]
                                    ]
                                , HH.p
                                    [ HP.classes [ T.mt5, T.textBase, T.textGray500 ] ]
                                    [ HH.text "Ac tincidunt sapien vehicula erat auctor pellentesque rhoncus. Et magna sit morbi lobortis." ]
                                ]
                            ]
                        ]
                    , HH.div
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
                                        [ Icons.search
                                            [ Icons.classes [ T.h6, T.w6, T.textWhite ] ]
                                        ]
                                    ]
                                , HH.div
                                    [ HP.classes [ T.mt8, T.flex, T.justifyCenter ] ]
                                    [ HH.h3
                                        [ HP.classes [ T.textLg, T.fontMedium, T.textGray400, T.trackingTight ] ]
                                        [ HH.text "Find all you read" ]
                                    , HH.span
                                        [ HP.classes [ T.ml2, T.textXs, T.fontSemibold, T.textKiwi ] ]
                                        [ HH.text "SOON" ]
                                    ]
                                , HH.p
                                    [ HP.classes [ T.mt5, T.textBase, T.textGray500 ] ]
                                    [ HH.text "Ac tincidunt sapien vehicula erat auctor pellentesque rhoncus. Et magna sit morbi lobortis." ]
                                ]
                            ]
                        ]
                    , HH.div
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
                                        [ Icons.beaker
                                            [ Icons.classes [ T.h6, T.w6, T.textWhite ] ]
                                        ]
                                    ]
                                , HH.h3
                                    [ HP.classes [ T.mt8, T.textLg, T.fontMedium, T.textGray400, T.trackingTight ] ]
                                    [ HH.text "More ..." ]
                                , HH.p
                                    [ HP.classes [ T.mt5, T.textBase, T.textGray500 ] ]
                                    [ HH.text "Ac tincidunt sapien vehicula erat auctor pellentesque rhoncus. Et magna sit morbi lobortis." ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]

    rest :: forall i p. HH.HTML i p
    rest =
      HH.main
        []
        [ HH.div
            [ HP.classes [ T.pt8, T.overflowHidden, T.smPt12, T.lgRelative, T.lgPy48 ] ]
            [ HH.div
                [ HP.classes
                    [ T.mxAuto
                    , T.maxWMd
                    , T.px4
                    , T.smMaxW3xl
                    , T.smPx6
                    , T.lgPx8
                    , T.lgMaxW7xl
                    , T.lgGrid
                    , T.lgGridCols2
                    , T.lgGap24
                    ]
                ]
                [ HH.div
                    []
                    [ HH.div
                        []
                        [ HH.img
                            [ HP.alt "Listas"
                            , HP.src "https://tailwindui.com/img/logos/workflow-mark.svg?color=rose&shade=500"
                            , HP.classes [ T.h11, T.wAuto ]
                            ]
                        ]
                    , HH.div
                        [ HP.classes [ T.mt20 ] ]
                        [ HH.div
                            []
                            [ HH.a
                                [ HP.classes [ T.inlineFlex, T.spaceX4 ], HP.href "#" ]
                                [ HH.span
                                    [ HP.classes
                                        [ T.rounded
                                        , T.bgDuraznoLight
                                        , T.px2p5
                                        , T.py1
                                        , T.textXs
                                        , T.fontSemibold
                                        , T.textDurazno
                                        , T.trackingWide
                                        , T.uppercase
                                        ]
                                    ]
                                    [ HH.text "What's new" ]
                                , HH.span
                                    [ HP.classes
                                        [ T.inlineFlex
                                        , T.itemsCenter
                                        , T.textSm
                                        , T.fontMedium
                                        , T.textDurazno
                                        , T.spaceX1
                                        ]
                                    ]
                                    [ HH.span
                                        []
                                        [ HH.text "Just shipped version 0.1.0" ]
                                    , Icons.chevronRight [ Icons.classes [ T.h5, T.w5 ] ]
                                    ]
                                ]
                            ]
                        , HH.div
                            [ HP.classes [ T.mt6, T.smMaxWXl ] ]
                            [ HH.h1
                                [ HP.classes [ T.text4xl, T.fontExtrabold, T.textGray900, T.trackingTight, T.smText5xl ] ]
                                [ HH.text "Digital content consumption manager" ]
                            , HH.p
                                [ HP.classes [ T.mt6, T.textXl, T.textGray500 ] ]
                                [ HH.text "Keep your reading and watching lists under control with Listas" ]
                            ]
                        , HH.form
                            [ HP.classes [ T.mt12, T.smMaxWLg, T.smWFull, T.smFlex ] ]
                            [ HH.div
                                [ HP.classes [ T.minW0, T.flex1 ] ]
                                [ HH.label
                                    [ HP.classes [ T.srOnly ], HP.for "hero_email" ]
                                    [ HH.text "Email address" ]
                                , HH.input
                                    [ HP.placeholder "Enter your email"
                                    , HP.classes
                                        [ T.block
                                        , T.wFull
                                        , T.border
                                        , T.borderGray300
                                        , T.roundedMd
                                        , T.px5
                                        , T.py3
                                        , T.textBase
                                        , T.textGray900
                                        , T.placeholderGray500
                                        , T.shadowSm
                                        , T.focusBorderDurazno
                                        , T.focusRingDurazno
                                        ]
                                    , HP.type_ HP.InputEmail
                                    , HP.id_ "hero_email"
                                    ]
                                ]
                            , HH.div
                                [ HP.classes [ T.mt4, T.smMt0, T.smMl3 ] ]
                                [ HH.button
                                    [ HP.classes
                                        [ T.block
                                        , T.wFull
                                        , T.roundedMd
                                        , T.border
                                        , T.borderTransparent
                                        , T.px5
                                        , T.py3
                                        , T.bgDuraznoLight
                                        , T.textBase
                                        , T.fontMedium
                                        , T.textWhite
                                        , T.shadow
                                        , T.hoverBgDurazno
                                        , T.focusOutlineNone
                                        , T.focusRing2
                                        , T.focusRingDurazno
                                        , T.focusRingOffset2
                                        , T.smPx10
                                        ]
                                    , HP.type_ HP.ButtonSubmit
                                    ]
                                    [ HH.text "Notify me" ]
                                ]
                            ]
                        , HH.div
                            [ HP.classes [ T.mt6 ] ]
                            [ HH.div
                                [ HP.classes [ T.inlineFlex, T.itemsCenter, T.divideX, T.divideGray300 ] ]
                                [ HH.div
                                    [ HP.classes [ T.flexShrink0, T.flex, T.pr5 ] ]
                                    [ Icons.star [ Icons.classes [ T.h5, T.w5, T.textYellow400 ] ]
                                    , Icons.star [ Icons.classes [ T.h5, T.w5, T.textYellow400 ] ]
                                    , Icons.star [ Icons.classes [ T.h5, T.w5, T.textYellow400 ] ]
                                    , Icons.star [ Icons.classes [ T.h5, T.w5, T.textYellow400 ] ]
                                    , Icons.star [ Icons.classes [ T.h5, T.w5, T.textYellow400 ] ]
                                    ]
                                , HH.div
                                    [ HP.classes
                                        [ T.minW0
                                        , T.flex1
                                        , T.pl5
                                        , T.py1
                                        , T.textSm
                                        , T.textGray500
                                        , T.smPy3
                                        ]
                                    ]
                                    [ HH.span
                                        [ HP.classes [ T.fontMedium, T.textGray900 ] ]
                                        [ HH.text "Rated 5 stars" ]
                                    , HH.text " by over "
                                    , HH.span
                                        [ HP.classes [ T.fontMedium, T.textDurazno ] ]
                                        [ HH.text "500 beta users" ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            , HH.div
                [ HP.classes [ T.smMxAuto, T.smMaxW3xl, T.smPx6 ] ]
                [ HH.div
                    [ HP.classes
                        [ T.py12
                        , T.smRelative
                        , T.smMt12
                        , T.smPy16
                        , T.lgAbsolute
                        , T.lgInsetY0
                        , T.lgRight0
                        , T.lgW1d2
                        ]
                    ]
                    [ HH.div
                        [ HP.classes [ T.hidden, T.smBlock ] ]
                        [ HH.div
                            [ HP.classes
                                [ T.absolute
                                , T.insetY0
                                , T.left1d2
                                , T.wScreen
                                , T.bgGray10
                                , T.roundedL3xl
                                , T.lgLeft80
                                , T.lgRight0
                                , T.lgWFull
                                ]
                            ]
                            []
                        ]
                    , HH.div
                        [ HP.classes
                            [ T.relative
                            , T.pl4
                            , T.negMr40
                            , T.smMxAuto
                            , T.smMaxW3xl
                            , T.smPx0
                            , T.lgMaxWNone
                            , T.lgHFull
                            , T.lgPl12
                            ]
                        ]
                        [ HH.img
                            [ HP.alt ""
                            , HP.src "https://i.imgur.com/2UGt2Ko.png"
                            , HP.classes
                                [ T.wFull
                                , T.roundedMd
                                , T.shadowXl
                                , T.ring1
                                , T.ringBlack
                                , T.ringOpacity5
                                , T.lgHFull
                                , T.lgWAuto
                                , T.lgMaxWNone
                                ]
                            ]
                        ]
                    ]
                ]
            ]
          , HH.div
            [ HP.classes [ T.relative, T.mt20 ] ]
            [ HH.div
                [ HP.classes
                    [ T.lgMxAuto
                    , T.lgMaxW7xl
                    , T.lgPx8
                    , T.lgGrid
                    , T.lgGridCols2
                    , T.lgGap24
                    , T.lgItemsStart
                    ]
                ]
                [ HH.div
                    [ HP.classes [ T.relative, T.smPy16, T.lgPy0 ] ]
                    [ HH.div
                        [ HP.classes
                            [ T.hidden
                            , T.smBlock
                            , T.lgAbsolute
                            , T.lgInsetY0
                            , T.lgRight0
                            , T.lgWScreen
                            ]
                        ]
                        [ HH.div
                            [ HP.classes
                                [ T.absolute
                                , T.insetY0
                                , T.right1d2
                                , T.wFull
                                , T.bgGray10
                                , T.roundedR3xl
                                , T.lgRight72
                                ]
                            ]
                            []
                        ]
                    , HH.div
                        [ HP.classes
                            [ T.relative
                            , T.mxAuto
                            , T.maxWMd
                            , T.px4
                            , T.smMaxW3xl
                            , T.smPx6
                            , T.lgPx0
                            , T.lgMaxWNone
                            , T.lgPy20
                            ]
                        ]
                        [ HH.div
                            [ HP.classes
                                [ T.relative
                                , T.pt64
                                , T.pb10
                                , T.rounded2xl
                                , T.shadowXl
                                , T.overflowHidden
                                ]
                            ]
                            [ HH.img
                                [ HP.alt ""
                                , HP.src "https://images.unsplash.com/photo-1521510895919-46920266ddb3?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&fp-x=0.5&fp-y=0.6&fp-z=3&width=1440&height=1440&sat=-100"
                                , HP.classes [ T.absolute, T.inset0, T.hFull, T.wFull, T.objectCover ]
                                ]
                            , HH.div
                                [ HP.classes
                                    [ T.absolute
                                    , T.inset0
                                    , T.bgGradientToT
                                    , T.fromDuraznoLight
                                    , T.viaDurazno
                                    , T.opacity90
                                    ]
                                ]
                                []
                            , HH.div
                                [ HP.classes [ T.relative, T.px8 ] ]
                                [ HH.div
                                    []
                                    [ HH.img
                                        [ HP.alt "Workcation"
                                        , HP.src "https://tailwindui.com/img/logos/workcation.svg?color=white"
                                        , HP.classes [ T.h12 ]
                                        ]
                                    ]
                                , HH.blockquote
                                    [ HP.classes [ T.mt8 ] ]
                                    [ HH.div
                                        [ HP.classes [ T.textLg, T.fontMedium, T.textWhite, T.mdFlexGrow ] ]
                                        [ HH.p
                                            []
                                            [ HH.text "Tincidunt integer commodo, cursus etiam aliquam neque, et. Consectetur pretium in volutpat, diam. Montes, magna cursus nulla feugiat dignissim id lobortis amet." ]
                                        ]
                                    , HH.footer
                                        [ HP.classes [ T.mt4 ] ]
                                        [ HH.p
                                            [ HP.classes [ T.textBase, T.fontSemibold, T.textManzana ] ]
                                            [ HH.text "Sarah Williams, CEO at Workcation" ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , HH.div
                    [ HP.classes
                        [ T.relative
                        , T.mxAuto
                        , T.maxWMd
                        , T.px4
                        , T.smMaxW3xl
                        , T.smPx6
                        , T.lgPx0
                        ]
                    ]
                    [ HH.div
                        [ HP.classes [ T.pt12, T.smPt16, T.lgPt20 ] ]
                        [ HH.h2
                            [ HP.classes [ T.text3xl, T.textGray900, T.fontExtrabold, T.trackingTight, T.smText4xl ] ]
                            [ HH.text "On a mission to empower teams" ]
                        , HH.div
                            [ HP.classes [ T.mt6, T.textGray500, T.spaceY6 ] ]
                            [ HH.p
                                [ HP.classes [ T.textLg ] ]
                                [ HH.text "Sagittis scelerisque nulla cursus in enim consectetur quam. Dictum urna sed consectetur neque tristique pellentesque. Blandit amet, sed aenean erat arcu morbi. Cursus faucibus nunc nisl netus morbi vel porttitor vitae ut. Amet vitae fames senectus vitae." ]
                            , HH.p
                                [ HP.classes [ T.textBase, T.leading7 ] ]
                                [ HH.text "Sollicitudin tristique eros erat odio sed vitae, consequat turpis elementum. Lorem nibh vel, eget pretium arcu vitae. Eros eu viverra donec ut volutpat donec laoreet quam urna. Sollicitudin tristique eros erat odio sed vitae, consequat turpis elementum. Lorem nibh vel, eget pretium arcu vitae. Eros eu viverra donec ut volutpat donec laoreet quam urna." ]
                            , HH.p
                                [ HP.classes [ T.textBase, T.leading7 ] ]
                                [ HH.text "Rhoncus nisl, libero egestas diam fermentum dui. At quis tincidunt vel ultricies. Vulputate aliquet velit faucibus semper. Pellentesque in venenatis vestibulum consectetur nibh id. In id ut tempus egestas. Enim sit aliquam nec, a. Morbi enim fermentum lacus in. Viverra." ]
                            ]
                        ]
                    , HH.div
                        [ HP.classes [ T.mt10 ] ]
                        [ HH.dl
                            [ HP.classes [ T.grid, T.gridCols2, T.gapX4, T.gapY8 ] ]
                            [ HH.div
                                [ HP.classes [ T.borderT2, T.borderGray100, T.pt6 ] ]
                                [ HH.dt
                                    [ HP.classes [ T.textBase, T.fontMedium, T.textGray500 ] ]
                                    [ HH.text "Founded" ]
                                , HH.dd
                                    [ HP.classes [ T.text3xl, T.fontExtrabold, T.trackingTight, T.textGray900 ] ]
                                    [ HH.text "2021" ]
                                ]
                            , HH.div
                                [ HP.classes [ T.borderT2, T.borderGray100, T.pt6 ] ]
                                [ HH.dt
                                    [ HP.classes [ T.textBase, T.fontMedium, T.textGray500 ] ]
                                    [ HH.text "Employees" ]
                                , HH.dd
                                    [ HP.classes [ T.text3xl, T.fontExtrabold, T.trackingTight, T.textGray900 ] ]
                                    [ HH.text "5" ]
                                ]
                            , HH.div
                                [ HP.classes [ T.borderT2, T.borderGray100, T.pt6 ] ]
                                [ HH.dt
                                    [ HP.classes [ T.textBase, T.fontMedium, T.textGray500 ] ]
                                    [ HH.text "Beta Users" ]
                                , HH.dd
                                    [ HP.classes [ T.text3xl, T.fontExtrabold, T.trackingTight, T.textGray900 ] ]
                                    [ HH.text "521" ]
                                ]
                            , HH.div
                                [ HP.classes [ T.borderT2, T.borderGray100, T.pt6 ] ]
                                [ HH.dt
                                    [ HP.classes [ T.textBase, T.fontMedium, T.textGray500 ] ]
                                    [ HH.text "Raised" ]
                                , HH.dd
                                    [ HP.classes [ T.text3xl, T.fontExtrabold, T.trackingTight, T.textGray900 ] ]
                                    [ HH.text "$25M" ]
                                ]
                            ]
                        , HH.div
                            [ HP.classes [ T.mt10 ] ]
                            [ HH.a
                                [ HP.classes [ T.textBase, T.fontMedium, T.textDurazno ], HP.href "#" ]
                                [ HH.text "Learn more about how we're changing the world" ]
                            ]
                        ]
                    ]
                ]
            ]
          , HH.div
            [ HP.classes [ T.mt32 ] ]
            [ HH.div
                [ HP.classes
                    [ T.mxAuto
                    , T.maxWMd
                    , T.px4
                    , T.smMaxW3xl
                    , T.smPx6
                    , T.lgPx8
                    , T.lgMaxW7xl
                    ]
                ]
                [ HH.div
                    [ HP.classes [ T.lgGrid, T.lgGridCols2, T.lgGap24, T.lgItemsCenter ] ]
                    [ HH.div
                        []
                        [ HH.h2
                            [ HP.classes [ T.text3xl, T.fontExtrabold, T.textGray900, T.trackingTight, T.smText4xl ] ]
                            [ HH.text "Backed by world-renowned investors" ]
                        , HH.p
                            [ HP.classes [ T.mt6, T.maxW3xl, T.textLg, T.leading7, T.textGray500 ] ]
                            [ HH.text "Sagittis scelerisque nulla cursus in enim consectetur quam. Dictum urna sed consectetur neque tristique pellentesque. Blandit amet, sed aenean erat arcu morbi. Cursus faucibus nunc nisl netus morbi vel porttitor vitae ut. Amet vitae fames senectus vitae." ]
                        , HH.div
                            [ HP.classes [ T.mt6 ] ]
                            [ HH.a
                                [ HP.classes [ T.textBase, T.fontMedium, T.textDurazno ], HP.href "#" ]
                                [ HH.text "Meet our investors and advisors" ]
                            ]
                        ]
                    , HH.div
                        [ HP.classes
                            [ T.mt12
                            , T.grid
                            , T.gridCols2
                            , T.gap0p5
                            , T.mdGridCols3
                            , T.lgMt0
                            , T.lgGridCols2
                            ]
                        ]
                        [ HH.div
                            [ HP.classes
                                [ T.colSpan1
                                , T.flex
                                , T.justifyCenter
                                , T.py8
                                , T.px8
                                , T.bgGray10
                                ]
                            ]
                            [ HH.img
                                [ HP.alt "Workcation"
                                , HP.src "https://tailwindui.com/img/logos/transistor-logo-gray-400.svg"
                                , HP.classes [ T.maxH12 ]
                                ]
                            ]
                        , HH.div
                            [ HP.classes
                                [ T.colSpan1
                                , T.flex
                                , T.justifyCenter
                                , T.py8
                                , T.px8
                                , T.bgGray10
                                ]
                            ]
                            [ HH.img
                                [ HP.alt "Mirage"
                                , HP.src "https://tailwindui.com/img/logos/mirage-logo-gray-400.svg"
                                , HP.classes [ T.maxH12 ]
                                ]
                            ]
                        , HH.div
                            [ HP.classes
                                [ T.colSpan1
                                , T.flex
                                , T.justifyCenter
                                , T.py8
                                , T.px8
                                , T.bgGray10
                                ]
                            ]
                            [ HH.img
                                [ HP.alt "Tuple"
                                , HP.src "https://tailwindui.com/img/logos/tuple-logo-gray-400.svg"
                                , HP.classes [ T.maxH12 ]
                                ]
                            ]
                        , HH.div
                            [ HP.classes
                                [ T.colSpan1
                                , T.flex
                                , T.justifyCenter
                                , T.py8
                                , T.px8
                                , T.bgGray10
                                ]
                            ]
                            [ HH.img
                                [ HP.alt "Laravel"
                                , HP.src "https://tailwindui.com/img/logos/laravel-logo-gray-400.svg"
                                , HP.classes [ T.maxH12 ]
                                ]
                            ]
                        , HH.div
                            [ HP.classes
                                [ T.colSpan1
                                , T.flex
                                , T.justifyCenter
                                , T.py8
                                , T.px8
                                , T.bgGray10
                                ]
                            ]
                            [ HH.img
                                [ HP.alt "StaticKit"
                                , HP.src "https://tailwindui.com/img/logos/statickit-logo-gray-400.svg"
                                , HP.classes [ T.maxH12 ]
                                ]
                            ]
                        , HH.div
                            [ HP.classes
                                [ T.colSpan1
                                , T.flex
                                , T.justifyCenter
                                , T.py8
                                , T.px8
                                , T.bgGray10
                                ]
                            ]
                            [ HH.img
                                [ HP.alt "Statamic"
                                , HP.src "https://tailwindui.com/img/logos/workcation-logo-gray-400.svg"
                                , HP.classes [ T.maxH12 ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
          , HH.div
            [ HP.classes [ T.relative, T.mt24, T.smMt32, T.smPy16 ] ]
            [ HH.div
                [ HP.classes [ T.hidden, T.smBlock ] ]
                [ HH.div
                    [ HP.classes
                        [ T.absolute
                        , T.insetY0
                        , T.left0
                        , T.w1d2
                        , T.bgGray10
                        , T.roundedR3xl
                        ]
                    ]
                    []
                ]
            , HH.div
                [ HP.classes
                    [ T.mxAuto
                    , T.maxWMd
                    , T.px4
                    , T.smMaxW3xl
                    , T.smPx6
                    , T.lgMaxW7xl
                    , T.lgPx8
                    ]
                ]
                [ HH.div
                    [ HP.classes
                        [ T.relative
                        , T.rounded2xl
                        , T.px6
                        , T.py10
                        , T.bgDuraznoLight
                        , T.overflowHidden
                        , T.shadowXl
                        , T.smPx12
                        , T.smPy20
                        ]
                    ]
                    [ HH.div
                        [ HP.classes [ T.relative ] ]
                        [ HH.div
                            [ HP.classes [ T.smTextCenter ] ]
                            [ HH.h2
                                [ HP.classes [ T.text3xl, T.fontExtrabold, T.textWhite, T.trackingTight, T.smText4xl ] ]
                                [ HH.text "Get notified when we are launching." ]
                            , HH.p
                                [ HP.classes [ T.mt6, T.mxAuto, T.maxW2xl, T.textLg, T.textManzana ] ]
                                [ HH.text "Sagittis scelerisque nulla cursus in enim consectetur quam. Dictum urna sed consectetur neque tristique pellentesque." ]
                            ]
                        , HH.form
                            [ HP.classes [ T.mt12, T.smMxAuto, T.smMaxWLg, T.smFlex ] ]
                            [ HH.div
                                [ HP.classes [ T.minW0, T.flex1 ] ]
                                [ HH.label
                                    [ HP.classes [ T.srOnly ], HP.for "cta_email" ]
                                    [ HH.text "Email address" ]
                                , HH.input
                                    [ HP.placeholder "Enter your email"
                                    , HP.classes
                                        [ T.block
                                        , T.wFull
                                        , T.border
                                        , T.borderTransparent
                                        , T.roundedMd
                                        , T.px5
                                        , T.py3
                                        , T.textBase
                                        , T.textGray900
                                        , T.placeholderGray500
                                        , T.shadowSm
                                        , T.focusOutlineNone
                                        , T.focusBorderTransparent
                                        , T.focusRing2
                                        , T.focusRingWhite
                                        , T.focusRingOffset2
                                        , T.focusRingOffsetDurazno
                                        ]
                                    , HP.type_ HP.InputEmail
                                    , HP.id_ "cta_email"
                                    ]
                                ]
                            , HH.div
                                [ HP.classes [ T.mt4, T.smMt0, T.smMl3 ] ]
                                [ HH.button
                                    [ HP.classes
                                        [ T.block
                                        , T.wFull
                                        , T.roundedMd
                                        , T.border
                                        , T.borderTransparent
                                        , T.px5
                                        , T.py3
                                        , T.bgGray900
                                        , T.textBase
                                        , T.fontMedium
                                        , T.textWhite
                                        , T.shadow
                                        , T.hoverBgBlack
                                        , T.focusOutlineNone
                                        , T.focusRing2
                                        , T.focusRingWhite
                                        , T.focusRingOffset2
                                        , T.focusRingOffsetDurazno
                                        , T.smPx10
                                        ]
                                    , HP.type_ HP.ButtonSubmit
                                    ]
                                    [ HH.text "Notify me" ]
                                ]
                            ]
                        ]
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
                            , HH.span
                                [ HP.classes [ T.block ] ]
                                [ HH.text "Start your free trial today." ]
                            ]
                        , HH.p
                            [ HP.classes [ T.mt4, T.textLg, T.leading6, T.textGray400 ] ]
                            [ HH.text "Ac euismod vel sit maecenas id pellentesque eu sed consectetur. Malesuada adipiscing sagittis vel nulla nec." ]
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
                , T.bgIndigo600
                , T.smH72
                , T.mdAbsolute
                , T.mdLeft0
                , T.mdHFull
                , T.mdW1d2
                ]
            ]
            [ HH.img
                [ -- TODO: https://unsplash.com/photos/gm2qQPnSJBA
                  HP.src "https://images.unsplash.com/photo-1545239352-8cf6abca7cfd?ixlib=rb-1.2.1&ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&auto=format&fit=crop&w=675&q=80"
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
                    [ HH.text "Find out what other people are reading" ]
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
                            , HE.onClick $ Just <<< Navigate Discover <<< Mouse.toEvent
                            ]
                            [ HH.text "Discover other lists" ]
                        ]
                    ]
                ]
            ]
        ]

footer :: forall i p. HH.HTML i p
footer =
  HH.footer
    [ HP.classes [ T.mt24, T.bgKiwiDark, T.smMt12 ] ]
    [
      HH.div
        [ HP.classes
            [ T.mxAuto
            , T.maxWMd
            , T.py12
            , T.px4
            , T.overflowHidden
            , T.smMaxW3xl
            , T.smPx6
            , T.lgMaxW7xl
            , T.lgPx8
            ]
        ]
        [ HH.nav
            [ HP.classes [ T.negMx5, T.negMy2, T.flex, T.flexWrap, T.justifyCenter ] ]
            [ HH.div
                [ HP.classes [ T.px5, T.py2 ] ]
                [ HH.a
                    [ HP.classes [ T.textBase, T.textWhite, T.hoverTextGray10 ], HP.href "#" ]
                    [ HH.text "About" ]
                ]
            , HH.div
                [ HP.classes [ T.px5, T.py2 ] ]
                [ HH.a
                    [ HP.classes [ T.textBase, T.textWhite, T.hoverTextGray10 ], HP.href "#" ]
                    [ HH.text "Blog" ]
                ]
            , HH.div
                [ HP.classes [ T.px5, T.py2 ] ]
                [ HH.a
                    [ HP.classes [ T.textBase, T.textWhite, T.hoverTextGray10 ], HP.href "#" ]
                    [ HH.text "Jobs" ]
                ]
            , HH.div
                [ HP.classes [ T.px5, T.py2 ] ]
                [ HH.a
                    [ HP.classes [ T.textBase, T.textWhite, T.hoverTextGray10 ], HP.href "#" ]
                    [ HH.text "Press" ]
                ]
            , HH.div
                [ HP.classes [ T.px5, T.py2 ] ]
                [ HH.a
                    [ HP.classes [ T.textBase, T.textWhite, T.hoverTextGray10 ], HP.href "#" ]
                    [ HH.text "Accessibility" ]
                ]
            , HH.div
                [ HP.classes [ T.px5, T.py2 ] ]
                [ HH.a
                    [ HP.classes [ T.textBase, T.textWhite, T.hoverTextGray10 ], HP.href "#" ]
                    [ HH.text "Partners" ]
                ]
            ]
        , HH.div
            [ HP.classes [ T.mt8, T.flex, T.justifyCenter, T.spaceX6 ] ]
            [ HH.a
                [ HP.classes [ T.textWhite, T.hoverTextGray10 ], HP.href "#" ]
                [ HH.span
                    [ HP.classes [ T.srOnly ] ]
                    [ HH.text "Twitter" ]
                , Icons.twitter [ Icons.classes [ T.h6, T.w6 ] ]
                ]
            , HH.a
                [ HP.classes [ T.textWhite, T.hoverTextGray10 ], HP.href "#" ]
                [ HH.span
                    [ HP.classes [ T.srOnly ] ]
                    [ HH.text "GitHub" ]
                , Icons.github [ Icons.classes [ T.h6, T.w6 ] ]
                ]
            ]
        , HH.p
            [ HP.classes [ T.mt8, T.textCenter, T.textBase, T.textWhite ] ]
            [ HH.text "© 2020 Listas, Inc. All rights reserved." ]
        ]
    ]

