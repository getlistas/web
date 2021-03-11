module Listasio.Page.Home where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Layout as Layout
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Route (Route(..))
import Listasio.Env (UserEnv)
import Tailwind as T
import Web.Event.Event (Event)

data Action
  = Initialize
  | Receive { currentUser :: Maybe ProfileWithIdAndEmail }
  | Navigate Route Event

type State = {currentUser :: Maybe ProfileWithIdAndEmail}

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
  initialState { currentUser } = { currentUser }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> pure unit

    Receive { currentUser } ->
      H.modify_ _ { currentUser = currentUser }

    Navigate route e -> navigate_ e route

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { currentUser } =
    if true
      then home
      else
        Layout.dashboard
          currentUser
          Navigate
          (Just Home)
          $ HH.div
              []
              [ HH.h1
                  [ HP.classes [ T.textGray400, T.mb6, T.text4xl, T.fontBold ] ]
                  [ HH.text "Digital content consumption manager" ]
              , HH.p
                  [ HP.classes [ T.textGray400, T.textLg ] ]
                  [ HH.text "Keep your reading and watching lists under control with Listas" ]
              ]

home :: forall i p. HH.HTML i p
home =
  HH.div
    [ HP.classes [ T.bgWhite ] ]
    [ HH.main
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
                                    , Icons.chevronRight [ HP.name "chevron-right", Icons.classes [ T.h5, T.w5 ] ]
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
                                , T.bgGray50
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
                                , T.bgGray50
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
                                , T.bgGray50
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
                                , T.bgGray50
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
                                , T.bgGray50
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
                                , T.bgGray50
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
                                , T.bgGray50
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
                                , T.bgGray50
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
                        , T.bgGray50
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
    , HH.footer
        [ HP.classes [ T.mt24, T.bgGray900, T.smMt12 ] ]
        [ HH.div
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
                        [ HP.classes [ T.textBase, T.textGray400, T.hoverTextGray300 ], HP.href "#" ]
                        [ HH.text "About" ]
                    ]
                , HH.div
                    [ HP.classes [ T.px5, T.py2 ] ]
                    [ HH.a
                        [ HP.classes [ T.textBase, T.textGray400, T.hoverTextGray300 ], HP.href "#" ]
                        [ HH.text "Blog" ]
                    ]
                , HH.div
                    [ HP.classes [ T.px5, T.py2 ] ]
                    [ HH.a
                        [ HP.classes [ T.textBase, T.textGray400, T.hoverTextGray300 ], HP.href "#" ]
                        [ HH.text "Jobs" ]
                    ]
                , HH.div
                    [ HP.classes [ T.px5, T.py2 ] ]
                    [ HH.a
                        [ HP.classes [ T.textBase, T.textGray400, T.hoverTextGray300 ], HP.href "#" ]
                        [ HH.text "Press" ]
                    ]
                , HH.div
                    [ HP.classes [ T.px5, T.py2 ] ]
                    [ HH.a
                        [ HP.classes [ T.textBase, T.textGray400, T.hoverTextGray300 ], HP.href "#" ]
                        [ HH.text "Accessibility" ]
                    ]
                , HH.div
                    [ HP.classes [ T.px5, T.py2 ] ]
                    [ HH.a
                        [ HP.classes [ T.textBase, T.textGray400, T.hoverTextGray300 ], HP.href "#" ]
                        [ HH.text "Partners" ]
                    ]
                ]
            , HH.div
                [ HP.classes [ T.mt8, T.flex, T.justifyCenter, T.spaceX6 ] ]
                [ HH.a
                    [ HP.classes [ T.textGray400, T.hoverTextGray300 ], HP.href "#" ]
                    [ HH.span
                        [ HP.classes [ T.srOnly ] ]
                        [ HH.text "Twitter" ]
                    , Icons.twitter [ Icons.classes [ T.h6, T.w6 ] ]
                    ]
                , HH.a
                    [ HP.classes [ T.textGray400, T.hoverTextGray300 ], HP.href "#" ]
                    [ HH.span
                        [ HP.classes [ T.srOnly ] ]
                        [ HH.text "GitHub" ]
                    , Icons.github [ Icons.classes [ T.h6, T.w6 ] ]
                    ]
                ]
            , HH.p
                [ HP.classes [ T.mt8, T.textCenter, T.textBase, T.textGray400 ] ]
                [ HH.text "© 2020 Listas, Inc. All rights reserved." ]
            ]
        ]
    ]
