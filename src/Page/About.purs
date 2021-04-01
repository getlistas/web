module Listasio.Page.About where

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
import Listasio.Data.Avatar as Avatar
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Route (Route)
import Listasio.Env (UserEnv)
import Tailwind as T
import Web.Event.Event (Event)

data Action
  = Receive { currentUser :: Maybe ProfileWithIdAndEmail }
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
      }
  }
  where
  initialState { currentUser } = { currentUser }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Receive { currentUser } ->
      H.modify_ _ { currentUser = currentUser }

    Navigate route e -> navigate_ e route

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { currentUser } =
    HH.div
      []
      [ HH.div
          [ HP.classes [ T.pt2 ] ]
          [ HH.h1
              [ HP.classes [ T.textGray400, T.mb6, T.text4xl, T.fontBold ] ]
              [ HH.text "About" ]
          ]
      , team
          [ { avatar: "https://avatars.githubusercontent.com/u/6719053?s=460&u=67beac88e53b8a83a0327eb03d0554547f4373e6&v=4"
            , name: "Nicolas"
            , role: "Backend"
            , icon: Icons.terminal
            , links:
                [ { icon: Icons.twitter, url: "https://twitter.com/delvallenicolas" }
                , { icon: Icons.github, url: "https://github.com/ndelvalle" }
                ]
            }
          , { avatar: "https://avatars.githubusercontent.com/u/8309423?s=460&u=0f306a70fdcc2359d21b4918efaabf617a396c91&v=4"
            , name: "Christian"
            , role: "Frontend"
            , icon: Icons.code
            , links:
                [ { icon: Icons.twitter, url: "https://twitter.com/gillchristian" }
                , { icon: Icons.github, url: "https://github.com/gillchristian" }
                , { icon: Icons.website, url: "https://gillchristian.xyz" }
                ]
            }
          , { avatar: "https://pbs.twimg.com/profile_images/1194743088373219330/10-3KEsy_400x400.jpg"
            , name: "Hahuel"
            , role: "Design"
            , icon: Icons.photo
            , links:
                [ { icon: Icons.twitter, url: "https://twitter.com/DvNahuel" }
                , { icon: Icons.website, url: "https://dvnahuel.website/" }
                ]
            }
          ]
      ]

    where
    team members =
      HH.ul
        [ HP.classes
            [ T.spaceY4
            , T.smGrid
            , T.smGridCols2
            , T.smGap6
            , T.smSpaceY0
            , T.lgGridCols3
            , T.lgGap8
            ]
        ]
        $ map memberEl members

    memberEl {avatar, name, role,icon,  links} =
      HH.li
        [ HP.classes
            [ T.py10
            , T.px6
            , T.bgKiwiLight
            , T.textCenter
            , T.roundedLg
            , T.xlPx10
            , T.xlTextLeft
            ]
        ]
        [ HH.div
            [ HP.classes [ T.spaceY6, T.xlSpaceY10 ] ]
            [ HH.div
                [ HP.classes [ T.mxAuto, T.h40, T.w40, T.roundedFull, T.xlW56, T.xlH56 ] ]
                [ Avatar.renderWithDefault Avatar.Full $ Avatar.parse avatar ]
            , HH.div
                [ HP.classes [ T.spaceY2, T.xlFlex, T.xlItemsEnd, T.xlJustifyBetween ] ]
                [ HH.div
                    [ HP.classes [ T.fontMedium, T.textLg, T.leading6, T.spaceY1 ] ]
                    [ HH.h3
                        [ HP.classes [ T.textWhite ] ]
                        [ HH.text name ]
                    , HH.p
                        [ HP.classes [ T.textGray400 ] ]
                        [ icon [ Icons.classes [ T.w5, T.h5, T.mr2, T.inlineBlock ] ]
                        , HH.text role
                        ]
                    ]
                , HH.ul
                    [ HP.classes [ T.flex, T.justifyCenter, T.spaceX5 ] ]
                    $ map socialEl links

                ]
            ]
        ]

    socialEl {icon, url} =
      HH.li
        []
        [ HH.a
            [ HP.classes [ T.textGray400, T.hoverTextGray300 ]
            , HP.href url
            , HP.target "_blank"
            , HP.rel "noopener noreferrer"
            ]
            [ icon
                [ Icons.classes [ T.w5, T.h5 ] ]
            ]
        ]
