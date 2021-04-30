module Listasio.Page.Profile where

import Prelude

import Component.HOC.Connect as Connect
import Control.Error.Util (note)
import Control.Monad.Reader (class MonadAsk)
import Data.Array (reverse)
import Data.Lens (_Just, preview)
import Data.Maybe (Maybe(..), isJust)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Capability.Resource.User (class ManageUser, myMetrics, userBySlug)
import Listasio.Component.HTML.Icons as Icons
import Listasio.Data.Avatar as Avatar
import Listasio.Data.DateTime as DateTime
import Listasio.Data.Lens (_currentUser, _id, _profile)
import Listasio.Data.Metrics (Metric)
import Listasio.Data.Profile (ProfileWithIdAndEmail, PublicProfile)
import Listasio.Data.Route (Route)
import Listasio.Data.Username as Username
import Listasio.Env (UserEnv)
import Network.RemoteData (RemoteData(..), _Success)
import Network.RemoteData as RemoteData
import Slug (Slug)
import Tailwind as T
import Web.Event.Event (Event)

data Action
  = Initialize
  | Receive {slug :: Slug, currentUser :: Maybe ProfileWithIdAndEmail}
  | LoadMetrics
  | Navigate Route Event

type State
  = { slug :: Slug
    , currentUser :: Maybe ProfileWithIdAndEmail
    , profile :: RemoteData String PublicProfile
    , metrics :: RemoteData String (Array Metric)
    }

component
  :: forall q o m r
   . MonadAff m
  => MonadAsk { userEnv :: UserEnv | r } m
  => ManageUser m
  => Navigate m
  => H.Component HH.HTML q {slug :: Slug} o m
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
  initialState { slug, currentUser } =
    { slug
    , currentUser
    , profile: NotAsked
    , metrics: NotAsked
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> do
      {slug} <- H.get
      H.modify_ _ {profile = Loading}
      profile <- RemoteData.fromEither <$> note "Could not fetch user profile" <$> userBySlug slug
      H.modify_ _ {profile = profile}
      {currentUser} <- H.get
      when (RemoteData.isSuccess profile && isJust currentUser) do
        void $ H.fork $ handleAction LoadMetrics

    Receive {currentUser} -> do
      H.modify_ _ {currentUser = currentUser}
      {profile, metrics} <- H.get
      when (RemoteData.isSuccess profile && isJust currentUser && RemoteData.isNotAsked metrics) do
        void $ H.fork $ handleAction LoadMetrics

    LoadMetrics -> do
      mbUser <- H.gets $ preview (_currentUser <<< _Just <<< _id)
      mbProfile <- H.gets $ preview (_profile <<< _Success <<< _id)

      when (mbUser == mbProfile) do
        H.modify_ _ {metrics = Loading}
        metrics <- RemoteData.fromEither <$> note "Could not fetch user metrics" <$> myMetrics
        H.modify_ _ {metrics = reverse <$> metrics}

    Navigate route e -> navigate_ e route

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render {currentUser, profile, metrics} =
    HH.div
      []
      [ wip
      , case profile of
          Success {name, avatar} ->
            profileHeader {name: Username.toString name, avatar}

          Failure _ -> HH.text "User Profile"

          _ -> HH.text "..."

      , case metrics of
          Success ms ->
            HH.div
              [ HP.classes [ T.maxW5xl, T.mxAuto, T.px4, T.smPx6, T.lgPx8, T.mt12 ] ]
              [ HH.div
                  [ HP.classes [ T.textLg, T.textGray400, T.fontSemibold, T.mb2, T.borderB, T.borderGray200, T.w96 ] ]
                  [ HH.text "Activity" ]
              , HH.div
                  [ HP.classes [ T.w96 ] ]
                  $ map metricEl ms
              ]
          _ -> HH.text ""
      ]

    where
    wip =
      HH.div
        [ HP.classes [ T.p2, T.roundedLg, T.bgDurazno, T.smP3, T.mb8 ] ]
        [ HH.div
            [ HP.classes [ T.flex, T.itemsCenter ] ]
            [ HH.span
                [ HP.classes [ T.flex, T.p2, T.roundedLg, T.bgManzana ] ]
                [ Icons.code
                    [ Icons.classes [ T.h6, T.w6, T.textWhite ] ]
                ]
            , HH.p
                [ HP.classes [ T.ml3, T.fontMedium, T.textWhite ] ]
                [ HH.text "Work in progress"
                ]
            ]
        ]

    metricEl {date, completed_count} =
      HH.div
        [ HP.classes [ T.flex, T.justifyBetween, T.mb2 ] ]
        [ HH.p
            [ HP.classes [ T.fontSemibold, T.textGray400 ] ]
            [ HH.text $ DateTime.toDisplayMonthDayYear date ]
        , HH.p
            [ HP.classes [ T.textGray300 ] ]
            [ HH.text $ show completed_count ]
        ]

    profileHeader {name, avatar} =
      HH.div
        []
        [ HH.div
            []
            [ HH.div
                [ HP.classes
                    [ T.h32
                    , T.wFull
                    , T.lgH48
                    , T.roundedMd
                    , T.bgGradientToR
                    , T.fromDurazno
                    , T.viaKiwi
                    , T.toManzana
                    ]
                ]
                []
            ]
        , HH.div
            [ HP.classes [ T.maxW5xl, T.mxAuto, T.px4, T.smPx6, T.lgPx8 ] ]
            [ HH.div
                [ HP.classes [ T.negMt12, T.smNegMt16, T.smFlex, T.smItemsEnd, T.smSpaceX5 ] ]
                [ HH.div
                    [ HP.classes [ T.flex ] ]
                    [ Avatar.profile avatar
                    ]
                , HH.div
                    [ HP.classes
                        [ T.mt6
                        , T.smFlex1
                        , T.smMinW0
                        , T.smFlex
                        , T.smItemsCenter
                        , T.smJustifyEnd
                        , T.smSpaceX6
                        , T.smPb1
                        ]
                    ]
                    [ HH.div
                        [ HP.classes [ T.smHidden, T.mdBlock, T.mt6, T.minW0, T.flex1 ] ]
                        [ HH.h1
                            [ HP.classes [ T.text2xl, T.fontBold, T.textGray400, T.truncate ] ]
                            [ HH.text name ]
                        ]
                    ]
                ]
            , HH.div
                [ HP.classes
                    [ T.hidden
                    , T.smBlock
                    , T.mdHidden
                    , T.mt6
                    , T.minW0
                    , T.flex1
                    ]
                ]
                [ HH.h1
                    [ HP.classes [ T.text2xl, T.fontBold, T.textGray400, T.truncate ] ]
                    [ HH.text name ]
                ]
            ]
        ]

