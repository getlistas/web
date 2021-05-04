module Listasio.Page.Profile where

import Prelude

import Component.HOC.Connect as Connect
import Control.Error.Util (note)
import Control.Monad.Reader (class MonadAsk)
import Data.Array (cons, range)
import Data.Date (Date, adjust)
import Data.DateTime (DateTime(..), date)
import Data.Int (toNumber)
import Data.Lens (_Just, preview)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Time (Time)
import Data.Time.Duration (Days(..), negateDuration)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Capability.Now (class Now, nowDate, nowTime)
import Listasio.Capability.Resource.User (class ManageUser, myMetrics, userBySlug)
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Utils (cx)
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
    , metrics :: RemoteData String (Map Date Int)
    , time :: Maybe Time
    }

metricToTuple :: Metric -> Tuple Date Int
metricToTuple m = Tuple (date m.date) m.completed_count

allDaysPastYear :: Date -> Map Date Int
allDaysPastYear date =
  Map.fromFoldable
    $ maybe [] (cons $ Tuple date 0)
    $ traverse (map Tuple.swap <<< sequence <<< Tuple 0 <<< flip adjust date <<< negateDuration <<< Days <<< toNumber)
    $ range 1 363

component
  :: forall q o m r
   . MonadAff m
  => MonadAsk { userEnv :: UserEnv | r } m
  => ManageUser m
  => Navigate m
  => Now m
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
    , time: Nothing
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> do
      time <- nowTime
      H.modify_ _ {time = Just time}

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

        year <- allDaysPastYear <$> nowDate

        H.modify_ _
          {metrics = flip Map.union year <<< Map.fromFoldable <<< map metricToTuple  <$> metrics}

    Navigate route e -> navigate_ e route

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render {currentUser, profile, metrics, time} =
    HH.div
      []
      [ wip
      , case profile of
          Success {name, avatar} ->
            profileHeader {name: Username.toString name, avatar}

          Failure _ -> HH.text "User Profile"

          _ -> HH.text "..."

      , case metrics, time of
          Success ms, Just t ->
            HH.div
              [ HP.classes [ T.maxW5xl, T.mxAuto, T.px4, T.smPx6, T.lgPx8, T.mt12 ] ]
              [ HH.div
                  [ HP.classes [ T.textLg, T.textGray400, T.fontSemibold, T.mb2, T.borderB, T.borderGray200 ] ]
                  [ HH.text "Activity" ]
              , HH.div
                  [ HP.classes
                      [ T.grid
                      , T.gridRows7
                      , T.gridCols52
                      , T.gridFlowCol
                      , T.gap1
                      ]
                  ]
                  $ map (metricEl t)
                  $ Map.toUnfoldable ms
              ]
          _, _ -> HH.text ""
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

    metricEl :: Time -> Tuple Date Int -> _
    metricEl t (Tuple date count) =
      HH.div
        [ HP.classes
            [ T.relative
            , T.h2
            , T.w2
            , T.group
              -- TODO
              -- 100% max
              -- >75% less dark
              -- >50% mid
              -- >25% light
              -- <25% lighter
            , cx T.bgKiwiDark $ count > 1
            , cx T.bgKiwi $ count == 1
            , cx T.bgGray100 $ count < 1
            ]
        ]
        [ HH.div
            [ HP.classes
                [ T.hidden
                , T.groupHoverBlock
                , T.absolute
                , T.z10
                , T.bottom4
                , T.negInsetX24
                , T.p2
                , T.bgGray300
                , T.textWhite
                , T.textXs
                , T.w56
                , T.textCenter
                , T.roundedMd
                ]
            ]
            [ HH.span [ HP.classes [ T.fontSemibold ] ] [ HH.text $ show count <> " completed" ]
            , HH.span [] [ HH.text $ " on " <> DateTime.toDisplayMonthDayYear (DateTime date t) ]
            ]
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

