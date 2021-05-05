module Listasio.Page.Profile where

import Prelude

import Component.HOC.Connect as Connect
import Control.Error.Util (note)
import Control.Monad.Reader (class MonadAsk)
import Data.Array (cons, range)
import Data.Date (Date, adjust)
import Data.DateTime (DateTime(..), date)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Time (Time)
import Data.Time.Duration (Days(..), negateDuration)
import Data.Traversable (maximum, sequence, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Capability.Now (class Now, nowDate, nowTime)
import Listasio.Capability.Resource.User (class ManageUser, userMetrics, userBySlug)
import Listasio.Component.HTML.Icons as Icons
import Listasio.Data.Avatar as Avatar
import Listasio.Data.DateTime as DateTime
import Listasio.Data.Metrics (Metric)
import Listasio.Data.Profile (ProfileWithIdAndEmail, PublicProfile)
import Listasio.Data.Route (Route)
import Listasio.Data.Username as Username
import Listasio.Env (UserEnv)
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RemoteData
import Slug (Slug)
import Tailwind as T
import Web.Event.Event (Event)

data Action
  = Initialize
  | Receive {slug :: Slug, currentUser :: Maybe ProfileWithIdAndEmail}
  | LoadUser
  | LoadMetrics
  | Navigate Route Event

type State
  = { slug :: Slug
    , currentUser :: Maybe ProfileWithIdAndEmail
    , profile :: RemoteData String PublicProfile
    , metrics :: RemoteData String (Map Date CountScale)
    , time :: Maybe Time
    }

data Scale
  = Highest
  | High
  | Mid
  | Low
  | Lowest

data CountScale
  = None
  | Some Scale Int

scaleColor :: CountScale -> H.ClassName
scaleColor None = T.bgGray100
scaleColor (Some Highest _) = T.bgGreen800
scaleColor (Some High _) = T.bgGreen700
scaleColor (Some Mid _) = T.bgGreen500
scaleColor (Some Low _) = T.bgGreen300
scaleColor (Some Lowest _) = T.bgGreen200

scaleCount :: CountScale -> Int
scaleCount None = 0
scaleCount (Some _ n) = n

toScale :: Int -> Int -> CountScale
toScale _ 0 = None
toScale max n =
  case toNumber n / toNumber max of
    r | r == 1.0 -> Some Highest n
    r | r > 0.75 -> Some High n
    r | r > 0.5 -> Some Mid n
    r | r > 0.25 -> Some Low n
    _ -> Some Lowest n

metricToTuple :: Int -> Metric -> Tuple Date CountScale
metricToTuple max m = Tuple (date m.date) $ toScale max m.completed_count

allDaysPastYear :: Date -> Map Date CountScale
allDaysPastYear date =
  Map.fromFoldable
    $ maybe [] (cons $ Tuple date None)
    $ traverse (map Tuple.swap <<< sequence <<< Tuple None <<< flip adjust date <<< negateDuration <<< Days <<< toNumber)
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
  initialState {slug, currentUser} =
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

      void $ H.fork $ handleAction LoadUser
      void $ H.fork $ handleAction LoadMetrics

    Receive {currentUser} -> do
      H.modify_ _ {currentUser = currentUser}

    LoadUser -> do
      {slug} <- H.get
      H.modify_ _ {profile = Loading}
      profile <- RemoteData.fromEither <$> note "Could not fetch user profile" <$> userBySlug slug
      H.modify_ _ {profile = profile}

    LoadMetrics -> do
      {slug} <- H.get

      H.modify_ _ {metrics = Loading}
      metrics <- RemoteData.fromEither <$> note "Could not fetch user metrics" <$> userMetrics slug
      year <- allDaysPastYear <$> nowDate

      let max = fromMaybe 0 $ join $ RemoteData.toMaybe $ maximum <$> map _.completed_count <$> metrics

      H.modify_ _
        {metrics = flip Map.union year <<< Map.fromFoldable <<< map (metricToTuple max)  <$> metrics}

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

          -- TODO: profile skeleton on loading
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
                      , T.px10
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

    metricEl :: Time -> Tuple Date CountScale -> _
    metricEl t (Tuple date count) =
      HH.div
        [ HP.classes
            [ T.relative
            , T.h3
            , T.w3
            , T.roundedSm
            , T.group
            , scaleColor count
            ]
        ]
        [ HH.div
            [ HP.classes
                [ T.hidden
                , T.groupHoverBlock
                , T.z10
                , T.absolute
                , T.bottom4
                , T.left1d2
                , T.transform
                , T.negTranslateX1d2
                , T.p2
                , T.bgGray300
                , T.textWhite
                , T.textXs
                , T.w56
                , T.textCenter
                , T.roundedMd
                ]
            ]
            [ HH.span [ HP.classes [ T.fontSemibold ] ] [ HH.text $ show (scaleCount count) <> " completed" ]
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

