module Listasio.Page.Profile where

import Prelude

import Data.Either (note)
import Data.Array (cons, drop, range)
import Data.Date (Date, Weekday(..), adjust, weekday)
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
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Capability.Now (class Now, nowDate, nowTime)
import Listasio.Capability.Resource.User (class ManageUser, userMetrics, userBySlug)
import Listasio.Component.HTML.Wip as Wip
import Listasio.Data.Avatar as Avatar
import Listasio.Data.DateTime as DateTime
import Listasio.Data.Metrics (Metric)
import Listasio.Data.Profile (ProfileWithIdAndEmail, PublicProfile)
import Listasio.Data.Route (Route)
import Listasio.Data.Username as Username
import Listasio.Store as Store
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RemoteData
import Slug (Slug)
import Tailwind as T
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)

_slot :: Proxy "profile"
_slot = Proxy

type Input
  = { slug :: Slug }

data Action
  = Initialize
  | Receive (Connected (Maybe ProfileWithIdAndEmail) Input)
  | LoadUser
  | LoadMetrics
  | Navigate Route Event

type State
  =
  { slug :: Slug
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

type CountScale
  = Maybe { color :: Scale, count :: Int }

scaleColor :: CountScale -> H.ClassName
scaleColor Nothing = T.bgGray100
scaleColor (Just { color: Highest }) = T.bgGreen800
scaleColor (Just { color: High }) = T.bgGreen700
scaleColor (Just { color: Mid }) = T.bgGreen500
scaleColor (Just { color: Low }) = T.bgGreen300
scaleColor (Just { color: Lowest }) = T.bgGreen200

toScale :: Int -> Int -> CountScale
toScale _ 0 = Nothing
toScale max n =
  case toNumber n / toNumber max of
    r | r == 1.0 -> Just { color: Highest, count: n }
    r | r > 0.75 -> Just { color: High, count: n }
    r | r > 0.5 -> Just { color: Mid, count: n }
    r | r > 0.25 -> Just { color: Low, count: n }
    _ -> Just { color: Lowest, count: n }

metricToTuple :: Int -> Metric -> Tuple Date CountScale
metricToTuple max m = Tuple (date m.date) $ toScale max m.completed_count

yearDays :: Int
yearDays = 364 -- 52 * 7

halfYear :: Int
halfYear = 182

threeMonths :: Int
threeMonths = 84

yearOffset :: Weekday -> Int
yearOffset Sunday = yearDays - 6
yearOffset Monday = yearDays - 5
yearOffset Tuesday = yearDays - 4
yearOffset Wednesday = yearDays - 3
yearOffset Thursday = yearDays - 2
yearOffset Friday = yearDays - 1
yearOffset Saturday = yearDays

allDaysPastYear :: Date -> Map Date CountScale
allDaysPastYear date =
  Map.fromFoldable
    $ maybe [] (cons $ Tuple date Nothing)
    $ traverse (map Tuple.swap <<< sequence <<< Tuple Nothing <<< flip adjust date <<< negateDuration <<< Days <<< toNumber)
    $ range 1
    $ (_ - 1)
    $ yearOffset
    $ weekday date

component
  :: forall q o m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => ManageUser m
  => Navigate m
  => Now m
  => H.Component q Input o m
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
  initialState { context: currentUser, input: { slug } } =
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
      H.modify_ _ { time = Just time }

      void $ H.fork $ handleAction LoadUser
      void $ H.fork $ handleAction LoadMetrics

    Receive { context: currentUser } -> do
      H.modify_ _ { currentUser = currentUser }

    LoadUser -> do
      { slug } <- H.get
      H.modify_ _ { profile = Loading }
      profile <- RemoteData.fromEither <$> note "Could not fetch user profile" <$> userBySlug slug
      H.modify_ _ { profile = profile }

    LoadMetrics -> do
      { slug } <- H.get

      H.modify_ _ { metrics = Loading }
      metrics <- RemoteData.fromEither <$> note "Could not fetch user metrics" <$> userMetrics slug
      year <- allDaysPastYear <$> nowDate

      let max = fromMaybe 0 $ join $ RemoteData.toMaybe $ maximum <$> map _.completed_count <$> metrics

      H.modify_ _
        { metrics = flip Map.union year <<< Map.fromFoldable <<< map (metricToTuple max) <$> metrics }

    Navigate route e -> navigate_ e route

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { profile, metrics, time } =
    HH.div
      []
      [ Wip.elem
      , case profile of
          Success { name, avatar } ->
            profileHeader { name: Username.toString name, avatar }

          Failure _ -> HH.text "User Profile"

          -- TODO: profile skeleton on loading
          _ -> HH.text "..."

      , case metrics, time of
          Success ms, Just t ->
            HH.div
              [ HP.classes [ T.maxW5xl, T.mxAuto, T.px4, T.smPx6, T.lgPx8, T.mt12 ] ]
              [ HH.div
                  [ HP.classes [ T.textLg, T.textGray400, T.fontSemibold, T.mb4, T.borderB, T.borderGray200 ] ]
                  [ HH.text "Activity" ]
              , HH.div
                  [ HP.classes [ T.mxAuto, T.wMax, T.flex, T.smHidden ] ]
                  [ HH.div
                      [ HP.classes
                          [ T.grid
                          , T.gridRows7
                          , T.gridFlowCol
                          , T.gap1
                          , T.pr1
                          ]
                      ]
                      [ gridDay "Mon" T.rowStart2
                      , gridDay "Wed" T.rowStart4
                      , gridDay "Fri" T.rowStart6
                      ]
                  , HH.div
                      [ HP.classes
                          [ T.grid
                          , T.gridRows7
                          , T.gridCols12
                          , T.gridFlowCol
                          , T.gap1
                          ]
                      ]
                      $ map (metricEl t)
                      $ drop (yearDays - threeMonths)
                      $ Map.toUnfoldable ms
                  ]
              , HH.div
                  [ HP.classes [ T.mxAuto, T.wMax, T.hidden, T.smFlex, T.lgHidden ] ]
                  [ HH.div
                      [ HP.classes
                          [ T.grid
                          , T.gridRows7
                          , T.gridFlowCol
                          , T.gap1
                          , T.pr1
                          ]
                      ]
                      [ gridDay "Mon" T.rowStart2
                      , gridDay "Wed" T.rowStart4
                      , gridDay "Fri" T.rowStart6
                      ]
                  , HH.div
                      [ HP.classes
                          [ T.grid
                          , T.gridRows7
                          , T.gridCols26
                          , T.gridFlowCol
                          , T.gap1
                          ]
                      ]
                      $ map (metricEl t)
                      $ drop (yearDays - halfYear)
                      $ Map.toUnfoldable ms
                  ]
              , HH.div
                  [ HP.classes [ T.mxAuto, T.wMax, T.hidden, T.lgFlex ] ]
                  [ HH.div
                      [ HP.classes
                          [ T.grid
                          , T.gridRows7
                          , T.gridFlowCol
                          , T.gap1
                          , T.pr1
                          ]
                      ]
                      [ gridDay "Mon" T.rowStart2
                      , gridDay "Wed" T.rowStart4
                      , gridDay "Fri" T.rowStart6
                      ]
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
              ]
          _, _ -> HH.text ""
      ]

    where
    gridDay day rowCx =
      HH.div
        [ HP.classes
            [ T.flex
            , T.justifyCenter
            , T.flexCol
            , T.textXs
            , T.h3
            , T.textGray300
            , rowCx
            ]
        ]
        [ HH.text day ]

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
            [ HH.span [ HP.classes [ T.fontSemibold ] ] [ HH.text $ show (maybe 0 _.count count) <> " completed" ]
            , HH.span [] [ HH.text $ " on " <> DateTime.toDisplayMonthDayYear (DateTime date t) ]
            ]
        ]

    profileHeader { name, avatar } =
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

