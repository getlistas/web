module Listasio.Component.HTML.List where

import Prelude

import Data.Array (cons, drop, head, null, snoc, tail)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), hush, note)
import Data.Filterable (class Filterable, filter)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.String (Pattern(..), Replacement(..), contains, replace)
import Data.String.Regex (regex, match) as Regex
import Data.String.Regex.Flags (noFlags) as Regex
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Capability.Resource.Resource (class ManageResource, completeResource, getListResources)
import Listasio.Component.HTML.Utils (maybeElem, whenElem)
import Listasio.Data.List (ListWithIdAndUser)
import Listasio.Data.Resource (ListResource)
import Network.RemoteData (RemoteData(..), fromEither, toMaybe)
import Tailwind as T

type Slot = H.Slot Query Void String

_list = SProxy :: SProxy "list"

data Action
  = Initialize
  | ToggleShowMore
  | CompleteResource ListResource

type Input
  = { list :: ListWithIdAndUser }

data Query a
  = ResourceAdded ListResource a

type State
  = { list :: ListWithIdAndUser
    , resources :: RemoteData String (Array ListResource)
    , showMore :: Boolean
    , markingAsDone :: Boolean
    }

component :: forall o m.
     MonadAff m
  => ManageResource m
  => H.Component HH.HTML Query Input o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , initialize = Just Initialize
      }
  }
  where
  initialState { list } = { list, resources: NotAsked, showMore: false, markingAsDone: false }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> do
      H.modify_ _ { resources = Loading }
      { list } <- H.get
      resources <- fromEither <$> note "Failed to load list resources" <$> getListResources list._id."$oid"
      H.modify_ _ { resources = resources }

    ToggleShowMore ->
      H.modify_ \s -> s { showMore = not s.showMore }

    CompleteResource toComplete -> do
      H.modify_ \s -> s { resources = drop 1 <$> s.resources, markingAsDone = true }
      result <- completeResource toComplete
      when (isNothing result)$ H.modify_ \s -> s { resources = cons toComplete <$> s.resources }
      H.modify_ _ { markingAsDone = false }

  handleQuery :: forall slots a. Query a -> H.HalogenM State Action slots o m (Maybe a)
  handleQuery = case _ of
    ResourceAdded resource a -> do
      H.modify_ \s -> s { resources = flip snoc resource <$> s.resources }
      pure $ Just a

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { list, resources, showMore, markingAsDone } =
    HH.div
      [ HP.classes [ T.border2, T.borderKiwi, T.roundedMd ] ]
      [ header
      , maybeElem (head =<< toMaybe resources) toRead
      , footer
      ]
    where
    tag text =
      HH.span
        [ HP.classes [ T.leadingNormal, T.mr1, T.mb1, T.px2, T.bgDurazno, T.textWhite, T.textXs, T.roundedMd ] ]
        [ HH.text text ]

    shortUrl url =
      maybeElem mbShort \short ->
        HH.div
          [ HP.classes [ T.textGray300, T.textSm, T.mb1, T.mr2, T.flex, T.itemsCenter ] ]
          [ HH.img [ HP.classes [ T.inlineBlock, T.mr1 ], HP.src $ "https://s2.googleusercontent.com/s2/favicons?domain_url=" <> url ]
          , HH.text short
          ]
      where
      mbShort =
        map toWebsiteName
          $ map (replace (Pattern "www.") (Replacement ""))
          $ (NEA.head =<< _)
          $ join
          $ hush
          $ (\rgx -> Regex.match <$> rgx <*> Right url)
          -- https://regexr.com/5jf24
          $ Regex.regex "[a-zA-Z-_]+(?:\\.[a-zA-Z-_]+)+" Regex.noFlags

      toWebsiteName = case _ of
        str | contains (Pattern "twitter.com") str -> "Twitter"
        str | contains (Pattern "music.youtube.com") str -> "YouTube Music"
        str | contains (Pattern "youtube.com") str -> "YouTube"
        str | contains (Pattern "youtu.be") str -> "YouTube"
        str | contains (Pattern "vimeo.com") str -> "Vimeo"
        str | contains (Pattern "reddit.com") str -> "Reddit" -- TODO: include the subreddit
        str | contains (Pattern "spotify.com") str -> "Spotify"
        str | contains (Pattern "medium.com") str -> "Medium"
        str | contains (Pattern "dev.to") str -> "DEV Community"
        str | contains (Pattern "itunes.apple.com") str && contains (Pattern "podcast") str -> "Apple Podcasts"
        str | contains (Pattern "podcasts.apple.com") str -> "Apple Podcasts"
        str | contains (Pattern "itunes.apple.com") str && contains (Pattern "music") str -> "Apple Music"
        str | contains (Pattern "music.apple.com") str -> "Apple Music"
        str | contains (Pattern "twitch.tv") str -> "Twitch"
        str -> str

    toRead next =
      HH.div
        [ HP.classes [ T.px2, T.py2 ] ]
        [ HH.a
            [ HP.href next.url, HP.target "_blank", HP.rel "noreferrer noopener nofollow",  HP.classes [ T.cursorPointer ] ]
            [ HH.div [] []
            , HH.div
                []
                [ HH.div [ HP.classes [ T.textBase, T.textGray400 ] ] [ HH.text next.title ]
                , maybeElem next.description \des -> HH.div [ HP.classes [ T.textSm, T.textGray400 ] ] [ HH.text des ]
                ]
            ]
        , HH.div
            [ HP.classes [ T.mt4, T.flex, T.justifyBetween, T.itemsStart ] ]
            [ HH.div
                [ HP.classes [ T.flex, T.flexWrap, T.itemsCenter ] ]
                [ shortUrl next.url
                -- TODO: resource tags
                , whenElem (not $ null list.tags) \_ ->
                    HH.div [ HP.classes [ T.flex, T.flexWrap ] ] $ map tag list.tags
                ]
            , HH.button
                [ HE.onClick \_ -> Just $ CompleteResource next
                , HP.classes
                    [ T.flexNone
                    , T.cursorPointer
                    , T.leadingNormal
                    , T.w32
                    , T.bgKiwi
                    , T.textWhite
                    , T.textXs
                    , T.roundedMd
                    , T.shadowMd
                    , T.hoverBgGreen700
                    , T.focusOutlineNone
                    , T.focusRing2
                    , T.focusRingGreen900
                    , T.disabledCursorNotAllowed
                    , T.disabledOpacity50
                    ]
                , HP.disabled markingAsDone
                ]
                [ HH.text "Mark as done" ]
            ]
        ]

    header =
      HH.div
        [ HP.classes [ T.p2, T.borderB2, T.borderGray200 ] ]
        [ HH.div
            [ HP.classes [ T.flex, T.justifyBetween, T.itemsCenter ] ]
            [ HH.div [ HP.classes [ T.text2xl, T.textGray400, T.fontBold ] ] [ HH.text list.title ]
            , HH.div
                [ HP.classes [ T.ml6 ] ]
                [ HH.span [ HP.classes [ T.mr2 ] ] [ HH.text "🔗" ]
                , HH.span [ HP.classes [ T.textLg, T.textGray400 ] ] [ HH.text "14" ]
                , HH.span [ HP.classes [ T.textLg, T.textGray400, T.mx1 ] ] [ HH.text "/" ]
                , HH.span [ HP.classes [ T.textLg, T.textGray300 ] ] [ HH.text "20" ]
                ]
            ]
        , HH.div [ HP.classes [ T.textSm, T.textGray200 ] ] [ HH.text "Last seen 8 days ago" ]
        ]

    mbResources = filterNotEmpty $ toMaybe resources
    mbRest = filterNotEmpty $ tail =<< mbResources
    hasMore = isJust mbRest

    footer =
      maybeElem mbResources \_ ->
        HH.div
          [ HP.classes [ T.p1, T.borderT2, T.borderGray200, T.flex, T.justifyCenter ] ]
          [ HH.div
              [ HP.classes [] ]
              [ HH.div
                  [ HP.classes [ T.flex, T.justifyCenter ] ]
                  [ HH.button
                      [ HE.onClick \_ -> Just ToggleShowMore
                      , HP.disabled $ not hasMore
                      , HP.classes
                          [ T.focusOutlineNone
                          , T.flex
                          , T.flexCol
                          , T.itemsCenter
                          , T.disabledCursorNotAllowed
                          , T.disabledOpacity50
                          ]
                      ]
                      [ HH.span
                          [ HP.classes [ T.textSm, T.textGray200 ] ]
                          [ HH.text $ if showMore then "Show less" else "Show more" ]
                      , HH.span
                          [ HP.classes [ T.textGray400 ] ]
                          [ HH.text $ if showMore then "▲" else "▼" ]
                      ]
                  ]
              , maybeElem mbRest \rest ->
                  whenElem showMore \_ ->
                    HH.div
                      [ HP.classes [ T.mt4, T.flex, T.flexCol ] ]
                      $ map (\{ url, title } -> HH.a [ HP.classes [ T.underline ], HP.href url ] [ HH.text title ]) rest
              ]
          ]

filterNotEmpty :: forall t a. Filterable t => t (Array a) -> t (Array a)
filterNotEmpty = filter (not <<< null)
