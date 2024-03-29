module Listasio.Page.ListIntegrations where

import Prelude

import Data.Array as Array
import Data.Either (note)
import Data.Lens (over, preview, set)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (for_)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Capability.Resource.Integration (class ManageIntegration, createRssIntegration, deleteIntegration, getListIntegrations)
import Listasio.Capability.Resource.List (class ManageList, getListBySlug)
import Listasio.Component.HTML.Button as Button
import Listasio.Component.HTML.CardsAndSidebar as CardsAndSidebar
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Input as Input
import Listasio.Component.HTML.ListForm as ListForm
import Listasio.Component.HTML.Utils (safeHref)
import Listasio.Data.DateTime as DateTime
import Listasio.Data.ID (ID)
import Listasio.Data.Integration (Integration(..), ListSubscription, RssIntegration)
import Listasio.Data.Lens (_id, _list, _newRss, _rss, _rssResult, _subscriptions)
import Listasio.Data.List (ListWithIdUserAndMeta)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Route (Route(..))
import Listasio.Form.Validation (FormError(..))
import Listasio.Store as Store
import Network.RemoteData (RemoteData(..), _Failure, _Loading, _NotAsked, _Success)
import Network.RemoteData as RemoteData
import Slug (Slug)
import Tailwind as T
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Util (fromPredicate)
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent as Mouse

_slot :: Proxy "listIntegrations"
_slot = Proxy

type Input
  = { user :: Slug, list :: Slug }

data Action
  = Initialize
  | LoadList
  | LoadIntegrations
  | Receive (Connected (Maybe ProfileWithIdAndEmail) Input)
  | OnNewChange String
  | SaveRss
  | Navigate Route Event
  | DeleteRssIntegration ID
  | DeleteSubscription ID

type State
  =
  { currentUser :: Maybe ProfileWithIdAndEmail
  , list :: RemoteData String ListWithIdUserAndMeta
  , rss :: RemoteData String (Array RssIntegration)
  , subscriptions :: RemoteData String (Array ListSubscription)
  , rssResult :: RemoteData String Unit
  , newRss :: String
  , listSlug :: Slug
  , userSlug :: Slug
  }

type Slots = (formless :: ListForm.Slot)

getRss :: Integration -> Maybe RssIntegration
getRss (RssIntegration a) = Just a
getRss _ = Nothing

getSubscription :: Integration -> Maybe ListSubscription
getSubscription (ListSubscription a) = Just a
getSubscription _ = Nothing

-- TODO: EditList approach -> only fetching when not present
component
  :: forall q o m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => ManageList m
  => ManageIntegration m
  => H.Component q Input o m
component = connect (selectEq _.currentUser) $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , receive = Just <<< Receive
      }
  }
  where
  initialState { context: currentUser, input: { list, user } } =
    { currentUser
    , userSlug: user
    , listSlug: list
    , list: NotAsked
    , rss: NotAsked
    , subscriptions: NotAsked
    , rssResult: NotAsked
    , newRss: ""
    }

  handleAction :: Action -> H.HalogenM State Action Slots o m Unit
  handleAction = case _ of
    Initialize ->
      void $ H.fork $ handleAction LoadList

    LoadList -> do
      st <- H.get
      H.modify_ _ { list = Loading }
      list <- RemoteData.fromEither <$> note "Could not get list" <$> getListBySlug { list: st.listSlug, user: st.userSlug }
      H.modify_ _ { list = list }

      void $ H.fork $ handleAction LoadIntegrations

    LoadIntegrations -> do
      mbId <- H.gets $ preview (_list <<< _Success <<< _id)

      for_ mbId \listId -> do
        H.modify_ _ { rss = Loading, subscriptions = Loading }
        res <- RemoteData.fromEither <$> note "Could not get RSS integrations" <$> getListIntegrations listId
        H.modify_ _ { rss = Array.mapMaybe getRss <$> res, subscriptions = Array.mapMaybe getSubscription <$> res }

    Receive { context: currentUser } -> do
      H.modify_ _ { currentUser = currentUser }

    Navigate route e -> navigate_ e route

    OnNewChange url -> do
      loading <- H.gets $ RemoteData.isLoading <<< _.rssResult
      unless loading do H.modify_ _ { newRss = url, rssResult = NotAsked }

    SaveRss -> do
      { newRss, rssResult, list } <- H.get
      let
        mbList =
          fromPredicate (not <<< String.null) newRss
            *> preview _NotAsked rssResult
            *> preview _Success list
      for_ mbList \{ id } -> do
        H.modify_ _ { rssResult = Loading }
        result <- RemoteData.fromEither <$> note "Failed to create RSS integration" <$> createRssIntegration { url: newRss, list: id }
        case result of
          Success newIntegration ->
            H.modify_
              $ set _rssResult (Success unit)
                  <<< over (_rss <<< _Success) (_ `Array.snoc` newIntegration)
                  <<< set _newRss ""
          r -> H.modify_ $ set _rssResult $ map (const unit) r

    DeleteRssIntegration id -> do
      mbRss <- H.gets $ preview (_rss <<< _Success)
      let
        shouldDelete = (_ == id) <<< _.id
        toDelete = Array.find shouldDelete =<< mbRss
      for_ ({ rss: _, deleted: _ } <$> mbRss <*> toDelete) $ \{ deleted } -> do
        H.modify_ $ over (_rss <<< _Success) (Array.filter (not <<< shouldDelete))
        result <- deleteIntegration id
        case result of
          Nothing -> H.modify_ $ over (_rss <<< _Success) (_ `Array.snoc` deleted)
          Just _ -> pure unit

    DeleteSubscription id -> do
      mbSubscriptions <- H.gets $ preview (_subscriptions <<< _Success)
      let
        shouldDelete = (_ == id) <<< _.id
        toDelete = Array.find shouldDelete =<< mbSubscriptions
      for_ ({ subscriptions: _, deleted: _ } <$> mbSubscriptions <*> toDelete) $ \{ deleted } -> do
        H.modify_ $ over (_subscriptions <<< _Success) (Array.filter (not <<< shouldDelete))
        result <- deleteIntegration id
        case result of
          Nothing -> H.modify_ $ over (_subscriptions <<< _Success) (_ `Array.snoc` deleted)
          Just _ -> pure unit

  render :: State -> H.ComponentHTML Action Slots m
  render { newRss, rss, rssResult, list: mbList, listSlug, userSlug } =
    HH.div [] [ header, content ]

    where
    header =
      HH.div
        [ HP.classes [ T.pt2 ] ]
        [ HH.div
            [ HP.classes [ T.flex, T.itemsCenter, T.justifyBetween ] ]
            [ HH.h1
                [ HP.classes [ T.textGray400, T.mb6, T.text4xl, T.fontBold ] ]
                [ HH.text $ RemoteData.maybe "..." _.title mbList ]
            , HH.a
                [ safeHref $ PublicList userSlug listSlug
                , HE.onClick $ Navigate (PublicList userSlug listSlug) <<< Mouse.toEvent
                , HP.classes
                    [ T.flex
                    , T.itemsCenter
                    , T.textGray300
                    , T.flexShrink0
                    ]
                ]
                [ Icons.eye [ Icons.classes [ T.w6, T.h6, T.mr2 ] ]
                , HH.text "View list"
                ]
            ]
        ]

    mkLayout cards =
      CardsAndSidebar.layout
        [ { active: false
          , icon: Icons.userCircle
          , label: "Settings"
          , link:
              Just
                { action: Navigate (EditList userSlug listSlug)
                , route: EditList userSlug listSlug
                }
          }
        , { active: true
          , icon: Icons.gridAdd
          , label: "Integrations"
          , link: Nothing
          }
        , { active: false
          , icon: Icons.documentAdd
          , label: "Import"
          , link:
              Just
                { action: Navigate (ImportResourcesList userSlug listSlug)
                , route: ImportResourcesList userSlug listSlug
                }
          }
        ]
        cards

    content =
      case mbList of
        Success _ ->
          mkLayout
            [ { cta: Nothing
              , title: "RSS feed"
              , description: Nothing
              , content:
                  HH.div
                    []
                    [ HH.div
                        [ HP.classes [ T.flex, T.itemsStart, T.spaceX4 ] ]
                        [ Input.input $ (Input.defaultProps OnNewChange)
                            { label = Nothing
                            , placeholder = Just "https://collectednotes.com/listas.rss"
                            , required = true
                            , iconBefore = unsafeCoerce $ Just Icons.rss -- TODO !!!!!!!!!
                            , value = newRss
                            , disabled = RemoteData.isLoading rssResult
                            , error = WithMsg <$> preview _Failure rssResult -- TODO validation / use form ?
                            , message = const "Creating RSS integration ..." <$> preview _Loading rssResult
                            }
                        , Button.primary (HH.text "Save") (String.null newRss || not (RemoteData.isNotAsked rssResult)) SaveRss
                        ]
                    , case rss of
                        Success items ->
                          HH.ul
                            [ HP.classes [ T.spaceY4, T.mt8 ] ]
                            $ map rssIntegrationEl items

                        Failure msg -> HH.div [ HP.classes [ T.textManzana ] ] [ HH.text msg ]
                        _ -> HH.text ""
                    ]
              }
            -- TODO: add back when list follow is reworked
            -- , { cta: Nothing
            --   , title: "Following lists"
            --   , description: Nothing
            --   , content:
            --       HH.div
            --         []
            --         [ case subscriptions of
            --             Success [] ->
            --               HH.div
            --                 [ HP.classes
            --                     [ T.textGray300
            --                     , T.textSm
            --                     ]
            --                 ]
            --                 [ HH.text "Find lists to follow on "
            --                 , HH.a
            --                     [ HE.onClick $ Navigate Discover <<< Mouse.toEvent
            --                     , safeHref Discover
            --                     , HP.classes [ T.textKiwi ]
            --                     ]
            --                     [ HH.text "Discover" ]
            --                 ]
            --             Success items ->
            --               HH.ul
            --                 [ HP.classes [ T.spaceY4, T.mt8 ] ]
            --                 $ map subscriptionIntegrationEl items

            --             Failure msg -> HH.div [ HP.classes [ T.textManzana ] ] [ HH.text msg ]
            --             _ -> HH.text ""
            --         ]
            --   }
            , { cta: Nothing
              , title: "More integrations comming soon"
              , description: Nothing
              , content:
                  HH.div
                    []
                    [ HH.p
                        [ HP.classes [ T.textGray400 ] ]
                        [ HH.text "If you have an integration in mind let as know on Twitter "
                        , HH.a
                            [ HP.href "https://twitter.com/getlistas"
                            , HP.target "_blank"
                            , HP.rel "noreferrer noopener nofollow"
                            , HP.classes
                                [ T.textKiwi
                                , T.hoverUnderline
                                , T.focusOutlineNone
                                , T.focusUnderline
                                ]
                            ]
                            [ HH.text "@getlistas" ]
                        ]
                    ]
              }
            ]

        -- TODO: better message
        Failure msg ->
          mkLayout
            [ { cta: Nothing
              , content: HH.div [ HP.classes [ T.textManzana ] ] [ HH.text msg ]
              , title: "RSS feed subscriptions"
              , description: Nothing
              }
            , { cta: Nothing
              , content: HH.div [ HP.classes [ T.textManzana ] ] [ HH.text msg ]
              , title: "Following lists"
              , description: Nothing
              }
            ]

        -- TODO: better message
        _ ->
          mkLayout
            [ { cta: Nothing
              , content: HH.div [ HP.classes [ T.textGray400 ] ] [ HH.text "Loading ..." ]
              , title: "RSS feed subscriptions"
              , description: Nothing
              }
            , { cta: Nothing
              , content: HH.div [ HP.classes [ T.textGray400 ] ] [ HH.text "Loading ..." ]
              , title: "Following lists"
              , description: Nothing
              }
            ]

    rssIntegrationEl :: RssIntegration -> _
    rssIntegrationEl i =
      HH.li
        [ HP.classes
            [ T.group
            , T.relative
            , T.bgWhite
            , T.roundedLg
            , T.shadowSm
            , T.hoverRing1
            , T.hoverRingKiwi
            ]
        ]
        [ HH.div
            [ HP.classes
                [ T.roundedLg
                , T.border
                , T.borderGray300
                , T.hoverBorderKiwi
                , T.bgWhite
                , T.px6
                , T.py4
                , T.hoverBorderGray400
                , T.smFlex
                , T.smJustifyBetween
                ]
            ]
            [ HH.div
                [ HP.classes [ T.flex, T.itemsCenter ] ]
                [ HH.div
                    [ HP.classes [ T.textSm ] ]
                    [ HH.p
                        [ HP.classes [ T.fontMedium, T.textGray400 ] ]
                        [ i.rss.url
                            # String.replace (String.Pattern "https://") (String.Replacement "")
                            # String.replace (String.Pattern "http://") (String.Replacement "")
                            # String.replace (String.Pattern "www.") (String.Replacement "")
                            # HH.text
                        ]
                    , HH.div
                        [ HP.classes [ T.textGray500 ] ]
                        [ HH.p
                            [ HP.classes [ T.smInline ] ]
                            [ HH.text $ "Subscribed " <> DateTime.toDisplayMonthDayYear i.created_at ]
                        ]
                    ]
                ]
            , HH.div
                [ HP.classes
                    [ T.mt2
                    , T.flex
                    , T.textSm
                    , T.smMt0
                    , T.smBlock
                    , T.smMl4
                    , T.smTextRight
                    ]
                ]
                [ HH.div
                    [ HP.classes [ T.flex ] ]
                    [ HH.button
                        [ HE.onClick $ const $ DeleteRssIntegration i.id
                        , HP.classes [ T.cursorPointer ]
                        , HP.type_ HP.ButtonButton
                        ]
                        [ Icons.trash [ Icons.classes [ T.w6, T.h6, T.textGray400, T.hoverTextManzana ] ]
                        ]
                    ]
                ]
            ]
        ]

-- subscriptionIntegrationEl :: ListSubscription -> _
-- subscriptionIntegrationEl i =
--   HH.li
--     [ HP.classes
--         [ T.group
--         , T.relative
--         , T.bgWhite
--         , T.roundedLg
--         , T.shadowSm
--         , T.hoverRing1
--         , T.hoverRingKiwi
--         ]
--     ]
--     [ HH.div
--         [ HP.classes
--             [ T.roundedLg
--             , T.border
--             , T.borderGray300
--             , T.hoverBorderKiwi
--             , T.bgWhite
--             , T.px6
--             , T.py4
--             , T.hoverBorderGray400
--             , T.smFlex
--             , T.smJustifyBetween
--             ]
--         ]
--         [ HH.div
--             [ HP.classes [ T.flex, T.itemsCenter ] ]
--             [ HH.div
--                 [ HP.classes [ T.textSm ] ]
--                 [ HH.p
--                     [ HP.classes [ T.fontMedium, T.textGray400 ] ]
--                     [ HH.text $ "List " <> ID.toString i.listas_subscription.list ]
--                 , HH.div
--                     [ HP.classes [ T.textGray500 ] ]
--                     [ HH.p
--                         [ HP.classes [ T.smInline ] ]
--                         [ HH.text $ "Followed " <> DateTime.toDisplayMonthDayYear i.created_at ]
--                     ]
--                 ]
--             ]
--         , HH.div
--             [ HP.classes
--                 [ T.mt2
--                 , T.flex
--                 , T.textSm
--                 , T.smMt0
--                 , T.smBlock
--                 , T.smMl4
--                 , T.smTextRight
--                 ]
--             ]
--             [ HH.div
--                 [ HP.classes [ T.flex ] ]
--                 [ HH.button
--                     [ HE.onClick $ const $ DeleteSubscription i.id
--                     , HP.classes [ T.cursorPointer ]
--                     , HP.type_ HP.ButtonButton
--                     ]
--                     [ Icons.trash [ Icons.classes [ T.w6, T.h6, T.textGray400, T.hoverTextManzana ] ]
--                     ]
--                 ]
--             ]
--         ]
--     ]
