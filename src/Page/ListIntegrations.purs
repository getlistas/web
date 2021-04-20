module Listasio.Page.ListIntegrations where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
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
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Capability.Resource.Integration (class ManageIntegration, createRssIntegration, deleteIntegration, getListIntegrations)
import Listasio.Capability.Resource.List (class ManageList, getListBySlug)
import Listasio.Component.HTML.Button as Button
import Listasio.Component.HTML.CardsAndSidebar as CardsAndSidebar
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Input as Input
import Listasio.Component.HTML.ListForm as ListForm
import Listasio.Data.DateTime as DateTime
import Listasio.Data.ID (ID)
import Listasio.Data.Integration (RssIntegration)
import Listasio.Data.Lens (_id, _newRss, _rss, _rssResult)
import Listasio.Data.List (ListWithIdAndUser)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Route (Route(..))
import Listasio.Env (UserEnv)
import Listasio.Form.Validation (FormError(..))
import Network.RemoteData (RemoteData(..), _Failure, _Loading, _NotAsked, _Success)
import Network.RemoteData as RemoteData
import Slug (Slug)
import Tailwind as T
import Unsafe.Coerce (unsafeCoerce)
import Util (fromPredicate)
import Web.Event.Event (Event)

data Action
  = Receive { currentUser :: Maybe ProfileWithIdAndEmail, listSlug :: Slug }
  | OnNewChange String
  | SaveRss
  | Navigate Route Event
  | DeleteIntegration ID

type State
  = { currentUser :: Maybe ProfileWithIdAndEmail
    , list :: RemoteData String ListWithIdAndUser
    , rss :: RemoteData String (Array RssIntegration)
    , rssResult :: RemoteData String Unit
    , newRss :: String
    , slug :: Slug
    }

type Slots = ( formless :: ListForm.Slot )

component
  :: forall q o m r
   . MonadAff m
  => MonadAsk { userEnv :: UserEnv | r } m
  => Navigate m
  => ManageList m
  => ManageIntegration m
  => H.Component HH.HTML q { listSlug :: Slug } o m
component = Connect.component $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }
  where
  initialState { currentUser, listSlug } =
    { currentUser
    , slug: listSlug
    , list: NotAsked
    , rss: NotAsked
    , rssResult: NotAsked
    , newRss: ""
    }

  handleAction :: Action -> H.HalogenM State Action Slots o m Unit
  handleAction = case _ of
    Receive { currentUser } -> do
      st <- H.get
      H.modify_ _ { currentUser = currentUser }
      case st.currentUser, currentUser of
        Nothing, Just { slug } -> do
          H.modify_ _ { list = Loading }
          list <- RemoteData.fromEither <$> note "Could not get list" <$> getListBySlug { list: st.slug, user: slug }
          H.modify_ _ { list = list }

          case preview (_Success <<< _id) list of
            Just listId -> do
              rss <- RemoteData.fromEither <$> note "Could not get RSS integrations" <$> getListIntegrations listId
              H.modify_ _ { rss = rss }
            Nothing -> pure unit

        _, _ -> pure unit

    Navigate route e -> navigate_ e route

    OnNewChange url -> do
      loading <- H.gets $ RemoteData.isLoading <<< _.rssResult
      unless loading do H.modify_ _ { newRss = url, rssResult = NotAsked }

    SaveRss -> do
      {newRss, rssResult, list} <- H.get
      let mbList =
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

    DeleteIntegration id -> do
      mbRss <- H.gets $ preview (_rss <<< _Success)
      let shouldDelete = (_ == id) <<< _.id
          toDelete = Array.find shouldDelete =<< mbRss
      for_ ({rss: _, deleted: _} <$> mbRss <*> toDelete) $ \{rss, deleted} -> do
        H.modify_ $ over (_rss <<< _Success) (Array.filter (not <<< shouldDelete))
        result <- deleteIntegration id
        case result of
          Nothing -> H.modify_ $ over (_rss <<< _Success) (_ `Array.snoc` deleted)
          Just _ -> pure unit

  render :: State -> H.ComponentHTML Action Slots m
  render { newRss, rss, rssResult, currentUser, list: mbList } =
    HH.div [] [ header, content ]

    where
    header =
      HH.div
        [ HP.classes [ T.pt2 ] ]
        [ HH.h1
            [ HP.classes [ T.textGray400, T.mb6, T.text4xl, T.fontBold ] ]
            [ HH.text $ RemoteData.maybe "..." _.title mbList  ]
        ]

    mkLayout list cards =
      CardsAndSidebar.layout
        [ { active: false
          , icon: Icons.userCircle
          , label: "Settings"
          , link:
              map
                ( \{slug} ->
                    { action: Just <<< Navigate (EditList slug)
                    , route: EditList slug
                    }
                )
                list
          }
        , { active: true
          , icon: Icons.gridAdd
          , label: "Integrations"
          , link: Nothing
          }
        ]
        cards

    content =
      case mbList of
        Success list ->
          mkLayout
            (Just list)
            [ { cta: Nothing
              , title: "RSS feed"
              , description: Nothing
              , content:
                  HH.div
                    []
                    [ HH.div
                        [ HP.classes [ T.flex, T.itemsStart, T.spaceX4 ] ]
                        [ Input.input $ Input.defaultProps
                            { label = Nothing
                            , placeholder = Just "https://collectednotes.com/listas.rss"
                            , required = true
                            , iconBefore = unsafeCoerce $ Just Icons.rss -- TODO !!!!!!!!!
                            , action = Just <<< OnNewChange
                            , value = newRss
                            , disabled = RemoteData.isLoading rssResult
                            , error = WithMsg <$> preview _Failure rssResult -- TODO validation / use form ?
                            , message = const "Creating RSS integration ..." <$> preview _Loading rssResult
                            }
                        , Button.primary (HH.text "Save") (String.null newRss || not (RemoteData.isNotAsked rssResult)) $ Just SaveRss
                        ]
                    , case rss of
                        Success [] -> HH.text ""
                        Success items ->
                          HH.ul
                            [ HP.classes [ T.spaceY4, T.mt8 ] ]
                            $ map rssIntegrationEl items

                        Failure msg -> HH.div [ HP.classes [ T.textManzana ] ] [ HH.text msg ]
                        _ -> HH.text ""
                    ]
              }
            ]

        -- TODO: better message
        Failure msg ->
          mkLayout
            Nothing
            [ { cta: Nothing
              , content: HH.div [ HP.classes [ T.textManzana ] ] [ HH.text msg ]
              , title: "RSS feed"
              , description: Nothing
              }
            ]

        -- TODO: better message
        _ ->
          mkLayout
            Nothing
            [ { cta: Nothing
              , content: HH.div [ HP.classes [ T.textGray400 ] ] [ HH.text "Loading ..." ]
              , title: "RSS feed"
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
                        [ HP.classes [ T.fontMedium, T.textGray900 ] ]
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
                            [ HH.text $ "Created " <> DateTime.toDisplayMonthDayYear i.created_at ]
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
                        [ HE.onClick \_ -> Just $ DeleteIntegration i.id
                        , HP.classes [ T.cursorPointer ]
                        , HP.type_ HP.ButtonButton
                        ]
                        [ Icons.trash [ Icons.classes [ T.w6, T.h6, T.textGray400, T.hoverTextManzana ] ]
                        ]
                    ]
                ]
            ]
        ]
