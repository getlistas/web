module Listasio.Page.ListIntegrations where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Either (note)
import Data.Lens (preview)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (for_)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Capability.Resource.Integration (class ManageIntegration, createIntegration)
import Listasio.Capability.Resource.List (class ManageList, getListBySlug)
import Listasio.Component.HTML.Button as Button
import Listasio.Component.HTML.CardsAndSidebar as CardsAndSidebar
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Input as Input
import Listasio.Component.HTML.Layout as Layout
import Listasio.Component.HTML.ListForm as ListForm
import Listasio.Data.List (ListWithIdAndUser)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Route (Route(..))
import Listasio.Env (UserEnv)
import Network.RemoteData (RemoteData(..), _NotAsked, _Success, fromEither, isNotAsked)
import Slug (Slug)
import Tailwind as T
import Unsafe.Coerce (unsafeCoerce)
import Util (fromPredicate)
import Web.Event.Event (Event)

data Action
  = Initialize
  | Receive { currentUser :: Maybe ProfileWithIdAndEmail, listSlug :: Slug }
  | OnNewChange String
  | SaveRss
  | Navigate Route Event

type State
  = { currentUser :: Maybe ProfileWithIdAndEmail
    , list :: RemoteData String ListWithIdAndUser
    , rss :: RemoteData String Unit
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
      , initialize = Just Initialize
      }
  }
  where
  initialState { currentUser, listSlug } =
    { currentUser
    , slug: listSlug
    , list: NotAsked
    , rss: NotAsked
    , newRss: ""
    }

  handleAction :: Action -> H.HalogenM State Action Slots o m Unit
  handleAction = case _ of
    Initialize -> pure unit

    Receive { currentUser } -> do
      st <- H.get
      H.modify_ _ { currentUser = currentUser }
      case st.currentUser, currentUser of
        Nothing, Just { slug } -> do
          H.modify_ _ { list = Loading }
          list <- fromEither <$> note "Could not get list" <$> getListBySlug { list: st.slug, user: slug }
          H.modify_ _ { list = list }
        _, _ -> pure unit

    Navigate route e -> navigate_ e route

    OnNewChange url -> do
      notAsked <- H.gets $ isNotAsked <<< _.rss
      when notAsked do H.modify_ _ { newRss = url }

    SaveRss -> do
      { newRss, rss, list } <- H.get
      let mbList =
            fromPredicate (not <<< String.null) newRss
              *> preview _NotAsked rss
              *> preview _Success list
      for_ mbList \{ id } -> do
        H.modify_ _ { rss = Loading }
        result <- createIntegration { url: newRss, list: id }
        case result of
          Just _ -> H.modify_ _ { rss = Success unit }
          Nothing -> H.modify_ _ { rss = Failure "Failed to create RSS integration" }

  render :: State -> H.ComponentHTML Action Slots m
  render { newRss, rss, currentUser, list: mbList } =
    Layout.dashboard
      currentUser
      Navigate
      Nothing
      $ HH.div [] [ header, content ]
    where
    header =
      HH.h1
        [ HP.classes [ T.textGray400, T.mb6, T.text4xl, T.fontBold ] ]
        [ HH.text "List Settings" ]

    mkLayout list cards =
      CardsAndSidebar.layout
        [ { active: false
          , icon: Icons.userCircle
          , label: "List settings"
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
              , content:
                  HH.div
                    [ HP.classes [ T.flex, T.itemsEnd, T.spaceX4 ] ]
                    [ Input.input $ Input.defaultProps
                        { label = Just "Add RSS Feed"
                        , placeholder = Just "https://collectednotes.com/listas.rss"
                        , required = true
                          -- TODO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                        , iconBefore = unsafeCoerce $ Just Icons.rss
                        , action = Just <<< OnNewChange
                        , value = newRss
                        , disabled = not $ isNotAsked rss
                        }
                    , Button.primary (HH.text "Save") (not $ isNotAsked rss) $ Just SaveRss
                    ]
              , title: "RSS feed"
              , description: Nothing
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
