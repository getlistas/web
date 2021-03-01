module Listasio.Page.ListIntegrations where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Either (note)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Capability.Resource.List (class ManageList, getListBySlug)
import Listasio.Component.HTML.CardsAndSidebar as CardsAndSidebar
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Layout as Layout
import Listasio.Component.HTML.ListForm as ListForm
import Listasio.Data.List (ListWithIdAndUser)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Route (Route(..))
import Listasio.Env (UserEnv)
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RemoteData
import Slug (Slug)
import Tailwind as T
import Web.Event.Event (Event)

data Action
  = Initialize
  | Receive { currentUser :: Maybe ProfileWithIdAndEmail, listSlug :: Slug }
  | Navigate Route Event

type State
  = { currentUser :: Maybe ProfileWithIdAndEmail
    , list :: RemoteData String ListWithIdAndUser
    , slug :: Slug
    }

type Slots = ( formless :: ListForm.Slot )

component
  :: forall q o m r
   . MonadAff m
  => MonadAsk { userEnv :: UserEnv | r } m
  => Navigate m
  => ManageList m
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
    , list: NotAsked
    , slug: listSlug
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
          list <- RemoteData.fromEither <$> note "Could not get list" <$> getListBySlug { list: st.slug, user: slug }
          H.modify_ _ { list = list }
        _, _ -> pure unit

    Navigate route e -> navigate_ e route

  render :: State -> H.ComponentHTML Action Slots m
  render { currentUser, list: mbList } =
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

    content =
      case mbList of
        Success list ->
          CardsAndSidebar.layout
            [ { active: false
              , icon: Icons.userCircle
              , label: "List settings"
              , link: Just { action: Just <<< Navigate (EditList list.slug), route: EditList list.slug }
              }
            , { active: true
              , icon: Icons.gridAdd
              , label: "Integrations"
              , link: Nothing
              }
            ]
            [ { cta: Nothing
              , content:
                  HH.div
                    [ HP.classes [ T.textKiwi, T.text3xl, T.fontSemibold ] ]
                    [ HH.text "Coming soon!" ]
              , title: "RSS feed"
              , description: Nothing
              }
            ]

        -- TODO: better message
        Failure msg -> HH.div [ HP.classes [ T.textManzana ] ] [ HH.text msg ]

        -- TODO: better message
        _ -> HH.text "Loading ..."
