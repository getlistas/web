module Listasio.Component.HTML.List where

import Prelude

import Data.Array (head, null, snoc)
import Data.Either (note)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Listasio.Capability.Resource.Resource (class ManageResource, getListResources)
import Listasio.Component.HTML.Utils (maybeElem, whenElem)
import Listasio.Data.List (ListWithIdAndUser)
import Listasio.Data.Resource (ListResource)
import Network.RemoteData (RemoteData(..), fromEither, toMaybe)
import Tailwind as T

type Slot = H.Slot Query Void String

_list = SProxy :: SProxy "list"

data Action
  = Initialize

type Input
  = { list :: ListWithIdAndUser }

data Query a
  = ResourceAdded ListResource a

type State
  = { list :: ListWithIdAndUser
    , resources :: RemoteData String (Array ListResource)
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
  initialState { list } = { list, resources: NotAsked }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> do
      H.modify_ _ { resources = Loading }
      { list } <- H.get
      resources <- fromEither <$> note "Failed to load list resources" <$> getListResources list._id."$oid"
      H.modify_ _ { resources = resources }

  handleQuery :: forall slots a. Query a -> H.HalogenM State Action slots o m (Maybe a)
  handleQuery = case _ of
    ResourceAdded resource a -> do
      H.modify_ \s -> s { resources = flip snoc resource <$> s.resources }
      pure $ Just a

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { list, resources } =
    HH.div
      [ HP.classes [ T.m4, T.p2, T.border4, T.borderIndigo400 ] ]
      [ HH.div [ HP.classes [ T.textLg, T.borderB2, T.borderGray200, T.mb4 ] ] [ HH.text list.title ]
      , maybeElem list.description \des -> HH.div [ HP.classes [ T.textSm, T.mb4 ] ] [ HH.text des ]
      , whenElem (not $ null list.tags) \_ ->
          HH.div
            [ HP.classes [ T.flex, T.textSm ] ]
            $ map ((\t -> HH.div [ HP.classes [ T.mr2 ] ] [ HH.text t ]) <<< ("#" <> _)) list.tags
      , maybeElem (head =<< toMaybe resources) \next ->
          HH.div
            [ HP.classes [ T.mt4 ] ]
            [ HH.text "Next item: "
            , HH.a [ HP.classes [ T.underline ], HP.href next.url ] [ HH.text next.title ]
            ]
      ]
