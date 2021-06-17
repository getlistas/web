module Listasio.Component.HTML.PublicResources where

import Prelude

import Data.Either (note)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Properties as HP
import Listasio.Capability.Navigate (class Navigate)
import Listasio.Capability.Resource.Resource (class ManageResource, getPublicListResources)
import Listasio.Component.HTML.Utils (maybeElem)
import Listasio.Data.ID as ID
import Listasio.Data.Resource (ListResource)
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RemoteData
import Slug (Slug)
import Tailwind as T
import Util (takeDomain)

type Slot id = forall query. H.Slot query Void id

_publicResources = SProxy :: SProxy "publicResources"

type Input
  = { listSlug :: Slug
    , authorSlug :: Slug
    }

data Action
  = Initialize

type State
  = { authorSlug :: Slug
    , listSlug :: Slug
    , resources :: RemoteData String (Array ListResource)
    }

component
  :: forall q o m
   . MonadAff m
  => ManageResource m
  => Navigate m
  => H.Component HH.HTML q Input o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }
  where
  initialState {authorSlug, listSlug} =
    {authorSlug, listSlug, resources: NotAsked}

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> do
      {authorSlug, listSlug} <- H.get

      H.modify_ _ {resources = Loading}

      resources <- RemoteData.fromEither <$> note "Failed to load resources" <$> getPublicListResources {user: authorSlug, list: listSlug}

      H.modify_ _ {resources = resources}


  render :: forall slots. State -> H.ComponentHTML Action slots m
  render {resources} =
    case resources of
      NotAsked -> HH.text ""

      -- TODO: skeleton
      Loading -> HH.text "..."

      Success rs ->
        HK.div
          [ HP.classes
              [ T.flex
              , T.flexCol
              ]
          ]
          $ listResource <$> rs

      -- TODO: error message element
      Failure msg -> HH.text msg

    where
    listResource :: ListResource -> Tuple String _
    listResource {id, url, thumbnail, title, description} =
      Tuple (ID.toString id)
        $ HH.div
            [ HP.classes
                [ T.flex
                , T.justifyBetween
                , T.border
                , T.borderKiwi
                , T.roundedLg
                , T.bgWhite
                , T.mb4
                ]
            ]
            [ HH.div
                [ HP.classes [ T.p4, T.truncate ] ]
                [ HH.a
                    [ HP.classes [ T.textGray400, T.fontMedium, T.truncate ]
                    , HP.href url
                    , HP.target "_blank"
                    , HP.rel "noreferrer noopener nofollow"
                    ]
                    [ HH.text title ]

                , maybeElem (takeDomain url) \short ->
                    HH.div
                      [ HP.classes [ T.flex, T.itemsCenter, T.mt1 ] ]
                      [ HH.img [ HP.classes [ T.w4, T.h4, T.mr2 ], HP.src $ "https://s2.googleusercontent.com/s2/favicons?domain_url=" <> url ]
                      , HH.div
                          [ HP.classes [ T.textGray300, T.textXs ] ]
                          [ HH.text short ]
                      ]

                , maybeElem description \d ->
                    HH.div
                      [ HP.classes [ T.mt2, T.truncate, T.textGray400, T.textSm ] ]
                      [ HH.text d ]
                ]
            , maybeElem thumbnail \u ->
                HH.div
                  [ HP.classes [ T.w40, T.h36, T.py4, T.pr4, T.flexShrink0 ] ]
                  [ HH.img
                      [ HP.alt title
                      , HP.src u
                      , HP.classes
                          [ T.wFull
                          , T.hFull
                          , T.objectCover
                          , T.objectCenter
                          , T.roundedMd
                          ]
                      ]
                ]
            ]
