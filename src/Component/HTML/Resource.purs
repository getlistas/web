module Listasio.Component.HTML.Resource where

import Prelude

import Data.Array (find)
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..), isJust)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Tag as Tag
import Listasio.Component.HTML.Utils (maybeElem, safeHref)
import Listasio.Data.List (ListWithIdUserAndMeta)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Resource (ListResource, titleOrUrl)
import Listasio.Data.Route (Route(..))
import Tailwind as T
import Util (takeDomain)
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent as Mouse

resource :: forall i p. Maybe ProfileWithIdAndEmail -> Maybe (Route -> Event -> p) -> Array ListWithIdUserAndMeta -> ListResource -> HH.HTML i p
resource currentUser navigate lists r@{url, list, completed_at, tags} =
  HH.div
    [ HP.classes
        [ T.roundedMd
        , T.bgWhite
        , T.border2
        , T.borderKiwi
        , T.p2
        , T.flex
        , T.flexCol
        , T.justifyBetween
        ]
    ]
    [ HH.a
        [ HP.classes [ T.flex, T.itemsCenter ]
        , HP.href url
        , HP.target "_blank"
        , HP.rel "noreferrer noopener nofollow"
        ]
        [ HH.img [ HP.classes [ T.w4, T.h4, T.mr2 ], HP.src $ "https://s2.googleusercontent.com/s2/favicons?domain_url=" <> url ]
        , HH.div
            [ HP.classes
                [ T.textGray400
                , T.hoverTextKiwi
                , T.hoverUnderline
                , T.textSm
                , T.fontMedium
                , T.truncate
                ]
            ]
            [ HH.text $ titleOrUrl r ]
        ]
    , shortUrl url
    , HH.div
        [ HP.classes [ T.flex, T.justifyBetween, T.itemsCenter, T.mt2 ] ]
        [ HH.div
            [ HP.classes [ T.flex ] ]
            [ HH.div
                [ HP.classes
                    [ T.textSm
                    , T.fontBold
                    , T.mr2
                    , T.w4
                    , T.textCenter
                    ]
                ]
                [ if isJust completed_at
                    then Icons.check [ Icons.classes [ T.textKiwi, T.h5, T.w5 ] ]
                    else HH.text ""
                ]
            , maybeElem (NEA.fromArray tags) \ts ->
                HH.div
                  [ HP.classes [ T.flex ] ]
                  $ NEA.toArray
                  $ map Tag.tag ts
            ]
        , maybeElem (find ((list == _) <<< _.id) lists) \l ->
            HH.div
              [ HP.classes [ T.textXs, T.mr2 ] ]
              [ case currentUser, navigate of
                  Just u, Just nav ->
                    HH.a
                      [ HP.classes [ T.textGray300, T.fontMedium ]
                      , safeHref $ ViewList l.slug
                      , HE.onClick $ nav (PublicList u.slug l.slug) <<< Mouse.toEvent
                      ]
                      [ HH.text l.title ]
                  _, _ ->
                    HH.span
                      [ HP.classes [ T.textGray300, T.fontMedium ] ]
                      [ HH.text l.title ]
              ]
        ]
    ]

shortUrl :: forall i p. String -> HH.HTML i p
shortUrl u =
  maybeElem (takeDomain u) \short ->
    HH.div
      [ HP.classes [ T.textGray300, T.textXs, T.mt2, T.ml6 ] ]
      [ HH.text short ]
