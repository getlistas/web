module Listasio.Component.HTML.Message where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen (ClassName)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Listasio.Component.HTML.Utils (maybeElem)
import Tailwind as T

data Style
  = Alert
  | Warning
  | Success

type Props
  =
  { style :: Style
  , title :: Maybe String
  , text :: Maybe String
  , icon :: Maybe String
  , classes :: Array ClassName
  }

props :: Props
props =
  { style: Success
  , title: Nothing
  , text: Nothing
  , icon: Nothing
  , classes: []
  }

message :: forall i p. Props -> HH.HTML i p
message { classes, style, title: mbTitle, text: mbText, icon: mbIcon } =
  HH.div
    [ HP.classes
        $ classes <>
            [ T.m1
            , T.py2
            , T.px6
            , T.roundedMd
            , case style of
                Alert -> T.bgManzana
                Success -> T.bgKiwi
                Warning -> T.bgDuraznoLight
            , case style of
                Warning -> T.textGray300
                _ -> T.textWhite
            , T.flex
            , T.itemsCenter
            -- , T.justifyBetween
            ]
    ]
    [ HH.div
        []
        [ maybeElem mbTitle \title ->
            HH.div [ HP.classes [ T.textXl, T.fontSemibold ] ] [ HH.text title ]
        , maybeElem mbText \text ->
            HH.div [ HP.classes [ T.textLg, T.fontNormal ] ] [ HH.text text ]
        ]
    , maybeElem mbIcon \icon ->
        HH.div
          [ HP.classes [ T.ml4, T.text5xl ] ]
          [ HH.text icon ]
    ]
