module Listasio.Component.HTML.Footer where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Tailwind as T

footer :: forall i p. HH.HTML i p
footer =
  HH.footer_
    [ HH.div
        [ HP.classes [ T.container ] ]
        [ HH.a
            [ HP.href "/" ]
            [ HH.text "conduit" ]
        , HH.span
            []
            [ HH.text "An interactive learning project from "
            , HH.a
                [ HP.href "https://thinkster.io" ]
                [ HH.text "Thinkster" ]
            , HH.text ". Code & design licensed under MIT. Implemented by "
            , HH.a
                [ HP.href "https://thomashoneyman.com" ]
                [ HH.text "Thomas Honeyman" ]
            , HH.text "."
            ]
        ]
    ]
