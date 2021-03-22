-- | It's useful to be able to tell at a glance that a
-- | value isn't just a string -- it's an avatar
module Listasio.Data.Avatar
  ( Avatar -- constructor not exported
  , Size(..)
  , parse
  , toString
  , renderWithDefault
  , codec
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Maybe (Maybe(..))
import Halogen (ClassName)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Listasio.Component.HTML.Icons as Icons
import Tailwind as T

newtype Avatar = Avatar String

derive instance eqAvatar :: Eq Avatar

codec :: JsonCodec Avatar
codec = CA.prismaticCodec parse toString CA.string

parse :: String -> Maybe Avatar
parse = case _ of
  "" -> Nothing
  str -> Just (Avatar str)

toString :: Avatar -> String
toString (Avatar str) = str

data Size
  = Xs | Sm | Full

h :: Size -> ClassName
h Xs = T.h8
h Sm = T.h10
h Full = T.hFull

w :: Size -> ClassName
w Xs = T.w8
w Sm = T.w10
w Full = T.wFull

renderWithDefault :: forall i p. Size -> Maybe Avatar -> HH.HTML i p
renderWithDefault size = case _ of
  Just (Avatar avatar) ->
    HH.img [ HP.classes [ T.roundedFull, h size, w size ], HP.src avatar ]
  Nothing ->
    HH.div
      [ HP.classes
          [ h size
          , w size
          , T.roundedFull
          , T.bgGray100
          , T.flex
          , T.justifyCenter
          , T.itemsCenter
          ]
      ]
      [ Icons.userCircle [ Icons.classes [ T.textGray300, T.w5d6, T.h5d6 ] ] ]
