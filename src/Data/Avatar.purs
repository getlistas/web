-- | It's useful to be able to tell at a glance that a
-- | value isn't just a string -- it's an avatar
module Doneq.Data.Avatar
  ( Avatar -- constructor not exported
  , parse
  , toString
  , toStringWithDefault
  , codec
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Maybe (Maybe(..))

newtype Avatar = Avatar String

derive instance eqAvatar :: Eq Avatar

codec :: JsonCodec Avatar
codec = CA.prismaticCodec parse toString CA.string

-- | TODO: validate for URL
parse :: String -> Maybe Avatar
parse "" = Nothing
parse str = Just (Avatar str)

toString :: Avatar -> String
toString (Avatar str) = str

-- | Avatars are optional, but we don't want to display broken images on our site.
-- | This function provides a fallback avatar for when a user doesn't have one.
-- |
-- | TODO: default avatar URL
toStringWithDefault :: Maybe Avatar -> String
toStringWithDefault (Just av) = toString av
toStringWithDefault Nothing =
  "https://static.productionready.io/images/smiley-cyrus.jpg"
