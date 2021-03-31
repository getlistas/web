module Slug
  ( Slug
  , codec
  , parse
  , term
  , toString
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Either (note)
import Data.Maybe (Maybe(..))
import Routing.Duplex (RouteDuplex', as)

newtype Slug = Slug String

derive instance eqSlug :: Eq Slug
derive instance ordSlug :: Ord Slug
derive newtype instance semigroupSlug :: Semigroup Slug

instance showSlug :: Show Slug where
  show (Slug str) = "(Slug " <> str <> ")"

instance encodeJsonSlug :: EncodeJson Slug where
  encodeJson (Slug s) = encodeJson s

instance decodeJsonSlug :: DecodeJson Slug where
  decodeJson = note (TypeMismatch "Slug") <<< parse <=< decodeJson

parse :: String -> Maybe Slug
parse "" = Nothing
parse str = Just $ Slug str

toString :: Slug -> String
toString (Slug s) = s


codec :: JsonCodec Slug
codec = CA.prismaticCodec parse toString CA.string

term :: RouteDuplex' String -> RouteDuplex' Slug
term = as toString (parse >>> note "Bad slug")
