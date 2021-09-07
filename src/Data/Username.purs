-- | Usernames are the unique identifier for users in Listasio.
module Listasio.Data.Username
  ( Username(..)
  , parse
  , toString
  , codec
  ) where

import Prelude
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)

newtype Username
  = Username String

derive newtype instance eqShow :: Show Username
derive instance eqUsername :: Eq Username
derive instance ordUsername :: Ord Username
derive instance newtypeUsername :: Newtype Username _

codec :: JsonCodec Username
codec = wrapIso Username CA.string

-- | Enforce a username is non-empty.
parse :: String -> Maybe Username
parse = case _ of
  "" -> Nothing
  str -> Just (Username str)

toString :: Username -> String
toString (Username str) = str
