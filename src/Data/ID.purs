-- | IDs are unique identifier for users, lists & resources in Listasio.
module Listasio.Data.ID
  ( ID -- constructor not exported
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

newtype ID = ID String

derive newtype instance showID :: Show ID
derive instance eqID :: Eq ID
derive instance ordID :: Ord ID
derive instance newtypeID :: Newtype ID _

codec :: JsonCodec ID
codec = wrapIso ID CA.string

-- | Enforce an ID is non-empty.
parse :: String -> Maybe ID
parse = case _ of
  "" -> Nothing
  str -> Just (ID str)

toString :: ID -> String
toString (ID str) = str
