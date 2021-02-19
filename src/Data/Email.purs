-- | It's useful to be able to tell at a glance that a
-- | string is not just a string -- it's an email address.
module Listasio.Data.Email where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)

newtype Email = Email String

derive newtype instance showEmail :: Show Email
derive instance newtypeEmail :: Newtype Email _
derive instance eqEmail :: Eq Email
derive instance ordEmail :: Ord Email

codec :: JsonCodec Email
codec = wrapIso Email CA.string
