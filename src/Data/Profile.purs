module Doneq.Data.Profile where

import Prelude
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe)
import Doneq.Data.Email (Email)
import Doneq.Data.Email as Email
import Doneq.Data.Username (Username)
import Doneq.Data.Username as Username

data Relation
  = Following
  | NotFollowing
  | You

derive instance eqRelation :: Eq Relation

type ProfileRep row
  = ( name :: Username -- TODO: use NonEmptyString
    , slug :: Username
    | row
    )

type Profile
  = { | ProfileRep () }

type ProfileWithEmail
  = { | ProfileRep ( email :: Email ) }

type ProfileWithEmailPassword
  = {
    | ProfileRep
      ( email :: Email
      , password :: Maybe String
      )
    }

profileCodec :: JsonCodec Profile
profileCodec =
  CAR.object "Profile"
    { name: Username.codec
    , slug: Username.codec
    }

profileWithEmailCodec :: JsonCodec ProfileWithEmail
profileWithEmailCodec =
  CAR.object "Profile"
    { email: Email.codec
    , name: Username.codec
    , slug: Username.codec
    }

profileWithEmailPasswordCodec :: JsonCodec ProfileWithEmailPassword
profileWithEmailPasswordCodec =
  CAR.object "Profile"
    { email: Email.codec
    , password: CAC.maybe CA.string
    , name: Username.codec
    , slug: Username.codec
    }
