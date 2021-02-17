module Listasio.Data.Profile where

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe)
import Listasio.Data.Email (Email)
import Listasio.Data.Email as Email
import Listasio.Data.ID (ID)
import Listasio.Data.ID as ID
import Listasio.Data.Username (Username)
import Listasio.Data.Username as Username
import Slug (Slug)
import Slug as Slug

type ProfileRep row
  = ( name :: Username
    , slug :: Slug
    | row
    )

type Profile
  = { | ProfileRep () }

type ProfileWithEmail
  = { | ProfileRep ( email :: Email ) }

type ProfileWithIdAndEmail
  = {
    | ProfileRep
      ( email :: Email
      , id :: ID
      )
    }

type ProfileWithEmailPassword
  = {
    | ProfileRep
      ( email :: Email
      , password :: Maybe String -- TODO newtype with validation
      )
    }

profileCodec :: JsonCodec Profile
profileCodec =
  CAR.object "Profile"
    { name: Username.codec
    , slug
    }

profileWithIdAndEmailCodec :: JsonCodec ProfileWithIdAndEmail
profileWithIdAndEmailCodec =
  CAR.object "Profile"
    { email: Email.codec
    , name: Username.codec
    , id: ID.codec
    , slug
    }

profileWithEmailCodec :: JsonCodec ProfileWithEmail
profileWithEmailCodec =
  CAR.object "Profile"
    { email: Email.codec
    , name: Username.codec
    , slug
    }

profileWithEmailPasswordCodec :: JsonCodec ProfileWithEmailPassword
profileWithEmailPasswordCodec =
  CAR.object "Profile"
    { email: Email.codec
    , password: CAC.maybe CA.string
    , name: Username.codec
    , slug
    }

slug :: JsonCodec Slug
slug = CA.prismaticCodec Slug.parse Slug.toString CA.string
