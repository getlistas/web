module Listasio.Data.Profile where

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe)
import Listasio.Data.Avatar (Avatar)
import Listasio.Data.Avatar as Avatar
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

type PublicProfile
  = { | ProfileRep ( id :: ID, avatar :: Maybe Avatar ) }

type ProfileWithEmail
  = { | ProfileRep ( email :: Email ) }

type ProfileWithIdAndEmail
  = {
    | ProfileRep
      ( email :: Email
      , id :: ID
      , avatar :: Maybe Avatar
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
    , slug: Slug.codec
    }

publicProfileCodec :: JsonCodec PublicProfile
publicProfileCodec =
  CAR.object "PublicProfile"
    { name: Username.codec
    , slug: Slug.codec
    , id: ID.codec
    , avatar: CAC.maybe Avatar.codec
    }

profileWithIdAndEmailCodec :: JsonCodec ProfileWithIdAndEmail
profileWithIdAndEmailCodec =
  CAR.object "ProfileWithIdAndEmail"
    { email: Email.codec
    , name: Username.codec
    , id: ID.codec
    , slug: Slug.codec
    , avatar: CAC.maybe Avatar.codec
    }

profileWithEmailCodec :: JsonCodec ProfileWithEmail
profileWithEmailCodec =
  CAR.object "ProfileWithEmail"
    { email: Email.codec
    , name: Username.codec
    , slug: Slug.codec
    }

profileWithEmailPasswordCodec :: JsonCodec ProfileWithEmailPassword
profileWithEmailPasswordCodec =
  CAR.object "ProfileWithEmailPassword"
    { email: Email.codec
    , password: CAC.maybe CA.string
    , name: Username.codec
    , slug: Slug.codec
    }
