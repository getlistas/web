module Listasio.Data.List where

import Prelude

import Data.Codec (mapCodec)
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.DateTime (DateTime)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Listasio.Data.DateTime as DateTime
import Listasio.Data.ID (ID)
import Listasio.Data.ID as ID
import Listasio.Data.Resource (ListResource, listResourceCodec)

data Author
  = You
  | Other ID

derive instance authorEq :: Eq Author
instance authorShow :: Show Author where
  show You = "You"
  show (Other id) = "Other " <> ID.toString id

type ForkMeta
  = { from :: String
    , at :: DateTime
    }

type ResourceMeta
  = { count :: Int
    , completed_count :: Int
    , last_completed_at :: Maybe DateTime
    , next :: Maybe ListResource
    }

type ListRep row
  = ( title :: String
    , description :: Maybe String
    , tags :: Array String
    , is_public :: Boolean
    | row
    )

type CreateListFields
  = { | ListRep () }

type List
  = { | ListRep ( fork :: Maybe ForkMeta ) }

type ListWithIdAndUserRep row
  = ( title :: String
    , description :: Maybe String
    , tags :: Array String
    , is_public :: Boolean
    , id :: ID
    , user :: ID
    , created_at :: DateTime
    , updated_at :: DateTime
    | row
    )

type ListWithIdAndUser
  = {
    | ListWithIdAndUserRep
      ( fork :: Maybe ForkMeta
      , author :: Author
      )
    }

type ListWithIdUserAndMeta
  = {
    | ListWithIdAndUserRep
      ( fork :: Maybe ForkMeta
      , author :: Author
      , resource_metadata :: ResourceMeta
      )
    }

resourceMetaCodec :: JsonCodec ResourceMeta
resourceMetaCodec =
  CAR.object "ResourceMeta"
    { count: CA.int
    , completed_count: CA.int
    , last_completed_at: CAC.maybe DateTime.codec
    , next: CAC.maybe listResourceCodec
    }

forkMetaCodec :: JsonCodec ForkMeta
forkMetaCodec =
  CAR.object "ForkMeta"
    { from: CA.string
    , at: DateTime.codec
    }

createListFieldsCodec :: JsonCodec CreateListFields
createListFieldsCodec =
  CAR.object "CreateListFieldsList"
    { title: CA.string
    , description: CAC.maybe CA.string
    , tags: CAC.array CA.string
    , is_public: CA.boolean
    }

listCodec :: JsonCodec List
listCodec =
  CAR.object "List"
    { title: CA.string
    , description: CAC.maybe CA.string
    , tags: CAC.array CA.string
    , is_public: CA.boolean
    , fork: CAC.maybe forkMetaCodec
    }

listWitIdAndUserCodec :: Maybe ID -> JsonCodec ListWithIdAndUser
listWitIdAndUserCodec mbUserId = mapCodec to from codec
  where
  codec =
    CAR.object "List"
      { id: ID.codec
      , title: CA.string
      , description: CAC.maybe CA.string
      , tags: CAC.array CA.string
      , user: ID.codec
      , is_public: CA.boolean
      , created_at: DateTime.codec
      , updated_at: DateTime.codec
      , fork: CAC.maybe forkMetaCodec
      }

  to :: { | ListWithIdAndUserRep ( fork :: Maybe ForkMeta ) } -> Either JsonDecodeError ListWithIdAndUser
  to {id, title, description, tags, user, is_public, created_at, updated_at, fork} = pure do
    let mkList = {id, title, description, tags, user, is_public, created_at, updated_at, fork, author: _ }

    mkList case mbUserId of
      Just userId | user == userId -> You
      _ -> Other id

  from :: ListWithIdAndUser -> { | ListWithIdAndUserRep ( fork :: Maybe ForkMeta ) }
  from {id, title, description, tags, user, is_public, created_at, updated_at, fork} = do
    {id, title, description, tags, user, is_public, created_at, updated_at, fork}

listWitIdUserAndMetaCodec :: Maybe ID -> JsonCodec ListWithIdUserAndMeta
listWitIdUserAndMetaCodec mbUserId = mapCodec to from codec
  where
  codec =
    CAR.object "List"
      { id: ID.codec
      , title: CA.string
      , description: CAC.maybe CA.string
      , tags: CAC.array CA.string
      , user: ID.codec
      , is_public: CA.boolean
      , created_at: DateTime.codec
      , updated_at: DateTime.codec
      , resource_metadata: resourceMetaCodec
      , fork: CAC.maybe forkMetaCodec
      }

  to :: { | ListWithIdAndUserRep ( fork :: Maybe ForkMeta, resource_metadata :: ResourceMeta ) } -> Either JsonDecodeError ListWithIdUserAndMeta
  to {id, title, description, tags, user, is_public, created_at, updated_at, fork, resource_metadata} = pure do
    let mkList = {id, title, description, tags, user, is_public, created_at, updated_at, fork, resource_metadata, author: _ }

    mkList case mbUserId of
      Just userId | user == userId -> You
      _ -> Other id

  from :: ListWithIdUserAndMeta -> { | ListWithIdAndUserRep ( fork :: Maybe ForkMeta, resource_metadata :: ResourceMeta ) }
  from {id, title, description, tags, user, is_public, created_at, updated_at, fork, resource_metadata} = do
    {id, title, description, tags, user, is_public, created_at, updated_at, fork, resource_metadata}
