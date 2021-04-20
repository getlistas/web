module Listasio.Data.List where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.DateTime (DateTime)
import Data.Maybe (Maybe(..))
import Listasio.Data.DateTime as DateTime
import Listasio.Data.ID (ID)
import Listasio.Data.ID as ID
import Listasio.Data.Profile (PublicProfile, publicProfileCodec)
import Listasio.Data.Resource (ListResource, listResourceCodec)
import Slug (Slug)
import Slug as Slug

data Author
  = You
  | Other PublicProfile

derive instance authorEq :: Eq Author
instance authorShow :: Show Author where
  show You = "You"
  show (Other user) = "Other (" <> ID.toString user.id <> ")"

type ForkedList
  = { id :: ID
    , slug :: Slug
    , user :: ID
    , title :: String
    , description :: Maybe String
    , tags :: Array String
    , created_at :: DateTime
    }

type ForkMeta
  = { list :: Maybe ForkedList
    , user :: Maybe PublicProfile
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
    , slug :: Slug
    , user :: ID
    , created_at :: DateTime
    , updated_at :: DateTime
    | row
    )

-- TODO: doesn't have user now
type ListWithIdAndUser
  = {
    | ListWithIdAndUserRep
      ( fork :: Maybe ForkMeta
      )
    }

-- TODO: doesn't have user now
type ListWithIdUserAndMeta
  = {
    | ListWithIdAndUserRep
      ( fork :: Maybe ForkMeta
      , resource_metadata :: ResourceMeta
      )
    }

type PublicListRep row
  = ( id :: ID
    , slug :: Slug
    , title :: String
    , description :: Maybe String
    , tags :: Array String
    , created_at :: DateTime
    | row
    )

type PublicListWithUser
  = { | PublicListRep (user :: PublicProfile) }

type PublicList
  = { | PublicListRep (author :: Author) }

publicListUserToAuthor :: Maybe ID -> PublicListWithUser -> PublicList
publicListUserToAuthor Nothing {id, slug, title, description, tags, created_at, user} =
  {id, slug, title, description, tags, created_at, author: Other user}
publicListUserToAuthor (Just currentUser) {id, slug, title, description, tags, created_at, user}
  | currentUser == user.id = {id, slug, title, description, tags, created_at, author: You}
  | otherwise = {id, slug, title, description, tags, created_at, author: Other user}

publicListCodec :: JsonCodec PublicListWithUser
publicListCodec =
  CAR.object "PublicListWithUser"
    { id: ID.codec
    , slug: Slug.codec
    , title: CA.string
    , description: CAC.maybe CA.string
    , tags: CAC.array CA.string
    , created_at: DateTime.codec
    , user: publicProfileCodec
    }

resourceMetaCodec :: JsonCodec ResourceMeta
resourceMetaCodec =
  CAR.object "ResourceMeta"
    { count: CA.int
    , completed_count: CA.int
    , last_completed_at: CAC.maybe DateTime.codec
    , next: CAC.maybe listResourceCodec
    }

forkedListCodec :: JsonCodec ForkedList
forkedListCodec =
  CAR.object "ForkedList"
    { id: ID.codec
    , slug: Slug.codec
    , title: CA.string
    , description: CAC.maybe CA.string
    , tags: CAC.array CA.string
    , created_at: DateTime.codec
    , user: ID.codec
    }

forkMetaCodec :: JsonCodec ForkMeta
forkMetaCodec =
  CAR.object "ForkMeta"
    { list: CAC.maybe forkedListCodec
    , user: CAC.maybe publicProfileCodec
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

listWitIdAndUserCodec :: JsonCodec ListWithIdAndUser
listWitIdAndUserCodec =
  CAR.object "ListWithIdAndUser"
    { id: ID.codec
    , slug: Slug.codec
    , title: CA.string
    , description: CAC.maybe CA.string
    , tags: CAC.array CA.string
    , user: ID.codec
    , is_public: CA.boolean
    , created_at: DateTime.codec
    , updated_at: DateTime.codec
    , fork: CAC.maybe forkMetaCodec
    }

listWitIdUserAndMetaCodec :: JsonCodec ListWithIdUserAndMeta
listWitIdUserAndMetaCodec =
  CAR.object "ListWithIdUserAndMeta"
    { id: ID.codec
    , slug: Slug.codec
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
