module Listasio.Data.Resource where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.DateTime (DateTime)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replace)
import Data.String.CodePoints (take)
import Data.String.Utils (startsWith)
import Listasio.Data.DateTime as DateTime
import Listasio.Data.ID (ID)
import Listasio.Data.ID as ID

type ResourceRep row
  =
  ( url :: String
  , title :: Maybe String
  , list :: ID
  , description :: Maybe String
  , thumbnail :: Maybe String
  , tags :: Array String
  | row
  )

type Resource
  = { | ResourceRep () }

type ListResource
  =
  {
  | ResourceRep
      ( id :: ID
      , user :: String
      , created_at :: DateTime
      , updated_at :: DateTime
      , completed_at :: Maybe DateTime
      , position :: Int
      )
  }

resourceCodec :: JsonCodec Resource
resourceCodec =
  CAR.object "Resource"
    { url: CA.string
    , title: CAC.maybe CA.string
    , description: CAC.maybe CA.string
    , thumbnail: CAC.maybe CA.string
    , list: ID.codec
    , tags: CAC.array CA.string
    }

listResourceCodec :: JsonCodec ListResource
listResourceCodec =
  CAR.object "Resource"
    { id: ID.codec
    , list: ID.codec
    , user: CA.string
    , url: CA.string
    , title: CAC.maybe CA.string
    , description: CAC.maybe CA.string
    , thumbnail: CAC.maybe CA.string
    , position: CA.int
    , created_at: DateTime.codec
    , updated_at: DateTime.codec
    , completed_at: CAC.maybe DateTime.codec
    , tags: CAC.array CA.string
    }

type PositionChangeBody
  =
  { list :: ID
  , previus :: Maybe ID
  }

positionChangeBodyCodec :: JsonCodec PositionChangeBody
positionChangeBodyCodec =
  CAR.object "PositionChangeBody"
    { list: ID.codec
    , previus: CAC.maybe ID.codec
    }

data FilterByDone
  = ShowAll
  | ShowDone
  | ShowPending

derive instance eqFilterByDone :: Eq FilterByDone

toggleFilterByDone :: FilterByDone -> FilterByDone
toggleFilterByDone ShowDone = ShowAll
toggleFilterByDone _ = ShowDone

toggleFilterByPending :: FilterByDone -> FilterByDone
toggleFilterByPending ShowPending = ShowAll
toggleFilterByPending _ = ShowPending

titleOrUrl :: forall r. { | ResourceRep r } -> String
titleOrUrl { title: Just title } = title
titleOrUrl { url }
  | startsWith "https://" url = take 50 $ replace (Pattern "https://") (Replacement "") url
  | startsWith "http://" url = take 50 $ replace (Pattern "http://") (Replacement "") url
  | otherwise = url

type ImportResourcesBody
  =
  { list :: ID
  , payload :: String
  }

importResourcesCodec :: JsonCodec ImportResourcesBody
importResourcesCodec =
  CAR.object "ImportResourcesBody"
    { list: ID.codec
    , payload: CA.string
    }
