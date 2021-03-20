module Listasio.Data.Integration where

import Prelude

import Data.Codec (mapCodec)
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError(..))
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Listasio.Data.DateTime as DateTime
import Listasio.Data.ID (ID)
import Listasio.Data.ID as ID

data IntegrationKind
  = KindRss

integrationKindCodec :: JsonCodec IntegrationKind
integrationKindCodec = CA.prismaticCodec parse toString CA.string
  where
  parse "rss" = Just KindRss
  parse _ = Nothing

  toString KindRss = "rss"

type RssIntegrationFields
  = { url :: String
    , list :: ID
    }

rssIntegrationFieldsCodec :: JsonCodec RssIntegrationFields
rssIntegrationFieldsCodec =
  CAR.object "RssIntegrationFields"
    { url: CA.string
    , list: ID.codec
    }

type IntegrationRep row
  = ( id :: ID
    , user :: ID
    , list :: ID
    , created_at :: DateTime
    , updated_at :: DateTime
    | row
    )

type RssBody
  = { url :: String
    , subscription_id :: String
    , status :: String
    , feed_type :: String
    , metadata :: Maybe String
    }

type RssIntegration
  = { | IntegrationRep ( rss :: RssBody ) }

data Integration
  = RssIntegration RssIntegration

rssBody :: JsonCodec RssBody
rssBody =
  CAR.object "RssBody"
    { url: CA.string
    , subscription_id: CA.string
    , status: CA.string
    , feed_type: CA.string
    , metadata: CAC.maybe CA.string
    }

rssIntegrationCodec :: JsonCodec RssIntegration
rssIntegrationCodec =
  CAR.object "RssIntegration"
    { id: ID.codec
    , user: ID.codec
    , list: ID.codec
    , created_at: DateTime.codec
    , updated_at: DateTime.codec
    , rss: rssBody
    }

integrationCodec  :: JsonCodec Integration
integrationCodec = mapCodec to from codec
  where
  codec =
    CAR.object "Integration_All"
      { id: ID.codec
      , user: ID.codec
      , list: ID.codec
      , created_at: DateTime.codec
      , updated_at: DateTime.codec
      , kind: integrationKindCodec
      , rss: CAC.maybe rssBody
      }

  to :: { | IntegrationRep ( kind :: IntegrationKind, rss :: Maybe RssBody ) } -> Either JsonDecodeError Integration
  to {kind: KindRss, rss: Just rss, id, user, list, created_at, updated_at} =
    Right $ RssIntegration {id, user, list, created_at, updated_at, rss}
  to {rss: Nothing} = Left $ AtKey "rss" MissingValue

  from :: Integration -> { | IntegrationRep ( kind :: IntegrationKind, rss :: Maybe RssBody ) }
  from (RssIntegration {id, user, list, created_at, updated_at, rss}) =
    {id, user, list, created_at, updated_at, kind: KindRss, rss: Just rss}
