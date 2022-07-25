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
  | KindListSubscription

integrationKindCodec :: JsonCodec IntegrationKind
integrationKindCodec =
  CA.prismaticCodec "IntegrationKind" parse toString CA.string
  where
  parse "rss" = Just KindRss
  parse "listas-subscription" = Just KindListSubscription
  parse _ = Nothing

  toString KindRss = "rss"
  toString KindListSubscription = "listas-subscription"

type RssIntegrationFields
  =
  { url :: String
  , list :: ID
  }

rssIntegrationFieldsCodec :: JsonCodec RssIntegrationFields
rssIntegrationFieldsCodec =
  CAR.object "RssIntegrationFields"
    { url: CA.string
    , list: ID.codec
    }

type ListSubscriptionFields
  =
  { subscribe_from :: ID
  , subscribe_to :: ID
  }

listSubscriptionFieldsCodec :: JsonCodec ListSubscriptionFields
listSubscriptionFieldsCodec =
  CAR.object "ListSubscriptionFields"
    { subscribe_from: ID.codec
    , subscribe_to: ID.codec
    }

type IntegrationRep row
  =
  ( id :: ID
  , user :: ID
  , list :: ID
  , created_at :: DateTime
  , updated_at :: DateTime
  | row
  )

type RssBody
  =
  { url :: String
  , subscription_id :: String
  , feed_type :: String
  , metadata :: Maybe String
  }

type RssIntegration
  = { | IntegrationRep (rss :: RssBody) }

type ListSubscriptionBody
  = { list :: ID }

type ListSubscription
  = { | IntegrationRep (listas_subscription :: ListSubscriptionBody) }

data Integration
  = RssIntegration RssIntegration
  | ListSubscription ListSubscription

rssBody :: JsonCodec RssBody
rssBody =
  CAR.object "RssBody"
    { url: CA.string
    , subscription_id: CA.string
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

listSubscriptionBodyCodec :: JsonCodec ListSubscriptionBody
listSubscriptionBodyCodec =
  CAR.object "ListSubscriptionBody" { list: ID.codec }

listSubscriptionCodec :: JsonCodec ListSubscription
listSubscriptionCodec =
  CAR.object "ListSubscription"
    { id: ID.codec
    , user: ID.codec
    , list: ID.codec
    , created_at: DateTime.codec
    , updated_at: DateTime.codec
    , listas_subscription: listSubscriptionBodyCodec
    }

integrationCodec :: JsonCodec Integration
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
      , listas_subscription: CAC.maybe listSubscriptionBodyCodec
      }

  to
    :: { | IntegrationRep
           ( kind :: IntegrationKind
           , rss :: Maybe RssBody
           , listas_subscription :: Maybe ListSubscriptionBody
           )
       }
    -> Either JsonDecodeError Integration
  to { kind: KindRss, rss: Just rss, id, user, list, created_at, updated_at } =
    Right $ RssIntegration { id, user, list, created_at, updated_at, rss }
  to { kind: KindListSubscription, listas_subscription: Just listas_subscription, id, user, list, created_at, updated_at } =
    Right $ ListSubscription { id, user, list, created_at, updated_at, listas_subscription }
  to { kind: KindRss, rss: Nothing } = Left $ AtKey "rss" MissingValue
  to { kind: KindListSubscription, listas_subscription: Nothing } = Left $ AtKey "listas_subscription" MissingValue

  from
    :: Integration
    -> { | IntegrationRep
           ( kind :: IntegrationKind
           , rss :: Maybe RssBody
           , listas_subscription :: Maybe ListSubscriptionBody
           )
       }
  from (RssIntegration { id, user, list, created_at, updated_at, rss }) =
    { id, user, list, created_at, updated_at, kind: KindRss, rss: Just rss, listas_subscription: Nothing }
  from (ListSubscription { id, user, list, created_at, updated_at, listas_subscription }) =
    { id, user, list, created_at, updated_at, kind: KindListSubscription, rss: Nothing, listas_subscription: Just listas_subscription }
