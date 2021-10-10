module Listasio.AppM where

import Prelude

import ConfigProvider as ConfigProvider
import Data.Argonaut.Encode (encodeJson)
import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now as Now
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, getStore, runStoreT, updateStore)
import Listasio.Api.Endpoint (Endpoint(..), SortingResources(..))
import Listasio.Api.Request (RequestMethod(..))
import Listasio.Api.Request as Request
import Listasio.Api.Utils (authenticate, mkAuthRequest, mkRequest)
import Listasio.Capability.Analytics (class Analytics)
import Listasio.Capability.Clipboard (class Clipboard)
import Listasio.Capability.LogMessages (class LogMessages, logError)
import Listasio.Capability.Navigate (class Navigate, locationState, navigate)
import Listasio.Capability.Now (class Now)
import Listasio.Capability.Resource.Integration (class ManageIntegration)
import Listasio.Capability.Resource.List (class ManageList)
import Listasio.Capability.Resource.Resource (class ManageResource)
import Listasio.Capability.Resource.User (class ManageUser)
import Listasio.Data.Integration as Integration
import Listasio.Data.List as List
import Listasio.Data.Log as Log
import Listasio.Data.Metrics as Metrics
import Listasio.Data.Profile as Profile
import Listasio.Data.Resource as Resource
import Listasio.Data.ResourceMetadata as ResourceMeta
import Listasio.Data.Route as Route
import Listasio.Foreign.Clipboard as ForeignClipboard
import Listasio.Foreign.Splitbee as Splitbee
import Listasio.Store as Store
import Routing.Duplex (print)
import Safe.Coerce (coerce)
import Web.Event.Event (preventDefault)

newtype AppM a
  = AppM (StoreT Store.Action Store.Store Aff a)

runAppM :: forall q i o. Store.Store -> H.Component q i o AppM -> Aff (H.Component q i o Aff)
runAppM store = runStoreT store Store.reduce <<< coerce

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadStoreAppM :: MonadStore Store.Action Store.Store AppM

instance cliboardAppM :: Clipboard AppM where
  writeText = liftAff <<< ForeignClipboard.writeText

instance nowAppM :: Now AppM where
  now = liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime

instance logMessagesAppM :: LogMessages AppM where
  logMessage log = do
    {env} <- getStore
    liftEffect case env, Log.reason log of
      Store.Prod, Log.Debug -> pure unit
      _, _ -> Console.log $ Log.message log

instance navigateAppM :: Navigate AppM where
  locationState = liftEffect =<< _.nav.locationState <$> getStore

  navigate route = do
    {nav} <- getStore
    {state} <- locationState
    liftEffect $ nav.pushState state $ print Route.routeCodec $ route

  navigate_ event route = do
    liftEffect $ preventDefault event
    navigate route

  logout = do
    liftEffect $ Request.removeToken
    updateStore Store.LogoutUser
    navigate Route.Home

instance analyticsAppM :: Analytics AppM where
  init = liftEffect $ Splitbee.init {scriptUrl: ConfigProvider.splitbeeUrl}

  userSet = liftEffect <<< Splitbee.userSet <<< encodeJson

  track event = case _ of
    Nothing -> liftEffect $ Splitbee.track event
    Just data_ -> liftEffect $ Splitbee.trackWithData event $ encodeJson data_

instance manageUserAppM :: ManageUser AppM where
  loginUser = authenticate Request.login

  googleLoginUser = authenticate Request.googleLogin unit

  registerUser fields = do
    {baseUrl} <- getStore
    res <- Request.register baseUrl fields
    case res of
      Left err -> logError err *> pure Nothing
      Right profile -> pure $ Just profile

  getCurrentUser =
    hush <$> mkAuthRequest {endpoint: Me, method: Get} Profile.profileWithIdAndEmailCodec

  updateUser fields =
    void $ mkAuthRequest
      { endpoint: User
      , method: Put $ Just $ Codec.encode Profile.profileCodec fields
      }
      Codec.json

  userBySlug slug =
    hush <$> mkRequest conf Profile.publicProfileCodec
    where conf = {endpoint: UserBySlug slug, method: Get}

  userMetrics slug =
    hush <$> mkRequest conf (CAC.array Metrics.metricCodec)
    where conf = {endpoint: UserMetrics slug, method: Get}

instance manageListAppM :: ManageList AppM where
  createList list =
    hush <$> mkAuthRequest conf List.listWitIdUserAndMetaCodec
    where method = Post $ Just $ Codec.encode List.createListFieldsCodec list
          conf = {endpoint: Lists, method}

  getList id =
    hush <$> mkAuthRequest conf List.listWitIdUserAndMetaCodec
    where conf = {endpoint: List id, method: Get}

  getListBySlug {list, user} =
    hush <$> mkAuthRequest conf List.listWitIdUserAndMetaCodec
    where conf = {endpoint: ListBySlug user list, method: Get}

  getPublicListBySlug {list, user} =
    hush <$> mkAuthRequest conf List.listWitIdUserAndMetaCodec
    where conf = {endpoint: ListBySlug user list, method: Get}

  getLists =
    hush <$> mkAuthRequest conf (CAC.array List.listWitIdUserAndMetaCodec)
    where conf = {endpoint: Lists, method: Get}

  updateList id list =
    hush <$> mkAuthRequest conf List.listWitIdUserAndMetaCodec
    where method = Put $ Just $ Codec.encode List.createListFieldsCodec list
          conf = {endpoint: List id, method}

  deleteList id =
    hush <$> map (const unit) <$> mkAuthRequest {endpoint: List id, method: Delete} Codec.json

  forkList id =
    hush <$> mkAuthRequest conf List.listWitIdAndUserCodec
    where conf = {endpoint: ListFork id, method: Post Nothing}

  discoverLists pagination = do
    res <- hush <$> mkRequest conf (CAC.array List.publicListCodec)
    mbId <- map _.id <$> _.currentUser <$> getStore
    pure $ map (List.publicListUserToAuthor mbId) <$> res
    where conf = {endpoint: Discover pagination, method: Get}

instance manageResourceAppM :: ManageResource AppM where
  getMeta url = do
    hush <$> mkAuthRequest conf codec
    where method = Post $ Just $ Codec.encode (CAR.object "Url" {url: Codec.string}) {url}
          conf = {endpoint: ResourceMeta, method}
          codec = ResourceMeta.metaCodec

  getResources = do
    hush <$> mkAuthRequest conf codec
    where conf = {endpoint: Resources, method: Get}
          codec = CAC.array Resource.listResourceCodec

  getListResources {list, completed} = do
    hush <$> mkAuthRequest conf codec
    where conf = {endpoint, method: Get}
          endpoint = ResourcesByList {list, sort: PositionAsc, completed}
          codec = CAC.array Resource.listResourceCodec

  getPublicListResources {user, list} = do
    hush <$> mkAuthRequest conf codec
    where conf = {endpoint, method: Get}
          endpoint = ListResourcesBySlug user list
          codec = CAC.array Resource.listResourceCodec

  searchResources args = do
    hush <$> mkAuthRequest conf codec
    where conf = {endpoint: SearchResources args, method: Get}
          codec = CAC.array Resource.listResourceCodec

  createResource newResource =
    hush <$> mkAuthRequest conf codec
    where method = Post $ Just $ Codec.encode Resource.resourceCodec newResource
          conf = {endpoint: Resources, method}
          codec = Resource.listResourceCodec

  updateResource id newResource =
    hush <$> mkAuthRequest conf codec
    where method = Put $ Just $ Codec.encode Resource.resourceCodec newResource
          conf = {endpoint: Resource id, method}
          codec = Resource.listResourceCodec

  completeResource {id} =
    map (const unit) <$> hush <$> mkAuthRequest conf Codec.json
    where conf = {endpoint: CompleteResource id, method: Post Nothing}

  uncompleteResource {id} =
    map (const unit) <$> hush <$> mkAuthRequest conf Codec.json
    where conf = {endpoint: UncompleteResource id, method: Post Nothing}

  deleteResource {id} =
    map (const unit) <$> hush <$> mkAuthRequest conf Codec.json
    where conf = {endpoint: Resource id, method: Delete}

  changePosition {id, list} {previus} =
    map (const unit) <$> hush <$> mkAuthRequest conf Codec.json
    where body = Codec.encode Resource.positionChangeBodyCodec {list, previus}
          conf = {endpoint: PositionResource id, method: Put $ Just body}

instance manageIntegrationAppM :: ManageIntegration AppM where
  createRssIntegration fields = do
    hush <$> mkAuthRequest conf codec
    where body = Codec.encode Integration.rssIntegrationFieldsCodec fields
          conf = {endpoint: RssIntegrations, method: Post $ Just body}
          codec = Integration.rssIntegrationCodec

  subscribeToList fields = do
    hush <$> mkAuthRequest conf codec
    where body = Codec.encode Integration.listSubscriptionFieldsCodec fields
          conf = {endpoint: ListSubscriptionIntegrations, method: Post $ Just body}
          codec = Integration.listSubscriptionCodec

  deleteIntegration id =
    map (const unit) <$> hush <$> mkAuthRequest conf Codec.json
    where conf = {endpoint: Integration id, method: Delete}

  getListIntegrations list =
    hush <$> mkAuthRequest conf codec
    where conf = {endpoint: Integrations {list, kind: Nothing}, method: Get}
          codec = CAC.array $ Integration.integrationCodec
