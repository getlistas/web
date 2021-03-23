module Listasio.AppM where

import Prelude

import ConfigProvider as ConfigProvider
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Argonaut.Encode (encodeJson)
import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now as Now
import Effect.Ref as Ref
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
import Listasio.Data.Profile as Profile
import Listasio.Data.Resource as Resource
import Listasio.Data.ResourceMetadata as ResourceMeta
import Listasio.Data.Route as Route
import Listasio.Env (Env, LogLevel(..))
import Listasio.Foreign.Clipboard as ForeignClipboard
import Listasio.Foreign.Splitbee as Splitbee
import Routing.Duplex (print)
import Type.Equality (class TypeEquals, from)
import Web.Event.Event (preventDefault)

newtype AppM a
  = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

instance cliboardAppM :: Clipboard AppM where
  writeText = liftAff <<< ForeignClipboard.writeText

instance nowAppM :: Now AppM where
  now = liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime

instance logMessagesAppM :: LogMessages AppM where
  logMessage log = do
    env <- ask
    liftEffect case env.logLevel, Log.reason log of
      Prod, Log.Debug -> pure unit
      _, _ -> Console.log $ Log.message log

instance navigateAppM :: Navigate AppM where
  locationState = liftEffect =<< _.nav.locationState <$> ask

  navigate route = do
    {pushState} <- asks _.nav
    {state} <- locationState
    liftEffect $ pushState state $ print Route.routeCodec $ route

  navigate_ event route = do
    liftEffect $ preventDefault event
    navigate route

  logout = do
    {currentUser, userBus} <- asks _.userEnv
    liftEffect do
      Ref.write Nothing currentUser
      Request.removeToken
    liftAff do
      Bus.write Nothing userBus
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
    {baseUrl} <- ask
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

instance manageListAppM :: ManageList AppM where
  createList list =
    hush <$> mkAuthRequest conf List.listWitIdAndUserCodec
    where method = Post $ Just $ Codec.encode List.createListFieldsCodec list
          conf = {endpoint: Lists, method}

  getList id =
    hush <$> mkAuthRequest conf List.listWitIdUserAndMetaCodec
    where conf = {endpoint: List id, method: Get}

  getListBySlug {list, user} =
    hush <$> mkAuthRequest conf List.listWitIdAndUserCodec
    where conf = {endpoint: ListBySlug user list, method: Get}

  getLists =
    hush <$> mkAuthRequest conf (CAC.array List.listWitIdUserAndMetaCodec)
    where conf = {endpoint: Lists, method: Get}

  updateList id list =
    hush <$> mkAuthRequest conf List.listWitIdAndUserCodec
    where method = Put $ Just $ Codec.encode List.createListFieldsCodec list
          conf = {endpoint: List id, method}

  deleteList id = void $ mkAuthRequest {endpoint: List id, method: Delete} Codec.json

  forkList id =
    hush <$> mkAuthRequest conf List.listWitIdAndUserCodec
    where conf = {endpoint: ListFork id, method: Post Nothing}

  discoverLists pagination = do
    {userEnv} <- ask
    mbId <- map _.id <$> (liftEffect $ Ref.read userEnv.currentUser)
    res <- hush <$> mkRequest conf (CAC.array List.publicListCodec)
    pure $ map (List.publicListUserToAuthor mbId) <$> res
    where conf = {endpoint: Discover pagination, method: Get}

instance manageResourceAppM :: ManageResource AppM where
  getMeta url = do
    hush <$> mkAuthRequest conf codec
    where method = Post $ Just $ Codec.encode (CAR.object "Url" {url: Codec.string}) {url}
          conf = {endpoint: ResourceMeta, method}
          codec = ResourceMeta.metaCodec

  getListResources list = do
    hush <$> mkAuthRequest conf codec
    where conf = {endpoint, method: Get}
          endpoint = ResourcesByList {list, sort: PositionAsc, completed: false}
          codec = CAC.array Resource.listResourceCodec

  getResources = do
    hush <$> mkAuthRequest conf codec
    where conf = {endpoint: Resources, method: Get}
          codec = CAC.array Resource.listResourceCodec

  createResource newResource =
    hush <$> mkAuthRequest conf codec
    where method = Post $ Just $ Codec.encode Resource.resourceCodec newResource
          conf = {endpoint: Resources, method}
          codec = Resource.listResourceCodec

  completeResource {id} =
    map (const unit) <$> hush <$> mkAuthRequest conf Codec.json
    where conf = {endpoint: CompleteResource id, method: Post Nothing}

  deleteResource {id} =
    map (const unit) <$> hush <$> mkAuthRequest conf Codec.json
    where conf = {endpoint: Resource id, method: Delete}

instance manageIntegrationAppM :: ManageIntegration AppM where
  createRssIntegration fields = do
    hush <$> mkAuthRequest conf codec
    where body = Codec.encode Integration.rssIntegrationFieldsCodec fields
          conf = {endpoint: RssIntegrations, method: Post $ Just body}
          codec = Integration.rssIntegrationCodec

  deleteIntegration id =
    map (const unit) <$> hush <$> mkAuthRequest conf Codec.json
    where conf = {endpoint: Integration id, method: Delete}

  getListIntegrations list =
    hush <$> mkAuthRequest conf codec
    where conf = {endpoint: Integrations {list, kind: Integration.KindRss}, method: Get}
          codec = CAC.array $ Integration.rssIntegrationCodec
