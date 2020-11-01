module Doneq.AppM where

import Prelude

import Doneq.Api.Endpoint (Endpoint(..), noArticleParams)
import Doneq.Api.Request (RequestMethod(..))
import Doneq.Api.Request as Request
import Doneq.Api.Utils (authenticate, decode, decodeWithUser, mkAuthRequest, mkRequest)
import Doneq.Capability.LogMessages (class LogMessages)
import Doneq.Capability.Navigate (class Navigate, locationState, navigate)
import Doneq.Capability.Now (class Now)
import Doneq.Capability.Resource.Article (class ManageArticle)
import Doneq.Capability.Resource.Comment (class ManageComment)
import Doneq.Capability.Resource.Tag (class ManageTag)
import Doneq.Capability.Resource.User (class ManageUser)
import Doneq.Data.Article as Article
import Doneq.Data.Comment as Comment
import Doneq.Data.Log as Log
import Doneq.Data.Profile as Profile
import Doneq.Data.Route as Route
import Doneq.Env (Env, LogLevel(..))
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now as Now
import Effect.Ref as Ref
import Routing.Duplex (print)
import Type.Equality (class TypeEquals, from)

newtype AppM a = AppM (ReaderT Env Aff a)

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

instance nowAppM :: Now AppM where
  now = liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime

-- TODO: log to a service on production
instance logMessagesAppM :: LogMessages AppM where
  logMessage log = do
    env <- ask
    liftEffect case env.logLevel, Log.reason log of
      Prod, Log.Debug -> pure unit
      _, _ -> Console.log $ Log.message log

instance navigateAppM :: Navigate AppM where
  locationState = liftEffect =<< _.nav.locationState <$> ask

  navigate route = do
    { pushState } <- asks _.nav
    { state } <- locationState
    liftEffect $ pushState state $ print Route.routeCodec $ route

  logout = do
    { currentUser, userBus } <- asks _.userEnv
    liftEffect do
      Ref.write Nothing currentUser
      Request.removeToken
    liftAff do
      Bus.write Nothing userBus
    navigate Route.Home

instance manageUserAppM :: ManageUser AppM where
  loginUser =
    authenticate Request.login

  registerUser =
    authenticate Request.register

  getCurrentUser = do
    mbJson <- mkAuthRequest { endpoint: User, method: Get }
    map (map _.user)
      $ decode (CAR.object "User" { user: Profile.profileWithEmailCodec }) mbJson

  getAuthor username = do
    mbJson <- mkRequest { endpoint: Profiles username, method: Get }
    map (map _.profile)
      $ decodeWithUser (\u -> CAR.object "Profile" { profile: Profile.authorCodec u }) mbJson

  updateUser fields =
    void $ mkAuthRequest
      { endpoint: User
      , method: Put (Just (Codec.encode Profile.profileWithEmailPasswordCodec fields))
      }

  followUser username = do
    mbJson <- mkAuthRequest { endpoint: Follow username, method: Post Nothing }
    map (map _.profile)
      $ decodeWithUser (\u -> CAR.object "Profile" { profile: Profile.authorCodec u }) mbJson

  unfollowUser username = do
    mbJson <- mkAuthRequest { endpoint: Follow username, method: Delete }
    map (map _.profile)
      $ decodeWithUser (\u -> CAR.object "Profile" { profile: Profile.authorCodec u }) mbJson

instance manageTagAppM :: ManageTag AppM where
  getAllTags = do
    mbJson <- mkRequest { endpoint: Tags, method: Get }
    map (map _.tags) $ decode (CAR.object "Tags" { tags: CA.array CA.string }) mbJson

instance manageCommentAppM :: ManageComment AppM where
  getComments slug = do
    mbJson <- mkRequest { endpoint: Comments slug, method: Get }
    map (map _.comments)
      $ decodeWithUser (\u -> CAR.object "Comments" { comments: CA.array (Comment.codec u) }) mbJson

  createComment slug body =
    let method = Post $ Just $ Codec.encode (CAR.object "CommentBody" { body: CA.string }) { body }
     in void $ mkAuthRequest { endpoint: Comments slug, method }

  deleteComment slug id =
    void $ mkAuthRequest { endpoint: Comment slug id, method: Delete }

instance manageArticleAppM :: ManageArticle AppM where
  getArticle slug = do
    mbJson <- mkRequest { endpoint: Article slug, method: Get }
    map (map _.article)
      $ decodeWithUser (\u -> CAR.object "Article" { article: Article.articleWithMetadataCodec u }) mbJson

  getArticles fields =
    mkRequest { endpoint: Articles fields, method: Get }
      >>= decodeWithUser Article.articlesWithMetadataCodec

  createArticle article = do
    let
      codec = CAR.object "Article" { article: Article.articleCodec }
      method = Post $ Just $ Codec.encode codec { article }

    mbJson <- mkAuthRequest { endpoint: Articles noArticleParams, method }
    map (map _.article)
      $ decodeWithUser (\u -> CAR.object "Article" { article: Article.articleWithMetadataCodec u }) mbJson

  updateArticle slug article = do
    let
      codec = CAR.object "Article" { article: Article.articleCodec }
      method = Put $ Just $ Codec.encode codec { article }

    mbJson <- mkAuthRequest { endpoint: Article slug, method }
    map (map _.article) $ decodeWithUser (\u -> CAR.object "Article" { article: Article.articleWithMetadataCodec u }) mbJson

  deleteArticle slug =
    void $ mkAuthRequest { endpoint: Article slug, method: Delete }

  favoriteArticle slug = do
    mbJson <- mkAuthRequest { endpoint: Favorite slug, method: Post Nothing }
    map (map _.article) $ decodeWithUser (\u -> CAR.object "Article" { article: Article.articleWithMetadataCodec u }) mbJson

  unfavoriteArticle slug = do
    mbJson <- mkAuthRequest { endpoint: Favorite slug, method: Delete }
    map (map _.article) $ decodeWithUser (\u -> CAR.object "Article" { article: Article.articleWithMetadataCodec u }) mbJson

  getCurrentUserFeed params =
    mkAuthRequest { endpoint: Feed params, method: Get }
      >>= decodeWithUser Article.articlesWithMetadataCodec
