module Main where

import Prelude

import ConfigProvider as ConfigProvider
import Data.Argonaut.Encode (encodeJson)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Exception (throw)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Aff.Util as HU
import Halogen.VDom.Driver (runUI)
import Listasio.Api.Request (decodeToken, readToken)
import Listasio.AppM (runAppM)
import Listasio.Component.Router as Router
import Listasio.Data.ID as ID
import Listasio.Data.Route (Route, routeCodec)
import Listasio.Foreign.ServiceWorker as ServiceWorker
import Listasio.Foreign.Splitbee as Splitbee
import Network.RemoteData (RemoteData(..))
import Routing.Duplex (parse)
import Routing.PushState (makeInterface, matchesWith)
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main = HA.runHalogenAff do
  _ <- ServiceWorker.register

  liftEffect $ Splitbee.init { scriptUrl: ConfigProvider.splitbeeUrl }

  _ <- HA.awaitBody

  mbToken <- liftEffect readToken

  currentUser <- case mbToken of
    Nothing -> pure Nothing
    Just token -> do
      let decodedUser = hush $ decodeToken token

      for_ decodedUser \{ email, id } ->
        liftEffect $ Splitbee.userSet $ encodeJson { email: unwrap email, userId: ID.toString id }

      pure decodedUser

  nav <- liftEffect makeInterface

  let
    baseUrl = ConfigProvider.provide
    env = ConfigProvider.env
    store = { nav, baseUrl, env, currentUser, lists: NotAsked }

  rootComponent <- (runAppM store) Router.component

  mbEl <- HU.selectElement $ QuerySelector ".app"

  case mbEl of
    Just el -> do
      halogenIO <- runUI rootComponent unit el

      let
        onRouteChange :: Maybe Route -> Route -> Effect Unit
        onRouteChange old new =
          when (old /= Just new) do
            launchAff_ $ halogenIO.query $ H.mkTell $ Router.Navigate new

      void $ liftEffect $ matchesWith (parse routeCodec) onRouteChange nav

    Nothing -> liftEffect $ throw "Could not mount app"
