module Main where

import Prelude

import Doneq.Api.Request (BaseURL(..), decodeToken, readToken)
import Doneq.AppM (runAppM)
import Doneq.Component.Router as Router
import Doneq.Data.Route (Route, routeCodec)
import Doneq.Env (Env, LogLevel(..))
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Bus as Bus
import Effect.Exception (throw)
import Effect.Ref as Ref
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Aff.Util as HU
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse)
import Routing.PushState (makeInterface, matchesWith)
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main = HA.runHalogenAff do

  _ <- HA.awaitBody

  let
    baseUrl = BaseURL "http://localhost:8080"
    logLevel = Dev

  userEnv <- liftEffect do
    currentUser <- Ref.new Nothing

    userBus <- Bus.make

    readToken >>= traverse_ \token -> do
      -- TODO: log the decoding error ?
      liftEffect (Ref.write (hush $ decodeToken token) currentUser)

    pure { currentUser, userBus }

  nav <- liftEffect makeInterface

  let
    environment :: Env
    environment = { nav, baseUrl, logLevel, userEnv }

    rootComponent :: H.Component HH.HTML Router.Query {} Void Aff
    rootComponent = H.hoist (runAppM environment) Router.component

  mbEl <- HU.selectElement $ QuerySelector ".app"

  case mbEl of
    Just el -> do
      halogenIO <- runUI rootComponent {} el

      let
        onRouteChange :: Maybe Route -> Route -> Effect Unit
        onRouteChange old new =
          when (old /= Just new) do
            launchAff_ $ halogenIO.query $ H.tell $ Router.Navigate new

      void $ liftEffect $ matchesWith (parse routeCodec) onRouteChange nav

    Nothing -> liftEffect $ throw "Could not mount app"
