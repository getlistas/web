-- | The `Router` component is the root of our Halogen application. Every other component is a
-- | direct descendent of this component. We'll use the router to choose which component to render
-- | given a particular `Route` and to manage the user's location in the application.
module Listasio.Component.Router where

import Prelude

import Component.HOC.Connect (WithCurrentUser)
import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Either (hush)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Listasio.Capability.Clipboard (class Clipboard)
import Listasio.Capability.LogMessages (class LogMessages)
import Listasio.Capability.Navigate (class Navigate, navigate, locationState)
import Listasio.Capability.Now (class Now)
import Listasio.Capability.Resource.Integration (class ManageIntegration)
import Listasio.Capability.Resource.List (class ManageList)
import Listasio.Capability.Resource.Resource (class ManageResource)
import Listasio.Capability.Resource.User (class ManageUser)
import Listasio.Component.Utils (OpaqueSlot)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Route (Route(..), routeCodec)
import Listasio.Env (UserEnv)
import Listasio.Page.About as About
import Listasio.Page.Changelog as Changelog
import Listasio.Page.CreateList as CreateList
import Listasio.Page.CreateResource as CreateResource
import Listasio.Page.Dashboard as Dashboard
import Listasio.Page.Discover as Discover
import Listasio.Page.EditList as EditList
import Listasio.Page.Home as Home
import Listasio.Page.ListIntegrations as ListIntegrations
import Listasio.Page.Login as Login
import Listasio.Page.Policy as Policy
import Listasio.Page.Pricing as Pricing
import Listasio.Page.Profile as Profile
import Listasio.Page.Register as Register
import Listasio.Page.Resources as Resources
import Listasio.Page.Settings as Settings
import Listasio.Page.Terms as Terms
import Listasio.Page.VerifyFailure as VerifyFailure
import Listasio.Page.ViewList as ViewList
import Routing.Duplex as RD

type State =
  { route :: Maybe Route
  , currentUser :: Maybe ProfileWithIdAndEmail
  }

data Query a
  = Navigate Route a

data Action
  = Initialize
  | Receive { | WithCurrentUser () }

type ChildSlots =
  ( home :: OpaqueSlot Unit
  , about :: OpaqueSlot Unit
  , discover :: OpaqueSlot Unit
  , pricing :: OpaqueSlot Unit
  , profile :: OpaqueSlot Unit
  , changelog :: OpaqueSlot Unit
    -- Legal
  , terms :: OpaqueSlot Unit
  , policy :: OpaqueSlot Unit
    -- Auth
  , login :: OpaqueSlot Unit
  , register :: OpaqueSlot Unit
  , verifySuccess :: OpaqueSlot Unit
  , verifyFailure :: OpaqueSlot Unit
    -- Private
  , dashboard :: OpaqueSlot Unit
  , resources :: OpaqueSlot Unit
  , settings :: OpaqueSlot Unit
  , createList :: OpaqueSlot Unit
  , createResource :: OpaqueSlot Unit
  , viewList :: OpaqueSlot Unit
  , editList :: OpaqueSlot Unit
  , listintegrations :: OpaqueSlot Unit
  )

component
  :: forall m r
   . MonadAff m
  => MonadAsk { userEnv :: UserEnv | r } m
  => Now m
  => LogMessages m
  => Navigate m
  => ManageUser m
  => ManageResource m
  => ManageIntegration m
  => ManageList m
  => Clipboard m
  => H.Component HH.HTML Query {} Void m
component = Connect.component $ H.mkComponent
  { initialState: \{ currentUser } -> { route: Nothing, currentUser }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery
      , handleAction = handleAction
      , receive = Just <<< Receive
      , initialize = Just Initialize
      }
  }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      -- first we'll get the route the user landed on
      initialRoute <- hush <<< (RD.parse routeCodec) <$> _.path <$> locationState
      -- then we'll navigate to the new route (also setting the hash)
      navigate $ fromMaybe Home initialRoute

    Receive { currentUser } ->
      H.modify_ _ { currentUser = currentUser }

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      { route, currentUser } <- H.get
      when (route /= Just dest) do
        case isJust currentUser && dest `elem` [ Login, Register ] of
          false -> H.modify_ _ { route = Just dest }
          _ -> pure unit
      pure $ Just a

  -- Display the login page instead of the expected page if there is no current user
  authorize :: Maybe ProfileWithIdAndEmail -> H.ComponentHTML Action ChildSlots m -> H.ComponentHTML Action ChildSlots m
  authorize mbProfile html = case mbProfile of
    Nothing ->
      HH.slot (SProxy :: _ "login") unit Login.component { redirect: false, registerSuccess: false } absurd
    Just _ ->
      html

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route, currentUser } = case route of
    Just r -> case r of
      -- PUBLIC ----------------------------------------------------------------
      Home ->
        HH.slot (SProxy :: _ "home") unit Home.component {} absurd

      About ->
        HH.slot (SProxy :: _ "about") unit About.component {} absurd

      Discover ->
        HH.slot (SProxy :: _ "discover") unit Discover.component {} absurd

      Pricing ->
        HH.slot (SProxy :: _ "pricing") unit Pricing.component {} absurd

      Changelog ->
        HH.slot (SProxy :: _ "changelog") unit Changelog.component {} absurd

      Profile _ ->
        HH.slot (SProxy :: _ "profile") unit Profile.component {} absurd

      -- LEGAL -----------------------------------------------------------------

      Terms ->
        HH.slot (SProxy :: _ "terms") unit Terms.component {} absurd

      Policy ->
        HH.slot (SProxy :: _ "policy") unit Policy.component {} absurd

      -- AUTH ------------------------------------------------------------------
      Login ->
        HH.slot (SProxy :: _ "login") unit Login.component { redirect: true, registerSuccess: false } absurd

      Register ->
        HH.slot (SProxy :: _ "register") unit Register.component unit absurd

      VerifyEmailSuccess ->
        HH.slot (SProxy :: _ "verifySuccess") unit Login.component { redirect: true, registerSuccess: true } absurd

      VerifyEmailFailure ->
        HH.slot (SProxy :: _ "verifyFailure") unit VerifyFailure.component {} absurd

      -- PRIVATE ---------------------------------------------------------------
      Dashboard ->
        HH.slot (SProxy :: _ "dashboard") unit Dashboard.component {} absurd
          # authorize currentUser

      Resources ->
        HH.slot (SProxy :: _ "resources") unit Resources.component {} absurd
          # authorize currentUser

      Settings ->
        HH.slot (SProxy :: _ "settings") unit Settings.component {} absurd
          # authorize currentUser

      CreateList ->
        HH.slot (SProxy :: _ "createList") unit CreateList.component {} absurd
          # authorize currentUser

      CreateResource { url } ->
        HH.slot (SProxy :: _ "createResource") unit CreateResource.component { url } absurd
          # authorize currentUser

      ViewList _ ->
        HH.slot (SProxy :: _ "viewList") unit ViewList.component {} absurd

      EditList listSlug ->
        HH.slot (SProxy :: _ "editList") unit EditList.component { listSlug } absurd
          # authorize currentUser

      IntegrationsList listSlug ->
        HH.slot (SProxy :: _ "listintegrations") unit ListIntegrations.component { listSlug } absurd
          # authorize currentUser

    Nothing ->
      HH.div_ [ HH.text "Oh no! That page wasn't found." ]
