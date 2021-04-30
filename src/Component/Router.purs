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
import Halogen.HTML.Properties as HP
import Listasio.Capability.Analytics (class Analytics)
import Listasio.Capability.Clipboard (class Clipboard)
import Listasio.Capability.LogMessages (class LogMessages)
import Listasio.Capability.Navigate (class Navigate, locationState, navigate, navigate_)
import Listasio.Capability.Now (class Now)
import Listasio.Capability.Resource.Integration (class ManageIntegration)
import Listasio.Capability.Resource.List (class ManageList)
import Listasio.Capability.Resource.Resource (class ManageResource)
import Listasio.Capability.Resource.User (class ManageUser)
import Listasio.Component.HTML.Footer (footer)
import Listasio.Component.HTML.Nav as Nav
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
import Tailwind as T
import Web.Event.Event (Event)

type State =
  { route :: Maybe Route
  , currentUser :: Maybe ProfileWithIdAndEmail
  }

data Query a
  = Navigate Route a

data Action
  = Initialize
  | NavigateAct Route Event
  | Receive { | WithCurrentUser () }

type ChildSlots =
  ( nav :: Nav.Slot
  , home :: OpaqueSlot Unit
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
  => MonadAsk {userEnv :: UserEnv | r} m
  => Now m
  => LogMessages m
  => Navigate m
  => ManageUser m
  => ManageResource m
  => ManageIntegration m
  => ManageList m
  => Clipboard m
  => Analytics m
  => H.Component HH.HTML Query {} Void m
component = Connect.component $ H.mkComponent
  { initialState: \{currentUser} -> {route: Nothing, currentUser}
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

    NavigateAct route e -> navigate_ e route

    Receive {currentUser} ->
      H.modify_ _ {currentUser = currentUser}

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      {route, currentUser} <- H.get
      when (route /= Just dest) do
        case isJust currentUser && dest `elem` authRoutes of
          false -> H.modify_ _ {route = Just dest}
          _ -> pure unit
      pure $ Just a
    where
    authRoutes = [ Login, Register, VerifyEmailSuccess, VerifyEmailFailure ]

  -- Display the login page instead of the expected page if there is no current user
  authorize :: Maybe ProfileWithIdAndEmail -> H.ComponentHTML Action ChildSlots m -> H.ComponentHTML Action ChildSlots m
  authorize mbProfile html = case mbProfile of
    Nothing ->
      HH.slot (SProxy :: _ "login") unit Login.component {redirect: false, registerSuccess: false} absurd
    Just _ ->
      html

  render :: State -> H.ComponentHTML Action ChildSlots m
  render {route, currentUser} =
    case route of
      Nothing ->
        -- TODO: 404 redirect? Or render proper page?
        layout Nothing
          $ HH.div [ HP.classes [ T.textGray400 ] ] [ HH.text "Oh no! That page wasn't found." ]

      Just Home ->
        HH.slot (SProxy :: _ "home") unit Home.component {} absurd

      Just r -> layout (Just r)
        case r of
          -- PUBLIC ----------------------------------------------------------------
          About ->
            HH.slot (SProxy :: _ "about") unit About.component {} absurd

          Discover ->
            HH.slot (SProxy :: _ "discover") unit Discover.component {} absurd

          Pricing ->
            HH.slot (SProxy :: _ "pricing") unit Pricing.component {} absurd

          Changelog ->
            HH.slot (SProxy :: _ "changelog") unit Changelog.component {} absurd

          Profile slug ->
            HH.slot (SProxy :: _ "profile") unit Profile.component {slug} absurd

          -- LEGAL -----------------------------------------------------------------
          Terms ->
            HH.slot (SProxy :: _ "terms") unit Terms.component {} absurd

          Policy ->
            HH.slot (SProxy :: _ "policy") unit Policy.component {} absurd

          -- AUTH ------------------------------------------------------------------
          Login ->
            HH.slot (SProxy :: _ "login") unit Login.component {redirect: true, registerSuccess: false} absurd

          Register ->
            HH.slot (SProxy :: _ "register") unit Register.component unit absurd

          VerifyEmailSuccess ->
            HH.slot (SProxy :: _ "verifySuccess") unit Login.component {redirect: true, registerSuccess: true} absurd

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

          CreateResource args ->
            HH.slot (SProxy :: _ "createResource") unit CreateResource.component args absurd
              # authorize currentUser

          ViewList _ ->
            HH.slot (SProxy :: _ "viewList") unit ViewList.component {} absurd

          EditList listSlug ->
            HH.slot (SProxy :: _ "editList") unit EditList.component {listSlug} absurd
              # authorize currentUser

          IntegrationsList listSlug ->
            HH.slot (SProxy :: _ "listintegrations") unit ListIntegrations.component {listSlug} absurd
              # authorize currentUser

          -- This shouldn't happend, as is already pattern matched above
          Home -> HH.text ""

    where
    layout currentRoute content =
      HH.div
        [ HP.classes [ T.minHScreen, T.wScreen, T.flex, T.flexCol, T.bgGray10 ] ]
        [ HH.div
            [ HP.classes [ T.container, T.mxAuto, T.mdPx4, T.xlPx0 ] ]
            [ HH.slot (SProxy :: _ "nav") unit Nav.component {route: currentRoute} absurd
            ]
        , HH.div
            [ HP.classes [ T.container, T.mxAuto, T.px4, T.xlPx0, T.pb20, T.flex1 ] ]
            [ content
            ]
        , footer NavigateAct
        ]
