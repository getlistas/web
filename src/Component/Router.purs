-- | The `Router` component is the root of our Halogen application. Every other component is a
-- | direct descendent of this component. We'll use the router to choose which component to render
-- | given a particular `Route` and to manage the user's location in the application.
module Listasio.Component.Router where

import Prelude

import Data.Either (hush)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (for_)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
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
import Listasio.Component.HTML.NotFound as NotFound
import Listasio.Component.Utils (OpaqueSlot)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Route (Route(..), routeCodec)
import Listasio.Page.About as About
import Listasio.Page.Changelog as Changelog
import Listasio.Page.CreateList as CreateList
import Listasio.Page.CreateResource as CreateResource
import Listasio.Page.Dashboard as Dashboard
import Listasio.Page.Discover as Discover
import Listasio.Page.EditList as EditList
import Listasio.Page.History as History
import Listasio.Page.Home as Home
import Listasio.Page.ListIntegrations as ListIntegrations
import Listasio.Page.Login as Login
import Listasio.Page.Policy as Policy
import Listasio.Page.Pricing as Pricing
import Listasio.Page.Profile as Profile
import Listasio.Page.PublicList as PublicList
import Listasio.Page.Register as Register
import Listasio.Page.Settings as Settings
import Listasio.Page.Terms as Terms
import Listasio.Page.VerifyFailure as VerifyFailure
import Listasio.Page.ViewList as ViewList
import Listasio.Store as Store
import Routing.Duplex as RD
import Slug (Slug)
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
  | Receive (Connected (Maybe ProfileWithIdAndEmail) Unit)

type ChildSlots =
  ( nav :: Nav.Slot
  , home :: OpaqueSlot Unit
  , about :: OpaqueSlot Unit
  , discover :: OpaqueSlot Unit
  , pricing :: OpaqueSlot Unit
  , profile :: OpaqueSlot Unit
  , publicList :: OpaqueSlot Unit
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
  , history :: OpaqueSlot Unit
  , settings :: OpaqueSlot Unit
  , createList :: OpaqueSlot Unit
  , createResource :: OpaqueSlot Unit
  , viewList :: OpaqueSlot Unit
  , editList :: OpaqueSlot Unit
  , listIntegrations :: OpaqueSlot Unit
  )

component
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Now m
  => LogMessages m
  => Navigate m
  => ManageUser m
  => ManageResource m
  => ManageIntegration m
  => ManageList m
  => Clipboard m
  => Analytics m
  => H.Component Query Unit Void m
component = connect (selectEq _.currentUser) $ H.mkComponent
  { initialState: \{context: currentUser} -> {route: Nothing, currentUser}
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
      for_ initialRoute navigate

    NavigateAct route e -> navigate_ e route

    Receive {context: currentUser} ->
      H.modify_ _ {currentUser = currentUser}

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      {route, currentUser} <- H.get

      let isLoggedIn = isJust currentUser
          isAuthRoute = dest `elem` authRoutes

      when (route /= Just dest) do
        case isLoggedIn && isAuthRoute of
          false -> H.modify_ _ {route = Just dest}
          _ -> pure unit

      pure $ Just a

    where
    authRoutes = [ Login, Register, VerifyEmailSuccess, VerifyEmailFailure ]

  -- Display the login page instead of the expected page if there is no current user
  authorize :: Maybe ProfileWithIdAndEmail -> H.ComponentHTML Action ChildSlots m -> H.ComponentHTML Action ChildSlots m
  authorize mbProfile html = case mbProfile of
    Nothing -> HH.slot_ Login._slot unit Login.component {redirect: false, registerSuccess: false}

    Just _ -> html

  authorizeList :: Maybe ProfileWithIdAndEmail -> Slug -> H.ComponentHTML Action ChildSlots m -> H.ComponentHTML Action ChildSlots m
  authorizeList mbProfile pathSlug html = case mbProfile of
    Nothing -> HH.slot_ Login._slot unit Login.component {redirect: false, registerSuccess: false}

    Just {slug} | slug == pathSlug -> html

    Just _ -> NotFound.elem

  layout :: Maybe Route -> _ -> _
  layout currentRoute content =
    HH.div
      [ HP.classes [ T.minHScreen, T.wScreen, T.flex, T.flexCol, T.bgGray10 ] ]
      [ HH.div
          [ HP.classes [ T.container, T.mxAuto, T.mdPx4, T.xlPx0 ] ]
          [ HH.slot_ Nav._slot unit Nav.component {route: currentRoute} ]
      , HH.div
          [ HP.classes [ T.container, T.mxAuto, T.px4, T.xlPx0, T.pb20, T.flex1 ] ]
          [ content ]
      , footer NavigateAct
      ]

  render :: State -> H.ComponentHTML Action ChildSlots m
  render {route, currentUser} =
    -- removing this wrapping div causes the navigation from Home to other pages
    -- to render nothing at all, even when the lifecycle of the other pages is
    -- triggered
    HH.div
      []
      [ case route of
          Nothing -> layout Nothing NotFound.elem

          Just Home -> HH.slot_ Home._slot unit Home.component unit

          Just r ->
            layout
              (Just r)
              case r of
                -- PUBLIC ----------------------------------------------------------------
                About ->
                  HH.slot_ About._slot unit About.component unit

                Discover ->
                  HH.slot_ Discover._slot unit Discover.component unit

                Pricing ->
                  HH.slot_ Pricing._slot unit Pricing.component unit

                Changelog ->
                  HH.slot_ Changelog._slot unit Changelog.component unit

                Profile slug ->
                  HH.slot_ Profile._slot unit Profile.component {slug}

                PublicList user list ->
                  HH.slot_ PublicList._slot unit PublicList.component {user, list}

                -- LEGAL -----------------------------------------------------------------
                Terms ->
                  HH.slot_ Terms._slot unit Terms.component unit

                Policy ->
                  HH.slot_ Policy._slot unit Policy.component unit

                -- AUTH ------------------------------------------------------------------
                Login ->
                  HH.slot_ Login._slot unit Login.component {redirect: true, registerSuccess: false}

                Register ->
                  HH.slot_ Register._slot unit Register.component unit

                VerifyEmailSuccess ->
                  HH.slot_ Login._slot unit Login.component {redirect: true, registerSuccess: true}

                VerifyEmailFailure ->
                  HH.slot_ VerifyFailure._slot unit VerifyFailure.component unit

                -- PRIVATE ---------------------------------------------------------------
                Dashboard ->
                  HH.slot_ Dashboard._slot unit Dashboard.component unit
                    # authorize currentUser

                History ->
                  HH.slot_ History._slot unit History.component unit
                    # authorize currentUser

                Settings ->
                  HH.slot_ Settings._slot unit Settings.component unit
                    # authorize currentUser

                CreateList ->
                  HH.slot_ CreateList._slot unit CreateList.component unit
                    # authorize currentUser

                CreateResource args ->
                  HH.slot_ CreateResource._slot unit CreateResource.component args
                    # authorize currentUser

                ViewList _ ->
                  HH.slot_ ViewList._slot unit ViewList.component unit

                EditList user list ->
                  HH.slot_ EditList._slot unit EditList.component {user, list}
                    # authorizeList currentUser user

                -- TODO: validate that the current user is accessing this list
                IntegrationsList user list ->
                  HH.slot_ ListIntegrations._slot unit ListIntegrations.component {user, list}
                    # authorizeList currentUser user

                -- This shouldn't happend, as is already pattern matched above
                Home -> HH.text ""
      ]
