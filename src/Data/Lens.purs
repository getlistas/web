module Listasio.Data.Lens where

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))

_actionInProgress :: forall a r. Lens' { actionInProgress :: a | r } a
_actionInProgress = prop (SProxy :: SProxy "actionInProgress")

_completed_count :: forall a r. Lens' { completed_count :: a | r } a
_completed_count = prop (SProxy :: SProxy "completed_count")

_confirmDelete :: forall a r. Lens' { confirmDelete :: a | r } a
_confirmDelete = prop (SProxy :: SProxy "confirmDelete")

_count :: forall a r. Lens' { count :: a | r } a
_count = prop (SProxy :: SProxy "count")

_currentUser :: forall a r. Lens' { currentUser :: a | r } a
_currentUser = prop (SProxy :: SProxy "currentUser")

_description :: forall a r. Lens' { description :: a | r } a
_description = prop (SProxy :: SProxy "description")

_id :: forall a r. Lens' { id :: a | r } a
_id = prop (SProxy :: SProxy "id")

_items :: forall a r. Lens' { items :: a | r } a
_items = prop (SProxy :: SProxy "items")

_isProcessingAction :: forall a r. Lens' { isProcessingAction :: a | r } a
_isProcessingAction = prop (SProxy :: SProxy "isProcessingAction")

_is_public :: forall a r. Lens' { is_public :: a | r } a
_is_public = prop (SProxy :: SProxy "is_public")

_email :: forall a r. Lens' { email :: a | r } a
_email = prop (SProxy :: SProxy "email")

_last_completed_at :: forall a r. Lens' { last_completed_at :: a | r } a
_last_completed_at = prop (SProxy :: SProxy "last_completed_at")

_list :: forall a r. Lens' { list :: a | r } a
_list = prop (SProxy :: SProxy "list")

_lists :: forall a r. Lens' { lists :: a | r } a
_lists = prop (SProxy :: SProxy "lists")

_menuOpen :: forall a r. Lens' { menuOpen :: a | r } a
_menuOpen = prop (SProxy :: SProxy "menuOpen")

_mobileMenuOpen :: forall a r. Lens' { mobileMenuOpen :: a | r } a
_mobileMenuOpen = prop (SProxy :: SProxy "mobileMenuOpen")

_name :: forall a r. Lens' { name :: a | r } a
_name = prop (SProxy :: SProxy "name")

_newRss :: forall a r. Lens' { newRss :: a | r } a
_newRss = prop (SProxy :: SProxy "newRss")

_next :: forall a r. Lens' { next :: a | r } a
_next = prop (SProxy :: SProxy "next")

_profile :: forall a r. Lens' { profile :: a | r } a
_profile = prop (SProxy :: SProxy "profile")

_resource_metadata :: forall a r. Lens' { resource_metadata :: a | r } a
_resource_metadata = prop (SProxy :: SProxy "resource_metadata")

_resources :: forall a r. Lens' { resources :: a | r } a
_resources = prop (SProxy :: SProxy "resources")

_showMenu :: forall a r. Lens' { showMenu :: a | r } a
_showMenu = prop (SProxy :: SProxy "showMenu")

_showMore :: forall a r. Lens' { showMore :: a | r } a
_showMore = prop (SProxy :: SProxy "showMore")

_subscriptions :: forall a r. Lens' { subscriptions :: a | r } a
_subscriptions = prop (SProxy :: SProxy "subscriptions")

_rss :: forall a r. Lens' { rss :: a | r } a
_rss = prop (SProxy :: SProxy "rss")

_rssResult :: forall a r. Lens' { rssResult :: a | r } a
_rssResult = prop (SProxy :: SProxy "rssResult")

_slug :: forall a r. Lens' { slug :: a | r } a
_slug = prop (SProxy :: SProxy "slug")

_showSettingsMenu :: forall a r. Lens' { showSettingsMenu :: a | r } a
_showSettingsMenu = prop (SProxy :: SProxy "showSettingsMenu")

_tags :: forall a r. Lens' { tags :: a | r } a
_tags = prop (SProxy :: SProxy "tags")

_title :: forall a r. Lens' { title :: a | r } a
_title = prop (SProxy :: SProxy "title")
