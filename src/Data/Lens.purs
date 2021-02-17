module Listasio.Data.Lens where

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))

_completed_count :: forall a r. Lens' { completed_count :: a | r } a
_completed_count = prop (SProxy :: SProxy "completed_count")

_count :: forall a r. Lens' { count :: a | r } a
_count = prop (SProxy :: SProxy "count")

_currentUser :: forall a r. Lens' { currentUser :: a | r } a
_currentUser = prop (SProxy :: SProxy "currentUser")

_forkInProgress :: forall a r. Lens' { forkInProgress :: a | r } a
_forkInProgress = prop (SProxy :: SProxy "forkInProgress")

_id :: forall a r. Lens' { id :: a | r } a
_id = prop (SProxy :: SProxy "id")

_items :: forall a r. Lens' { items :: a | r } a
_items = prop (SProxy :: SProxy "items")

_email :: forall a r. Lens' { email :: a | r } a
_email = prop (SProxy :: SProxy "email")

_last_completed_at :: forall a r. Lens' { last_completed_at :: a | r } a
_last_completed_at = prop (SProxy :: SProxy "last_completed_at")

_list :: forall a r. Lens' { list :: a | r } a
_list = prop (SProxy :: SProxy "list")

_markingAsDone :: forall a r. Lens' { markingAsDone :: a | r } a
_markingAsDone = prop (SProxy :: SProxy "markingAsDone")

_name :: forall a r. Lens' { name :: a | r } a
_name = prop (SProxy :: SProxy "name")

_next :: forall a r. Lens' { next :: a | r } a
_next = prop (SProxy :: SProxy "next")

_resource_metadata :: forall a r. Lens' { resource_metadata :: a | r } a
_resource_metadata = prop (SProxy :: SProxy "resource_metadata")

_resources :: forall a r. Lens' { resources :: a | r } a
_resources = prop (SProxy :: SProxy "resources")

_slug :: forall a r. Lens' { slug :: a | r } a
_slug = prop (SProxy :: SProxy "slug")
