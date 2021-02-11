module Listasio.Data.Lens where

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))

_completed_count :: forall a r. Lens' { completed_count :: a | r } a
_completed_count = prop (SProxy :: SProxy "completed_count")

_count :: forall a r. Lens' { count :: a | r } a
_count = prop (SProxy :: SProxy "count")

_id :: forall a r. Lens' { id :: a | r } a
_id = prop (SProxy :: SProxy "id")

_items :: forall a r. Lens' { items :: a | r } a
_items = prop (SProxy :: SProxy "items")

_list :: forall a r. Lens' { list :: a | r } a
_list = prop (SProxy :: SProxy "list")

_markingAsDone :: forall a r. Lens' { markingAsDone :: a | r } a
_markingAsDone = prop (SProxy :: SProxy "markingAsDone")

_next :: forall a r. Lens' { next :: a | r } a
_next = prop (SProxy :: SProxy "next")

_resource_metadata :: forall a r. Lens' { resource_metadata :: a | r } a
_resource_metadata = prop (SProxy :: SProxy "resource_metadata")

_resources :: forall a r. Lens' { resources :: a | r } a
_resources = prop (SProxy :: SProxy "resources")
