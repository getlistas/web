module Listasio.Data.Ordering where

import Prelude

comparingOn :: forall a b. Ord b => (a -> b) -> (b -> b -> Ordering) -> (a -> a -> Ordering)
comparingOn aToB f x y = f (aToB x) (aToB y)
