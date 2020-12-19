module Listasio.Foreign.Marked
  ( RawHTML
  , marked
  ) where

-- | The `Marked` library transforms an input string (which should be markdown) into an output HTML
-- | string, even if it failed to parse the input markdown. I'd like to know at glance that I'm
-- | using a string of HTML, so we'll use a newtype to track that information in the type system.
newtype RawHTML = RawHTML String

-- | The `markedImpl` function is a native JavaScript function we're importing into PureScript. The
-- | compiler will not check this type.
foreign import markedImpl :: String -> String

marked :: String -> RawHTML
marked str = RawHTML (markedImpl str)
