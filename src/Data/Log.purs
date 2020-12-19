-- | This module defines a type to represent well-formed logs to write to an external logging service.
-- |
-- | This module is rarely used in the rest of the application. It's a bit too low-level. In our
-- | business logic the critical thing is to report a particular error or message. We shouldn't have
-- | to care about how to format or gather metadata or the mechanics of sending the error to a
-- | particular reporting service.
-- |
-- | The `Listasio.Capability.LogMessages` module describes the higher-level interface to log an
-- | error or message that is used throughout the rest of the application. I'd recommend reading
-- | through that module as well.
module Listasio.Data.Log
  ( LogReason(..)
  , message
  , reason
  , timestamp
  , Log -- no constructors exported
  , mkLog
  ) where

import Prelude

import Data.DateTime (DateTime)
import Data.Either (either)
import Data.Foldable (fold)
import Data.Formatter.DateTime (formatDateTime)
import Listasio.Capability.Now (class Now, nowDateTime)

data LogReason
  = Debug
  | Info
  | Warn
  | Error

derive instance eqLogReason :: Eq LogReason

derive instance ordLogReason :: Ord LogReason

-- | `Log` type contains the metadata about a particular message
-- | along with the correctly-formatted message itself.
-- |
-- | We have not created a newtype instance nor exported the `Log` constructor,
-- | so this type cannot be created except by using functions in this module.
newtype Log
  = Log
  { reason :: LogReason
  , timestamp :: DateTime
  , message :: String
  }

derive instance eqLog :: Eq Log

message :: Log -> String
message (Log { message: m }) = m

reason :: Log -> LogReason
reason (Log { reason: r }) = r

timestamp :: Log -> DateTime
timestamp (Log { timestamp: t }) = t

mkLog :: forall m. Now m => LogReason -> String -> m Log
mkLog logReason inputMessage = do
  now <- nowDateTime
  let
    -- Will produce a header like "{DEBUG: 2018-10-25 11:25:29 AM]\nMessage contents..."
    headerWith start = fold [ "[", start, ": ", formatTimestamp now, "]\n", inputMessage ]

    -- Writes the header with the correct log reason
    formattedLog =
      headerWith case logReason of
        Debug -> "DEBUG"
        Info -> "INFO"
        Warn -> "WARNING"
        Error -> "ERROR"
  pure $ Log { reason: logReason, timestamp: now, message: formattedLog }
  where
  -- Will format "2018-10-25 11:25:29 AM"
  formatTimestamp =
    either (const "(Failed to assign time)") identity
      <<< formatDateTime "YYYY-DD-MM hh:mm:ss a"
