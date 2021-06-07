module Listasio.Data.DateTime where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.DateTime (DateTime)
import Data.DateTime (date) as Date
import Data.Date (Year)
import Data.Date (year) as Date
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.List (List, fromFoldable)
import Data.Newtype (unwrap)
import Data.PreciseDateTime as PDT
import Data.RFC3339String (RFC3339String(..))

codec :: JsonCodec DateTime
codec = CA.prismaticCodec from to CA.string
  where from = map PDT.toDateTimeLossy <<< PDT.fromRFC3339String <<< RFC3339String
        to = unwrap <<< PDT.toRFC3339String <<< PDT.fromDateTime

-- | Display a month and year
-- |
-- | Example: "January 2021"
toMonth :: DateTime -> String
toMonth = format dateFormatter
  where
  dateFormatter :: List FormatterCommand
  dateFormatter = fromFoldable
    [ MonthFull
    , Placeholder " "
    , YearFull
    ]

-- | Display a human-readable version of the datetime
-- |
-- | Example: "Wed Nov 5, 1999"
toDisplayWeekName :: DateTime -> String
toDisplayWeekName = format dateFormatter
  where
  dateFormatter :: List FormatterCommand
  dateFormatter = fromFoldable
    [ DayOfWeekNameShort
    , Placeholder " "
    , MonthShort
    , Placeholder " "
    , DayOfMonth
    , Placeholder ", "
    , YearFull
    ]

-- | An alternate way to display a human-readable version of the datetime
-- |
-- | Example: "November 5, 1999"
toDisplayMonthDayYear :: DateTime -> String
toDisplayMonthDayYear = format dateFormatter
  where
  dateFormatter :: List FormatterCommand
  dateFormatter = fromFoldable
    [ MonthFull
    , Placeholder " "
    , DayOfMonth
    , Placeholder ", "
    , YearFull
    ]

-- | Display (short) month and day
-- |
-- | Example: "Nov 5"
toDisplayDayMonth :: DateTime -> String
toDisplayDayMonth = format dateFormatter
  where
  dateFormatter :: List FormatterCommand
  dateFormatter = fromFoldable
    [ MonthShort
    , Placeholder " "
    , DayOfMonth
    ]

-- | Display (short) month, day and year
-- |
-- | Example: "Nov 5, 2021"
toDisplayDayMonthYear :: DateTime -> String
toDisplayDayMonthYear = format dateFormatter
  where
  dateFormatter :: List FormatterCommand
  dateFormatter = fromFoldable
    [ MonthShort
    , Placeholder " "
    , DayOfMonth
    , Placeholder ", "
    , YearFull
    ]

-- | If date is on the current year display (short) month and day, otherwise
-- | display (short) month, day and year.
-- |
-- | Example: "Nov 5"
-- | Example: "Nov 5, 2020"
toDisplayDayMonthRelative :: DateTime -> Year -> String
toDisplayDayMonthRelative date currentYear =
  if currentYear < Date.year (Date.date date)
    then toDisplayDayMonthYear date
    else toDisplayDayMonth date
