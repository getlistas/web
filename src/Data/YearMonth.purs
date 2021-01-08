module Listasio.Data.YearMonth where

import Prelude

import Data.DateTime (DateTime, Month, Year)
import Data.DateTime as DateTime
import Data.Enum (fromEnum)

data YearMonth = YearMonth Year Month

derive instance eqYearMonth :: Eq YearMonth
derive instance ordYearMonth :: Ord YearMonth

instance showYearMonth :: Show YearMonth where
  show (YearMonth y m) = show m <> " " <> show (fromEnum y)

fromDateTime :: DateTime -> YearMonth
fromDateTime dateTime =
  YearMonth <$> DateTime.year <*> DateTime.month $ DateTime.date dateTime
