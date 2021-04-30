module Listasio.Data.Metrics where

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.DateTime (DateTime)
import Listasio.Data.DateTime as DateTime

type Metric = {date :: DateTime, completed_count :: Int}

metricCodec :: JsonCodec Metric
metricCodec =
  CAR.object "Metric"
    { date: DateTime.codec
    , completed_count: CA.int
    }
