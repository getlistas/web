-- | We only receive a portion of large resources like "all resources" for performance reasons.
-- | This module exports a type to represent partial data in an array format.
module Listasio.Data.PaginatedArray where

type PaginatedArray a
  = { total :: Int
    , body :: Array a
    }
