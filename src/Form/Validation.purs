-- | Where there are forms, there is inevitably validation.
module Listasio.Form.Validation where

import Prelude

import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Either as Either
import Data.Maybe (Maybe(..))
import Data.String as String
import Listasio.Data.Avatar (Avatar)
import Listasio.Data.Avatar as Avatar
import Listasio.Data.Email (Email(..))
import Listasio.Data.ID (ID)
import Listasio.Data.ID as ID
import Listasio.Data.Username (Username)
import Listasio.Data.Username as Username
import Slug (Slug)
import Slug as Slug

data FormError
  = Required
  | TooShort Int
  | TooLong Int
  | TooMany Int
  | InvalidEmail
  | InvalidUsername
  | InvalidSlug
  | InvalidAvatar
  | InvalidID
  | WithMsg String

errorToString :: FormError -> String
errorToString = case _ of
  Required -> "Don't forget this one"
  TooShort n -> "Too short (min. " <> show n <> ")"
  TooLong n -> "Too long (max. " <> show n <> ")"
  TooMany n -> "Cannot have more than " <> show n
  InvalidEmail -> "Invalid email address"
  InvalidUsername -> "Invalid username"
  InvalidSlug -> "Invalid username"
  InvalidAvatar -> "Invalid image URL"
  InvalidID -> "This field is invalid"
  WithMsg msg -> msg

class ToText item where
  toText :: item -> String

instance toTextString :: ToText String where
  toText = identity

-- | A small helper function for writing validation functions that rely on a
-- | true/false predicate.
check :: forall a. (a -> Boolean) -> FormError -> a -> Either FormError a
check f err a
  | f a = Right a
  | otherwise = Left err

-- | Just check whether the input is the empty value using Monoid's `mempty`.
required :: forall a. Eq a => Monoid a => a -> Either FormError a
required = check (_ /= mempty) Required

minLength :: Int -> String -> Either FormError String
minLength n = check (\str -> String.length str >= n) $ TooShort n

maxLength :: Int -> String -> Either FormError String
maxLength n = check (\str -> String.length str <= n) $ TooLong n

maxLengthArr :: forall a. Int -> (Array a) -> Either FormError (Array a)
maxLengthArr n = check (\as -> Array.length as <= n) $ TooMany n

-- | A fairly naive requirement that it at least includes the `@` symbol.
emailFormat :: String -> Either FormError Email
emailFormat = map Email <<< check (String.contains (String.Pattern "@")) InvalidEmail

usernameFormat :: String -> Either FormError Username
usernameFormat = Either.note InvalidUsername <<< Username.parse

idFormat :: String -> Either FormError ID
idFormat = Either.note InvalidUsername <<< ID.parse

slugFormat :: String -> Either FormError Slug
slugFormat = Either.note InvalidSlug <<< Slug.parse

avatarFormat :: String -> Either FormError Avatar
avatarFormat = Either.note InvalidAvatar <<< Avatar.parse

-- Utilities

note :: forall a b. (a -> Either FormError b) -> FormError -> a -> Either FormError b
note v err = lmap (const err) <<< v

infixl 4 note as <?>

-- | Validate an input only if it isn't empty.
toOptional
  :: forall a b
   . Monoid a
  => Eq a
  => (a -> Either FormError b)
  -> (a -> Either FormError (Maybe b))
toOptional v value
  | value == mempty = Right Nothing
  | otherwise = map Just $ v value

-- | Validate an input only if it isn't empty.
requiredFromOptional
  :: forall a b
   . Eq a
  => (a -> Either FormError b)
  -> (Maybe a -> Either FormError b)
requiredFromOptional v = case _ of
  Just value -> v value
  Nothing -> Left Required
