-- | Where there are forms, there is inevitably validation.
module Listasio.Form.Validation where

import Prelude

import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Either as Either
import Data.Maybe (Maybe(..))
import Data.String as String
import Formless as F
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
  Required -> "This field is required."
  TooShort _ -> "Not enough characters entered"
  TooLong _ -> "Too many characters entered"
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

-- | In order to validate a particular field, we need to give Formless a value of type `Validation`,
-- | which takes several type parameters:
-- |
-- | `form`: the fields of the particular form the validation is meant for. This lets
-- |   you do things like compare the value of one field to the value of another, checked at
-- |   compile-time. Unless your validation relies on a different field from the one being validated,
-- |   you'll usually leave this parameter open.
-- | `m`: which monad the Formless is being run in. This lets you perform effectful
-- |   computations like asynchronously runnning some server-side validation. Once again, unless you
-- |   need a specific monadic ability, this is usually left open.
-- | `e`: the possible error type which can result from the validator.
-- |   We'll always fill this in with our custom `FormError` type.
-- | `i`: the input type being validated. For a validator that operates on strings,
-- |   this would be `String`, for a validator that operates on numbers, this would be `Number`, and
-- |   so on. This is usually filled in with a concrete type or a constraint like `Monoid`.
-- | `o`: the parsed output that will result from successful validation. If your
-- |   validator checks whether a username is valid, it might have an input type of `String` and
-- |   an output type of `Username`. This is usually filled in with a concrete type, or asserted
-- |   to be the same as the input type.
-- |
-- | For the most part, the generic validation functions we'll write just need to transform some
-- | input into some output, possibly failing, without the need to refer to any other values in the
-- | form or perform effects. When you have a simple function of this form...
-- |
-- | ```purescript
-- | check:: forall i e o. i -> Either e o
-- |
-- | ```
-- |
-- | ...then you can use the `hoistFnE_` helper from Formless to automatically turn it into the
-- | correct `Validation` type. We'll use this helper to write several simple, pure validators and
-- | then make them compatible with Formless.

-- | Just check whether the input is the empty value using Monoid's `mempty`.
required :: forall form m a. Eq a => Monoid a => Monad m => F.Validation form m FormError a a
required = F.hoistFnE_ $ cond (_ /= mempty) Required

minLength :: forall form m. Monad m => Int -> F.Validation form m FormError String String
minLength n = F.hoistFnE_ $ cond (\str -> String.length str > n) $ TooShort n

maxLength :: forall form m. Monad m => Int -> F.Validation form m FormError String String
maxLength n = F.hoistFnE_ $ cond (\str -> String.length str <= n) $ TooLong n

maxLengthArr :: forall form m a. Monad m => Int -> F.Validation form m FormError (Array a) (Array a)
maxLengthArr n = F.hoistFnE_ $ cond (\as -> Array.length as <= n) $ TooMany n

-- | A fairly naive requirement that it at least includes the `@` symbol.
emailFormat :: forall form m. Monad m => F.Validation form m FormError String Email
emailFormat = F.hoistFnE_ $ map Email <<< cond (String.contains (String.Pattern "@")) InvalidEmail

usernameFormat :: forall form m. Monad m => F.Validation form m FormError String Username
usernameFormat = F.hoistFnE_ $ Either.note InvalidUsername <<< Username.parse

idFormat :: forall form m. Monad m => F.Validation form m FormError String ID
idFormat = F.hoistFnE_ $ Either.note InvalidUsername <<< ID.parse

slugFormat :: forall form m. Monad m => F.Validation form m FormError String Slug
slugFormat = F.hoistFnE_ $ Either.note InvalidSlug <<< Slug.parse

avatarFormat :: forall form m. Monad m => F.Validation form m FormError String Avatar
avatarFormat = F.hoistFnE_ $ Either.note InvalidAvatar <<< Avatar.parse

-- Utilities

note :: forall form m a b. Monad m => F.Validation form m FormError a b -> FormError -> F.Validation form m FormError a b
note v err = F.Validation \form value ->
 lmap (const err) <$> F.runValidation v form value

infixl 4 note as <?>

cond :: forall a. (a -> Boolean) -> FormError -> a -> Either FormError a
cond f err a = if f a then pure a else Left err

-- | Validate an input only if it isn't empty.
toOptional :: forall form m a b
   . Monoid a
  => Eq a
  => Monad m
  => F.Validation form m FormError a b
  -> F.Validation form m FormError a (Maybe b)
toOptional v = F.Validation \form value ->
  case value == mempty of
    true -> pure (pure Nothing)
    _ -> (map <<< map) Just (F.runValidation v form value)

-- | Validate an input only if it isn't empty.
requiredFromOptional :: forall form m a b.
     Eq a
  => Monad m
  => F.Validation form m FormError a b
  -> F.Validation form m FormError (Maybe a) b
requiredFromOptional v = F.Validation \form val ->
  case val of
    Just val_ -> (map <<< map) identity (F.runValidation v form val_)
    Nothing -> pure (Left Required)
