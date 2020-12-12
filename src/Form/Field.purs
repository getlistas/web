module Doneq.Form.Field where

import Prelude
import DOM.HTML.Indexed (HTMLinput)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant)
import Doneq.Component.HTML.Utils (maybeElem)
import Doneq.Form.Validation (errorToString)
import Doneq.Form.Validation as V
import Formless as F
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Row as Row

submit :: forall i p. String -> HH.HTML i p
submit buttonText =
  HH.input
    -- TODO: styles
    [ HP.type_ HP.InputSubmit
    , HP.value buttonText
    ]

-- | This helper function creates an input field hooked up with Formless, including styles,
-- | events, error handling, and more. The function ensures at compile-time that the field we
-- | want actually exists in the form, that the input, error, and output types of the field are
-- | compatible, that the only properties you attempt to set on the HTML are actual valid <input>
-- | properties, and more.
-- |
-- | Let's deconstruct the type.
-- |
-- | First, the `IsSymbol` constraint requires that our first argument, `sym`, is a type-level
-- | string. You've seen these all over the place -- record labels are one example. We'll use
-- | this any time we need to talk about a value existing at a particular key in a record or
-- | a variant.
-- |
-- | Next, the two `Newtype` constraints require that you can use the `unwrap` function to
-- | transform the first type into the second type. In other words, the first type has to have
-- | a `Newtype` instance. This is how we'll unpack our self-defined Formless form type into
-- | either a raw record or variant we can work with.
-- |
-- | Next, the two `Cons` constraints require that there exists a value of the type given in
-- | the second argument at the label `sym` in the record or variant given in the last argument.
-- | For instance, we require that there's a field with an error type `FormError` and an input
-- | type `String` at the label `sym` in the row `fields`. In short, we require at compile-time
-- | that an input field of the correct type exists in our form state at the key we provided as
-- | the function's first argument.
-- |
-- | TODO: remove this once I understand it :)
input ::
  forall form act slots m sym fields inputs out t0 t1.
  IsSymbol sym =>
  Newtype (form Record F.FormField) { | fields } =>
  Newtype (form Variant F.InputFunction) (Variant inputs) =>
  Row.Cons sym (F.FormField V.FormError String out) t0 fields =>
  Row.Cons sym (F.InputFunction V.FormError String out) t1 inputs =>
  SProxy sym ->
  form Record F.FormField ->
  Array (HH.IProp HTMLinput (F.Action form act)) ->
  F.ComponentHTML form act slots m
input sym form props =
  HH.fieldset
    [] -- TODO: styles
    [ HH.input
        ( append
            -- TODO: styles
            [ HP.value $ F.getInput sym form
            , HE.onValueInput $ Just <<< F.setValidate sym
            ]
            props
        )
    , maybeElem (F.getError sym form) \err ->
        HH.div
          [] -- TODO: styles
          [ HH.text $ errorToString err ]
    ]
