module Listasio.Form.Field where

import Prelude

import DOM.HTML.Indexed (HTMLinput, HTMLtextarea)
import Data.Array (catMaybes)
import Data.Filterable (filter)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Utils (cx, maybeElem, whenElem)
import Listasio.Form.Validation (errorToString)
import Listasio.Form.Validation as V
import Tailwind as T
import Type.Row as Row

-- TODO: Move to Button module
submit :: forall i p. String -> Boolean -> HH.HTML i p
submit buttonText disabled =
  HH.input
    [ HP.type_ HP.InputSubmit
    , HP.value buttonText
    , HP.classes
        [ T.flex1
        , T.wFull
        , T.cursorPointer
        , T.disabledCursorNotAllowed
        , T.disabledOpacity50
        , T.py2
        , T.px4
        , T.bgKiwi
        , T.textWhite
        , T.roundedMd
        , T.shadowMd
        , T.hoverBgOpacity75
        , T.focusOutlineNone
        , T.focusRing2
        , T.focusRingOffset2
        , T.focusRingOffsetGray10
        , T.focusRingKiwi
        ]
    , HP.disabled disabled
    ]

cancel :: forall i p. String -> Boolean -> p -> HH.HTML i p
cancel buttonText disabled action =
  HH.input
    [ HP.type_ HP.InputButton
    , HP.value buttonText
    , HE.onClick \_ -> Just action
    , HP.classes
        [ T.flex1
        , T.wFull
        , T.cursorPointer
        , T.disabledCursorNotAllowed
        , T.disabledOpacity50
        , T.py2
        , T.px4
        , T.bgGray300
        , T.textWhite
        , T.fontSemibold
        , T.roundedMd
        , T.shadowMd
        , T.hoverBgOpacity75
        , T.focusOutlineNone
        , T.focusRing2
        , T.focusRingOffset2
        , T.focusRingOffsetGray10
        , T.focusRingKiwi
        ]
    , HP.disabled disabled
    ]

type InputProps form act
  = { required :: Boolean
    , hideOptional :: Boolean
    , placeholder :: Maybe String
    , id :: Maybe String
    , props :: Array (HH.IProp HTMLinput (F.Action form act))
    , type_ :: HP.InputType
    , message :: Maybe String
    , label :: Maybe String
    }

defaultProps :: forall form act. InputProps form act
defaultProps
  = { required: false
    , hideOptional: false
    , placeholder: Nothing
    , id: Nothing
    , props: []
    , type_: HP.InputText
    , message: Nothing
    , label: Nothing
    }

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
input ::
  forall form act slots m sym fields inputs out t0 t1.
  IsSymbol sym =>
  Newtype (form Record F.FormField) { | fields } =>
  Newtype (form Variant F.InputFunction) (Variant inputs) =>
  Row.Cons sym (F.FormField V.FormError String out) t0 fields =>
  Row.Cons sym (F.InputFunction V.FormError String out) t1 inputs =>
  SProxy sym ->
  form Record F.FormField ->
  InputProps form act ->
  F.ComponentHTML form act slots m
input sym form groupProps =
  HH.fieldset
    [ HP.classes [ T.wFull ] ]
    [ fieldLabel groupProps
    , HH.div
        [ HP.classes [ T.mt1, T.relative, T.roundedMd ] ]
        [ HH.input
            ( append
                ( catMaybes
                    [ Just $ HP.type_ groupProps.type_
                    , Just $ HP.value $ F.getInput sym form
                    , Just $ HE.onValueInput $ Just <<< F.setValidate sym
                    , Just $ HP.classes $ fieldInputClasses hasError
                    , Just $ HP.required groupProps.required
                    , HP.id_ <$> groupProps.id
                    , HP.name <$> groupProps.id
                    , HP.placeholder <$> groupProps.placeholder
                    ]
                )
                groupProps.props
            )
          -- TODO: extract as function for both input & textarea
        , whenElem hasError \_ ->
            HH.div
              [ HP.classes
                  [ T.absolute
                  , T.insetY0
                  , T.right0
                  , T.pr3
                  , T.flex
                  , T.itemsCenter
                  , T.pointerEventsNone
                  ]
              ]
              [ Icons.exclamationCircleSolid [ Icons.classes [ T.h5, T.w5, T.textRed500 ] ] ]
        ]
      -- TODO: extract as function for both input & textarea
    , whenElem (not hasError) \_ ->
        maybeElem groupProps.message \message ->
          HH.p
            [ HP.classes [ T.mt2, T.textSm, T.textGray500 ] ]
            [ HH.text message ]
      -- TODO: extract as function for both input & textarea
    , maybeElem mbError \error ->
          HH.p
            [ HP.classes [ T.mt2, T.textSm, T.textManzana ] ]
            [ HH.text $ errorToString error ]
    ]
  where
  mbError = filter (const $ F.getTouched sym form) $ F.getError sym form
  hasError = isJust mbError

type TextareaProps form act
  = { required :: Boolean
    , hideOptional :: Boolean
    , placeholder :: Maybe String
    , id :: Maybe String
    , props :: Array (HH.IProp HTMLtextarea (F.Action form act))
    , message :: Maybe String
    , label :: Maybe String
    }

textareaDefaultProps :: forall form act. TextareaProps form act
textareaDefaultProps
  = { required: false
    , hideOptional: false
    , placeholder: Nothing
    , id: Nothing
    , props: []
    , message: Nothing
    , label: Nothing
    }

textarea ::
  forall form act slots m sym fields inputs out t0 t1.
  IsSymbol sym =>
  Newtype (form Record F.FormField) { | fields } =>
  Newtype (form Variant F.InputFunction) (Variant inputs) =>
  Row.Cons sym (F.FormField V.FormError String out) t0 fields =>
  Row.Cons sym (F.InputFunction V.FormError String out) t1 inputs =>
  SProxy sym ->
  form Record F.FormField ->
  TextareaProps form act ->
  F.ComponentHTML form act slots m
textarea sym form groupProps =
  HH.fieldset
    [ HP.classes [ T.wFull ] ]
    [ fieldLabel groupProps
    , HH.div
        [ HP.classes [ T.mt1, T.relative, T.roundedMd ] ]
        [ HH.textarea
            ( append
                ( catMaybes
                    [ Just $ HP.value $ F.getInput sym form
                    , Just $ HE.onValueInput $ Just <<< F.setValidate sym
                    , Just $ HP.classes $ fieldInputClasses hasError
                    , Just $ HP.required groupProps.required
                    , HP.id_ <$> groupProps.id
                    , HP.name <$> groupProps.id
                    , HP.placeholder <$> groupProps.placeholder
                    ]
                )
                groupProps.props
            )
        , whenElem hasError \_ ->
            HH.div
              [ HP.classes
                  [ T.absolute
                  , T.insetY0
                  , T.right0
                  , T.pr3
                  , T.flex
                  , T.itemsCenter
                  , T.pointerEventsNone
                  ]
              ]
              [ Icons.exclamationCircleSolid [ Icons.classes [ T.h5, T.w5, T.textRed500 ] ]
              ]
        ]
    , whenElem (not hasError) \_ ->
        maybeElem groupProps.message \message ->
          HH.p
            [ HP.classes [ T.mt2, T.textSm, T.textGray500 ] ]
            [ HH.text message ]
    , maybeElem mbError \error ->
          HH.p
            [ HP.classes [ T.mt2, T.textSm, T.textManzana ] ]
            [ HH.text $ errorToString error ]
    ]
  where
  mbError = filter (const $ F.getTouched sym form) $ F.getError sym form
  hasError = isJust mbError

fieldLabel ::
  forall form act slots m fields inputs r.
  Newtype (form Record F.FormField) { | fields } =>
  Newtype (form Variant F.InputFunction) (Variant inputs) =>
  { label :: Maybe String, id :: Maybe String, required :: Boolean, hideOptional :: Boolean | r } ->
  F.ComponentHTML form act slots m
fieldLabel props =
  maybeElem props.label \label ->
    HH.div
      [ HP.classes [ T.flex, T.justifyBetween ] ]
      [ HH.label
          ( catMaybes
              [ Just $ HP.classes [ T.block, T.textSm, T.fontMedium, T.textGray400 ]
              , HP.for <$> props.id
              ]
          )
          [ HH.text label ]
      , whenElem (not props.required && not props.hideOptional) \_ ->
          HH.span [ HP.classes [ T.textSm, T.textGray300 ] ] [ HH.text "Optional" ]
      ]

fieldInputClasses :: Boolean -> Array H.ClassName
fieldInputClasses hasError =
  [ T.shadowSm
  , T.block
  , T.wFull
  , T.smTextSm
  , T.roundedMd
  , cx T.borderGray300 $ not hasError
  , cx T.focusRingKiwi $ not hasError
  , cx T.focusBorderKiwi $ not hasError
  , cx T.pr10 hasError
  , cx T.borderManzana hasError
  , cx T.focusRingManzana hasError
  , cx T.focusBorderManzana hasError
  , cx T.textRed900 hasError
  , cx T.placeholderRed300 hasError
  ]
