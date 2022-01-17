module Listasio.Form.Field where

import Prelude

import DOM.HTML.Indexed (HTMLinput, HTMLtextarea)
import Data.Array (catMaybes)
import Data.Lens (_Just, _Left, preview)
import Data.Maybe (Maybe(..), isJust)
import Formless as F
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Input as Input
import Listasio.Component.HTML.Utils (maybeElem, whenElem)
import Listasio.Form.Validation as V
import Tailwind as T

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
        , T.hoverBgKiwiDark
        , T.focusOutlineNone
        , T.focusRing2
        , T.focusRingOffset2
        , T.focusRingKiwi
        ]
    , HP.disabled disabled
    ]

cancel :: forall i p. String -> Boolean -> p -> HH.HTML i p
cancel buttonText disabled action =
  HH.input
    [ HP.type_ HP.InputButton
    , HP.value buttonText
    , HE.onClick \_ -> action
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

-- TODO: unify with SimpleInputProps using Row Type Maaaagic!!!
type InputProps act
  =
  { required :: Boolean
  , hideOptional :: Boolean
  , placeholder :: Maybe String
  , id :: Maybe String
  , props :: Array (HH.IProp HTMLinput act)
  , type_ :: HP.InputType
  , message :: Maybe String
  , label :: Maybe String
  }

defaultProps :: forall act. InputProps act
defaultProps =
  { required: false
  , hideOptional: false
  , placeholder: Nothing
  , id: Nothing
  , props: []
  , type_: HP.InputText
  , message: Nothing
  , label: Nothing
  }

input
  :: forall action output i
   . F.FieldState String V.FormError output
  -> F.FieldAction action String V.FormError output
  -> InputProps action
  -> HH.HTML i action
input state actions { required, hideOptional, placeholder, id, props, type_, message, label } =
  Input.input $ (Input.defaultProps actions.handleChange)
    { value = state.value
    , error = preview (_Just <<< _Left) state.result
    , hideOptional = hideOptional
    , placeholder = placeholder
    , id = id
    , props = props
    , type_ = type_
    , message = message
    , required = required
    , label = label
    , blurAction = Just $ actions.handleBlur
    }

type TextareaProps act
  =
  { required :: Boolean
  , hideOptional :: Boolean
  , placeholder :: Maybe String
  , id :: Maybe String
  , props :: Array (HH.IProp HTMLtextarea act)
  , message :: Maybe String
  , label :: Maybe String
  , rows :: Maybe Int
  , disabled :: Boolean
  }

textareaDefaultProps :: forall act. TextareaProps act
textareaDefaultProps =
  { required: false
  , hideOptional: false
  , placeholder: Nothing
  , id: Nothing
  , props: []
  , message: Nothing
  , label: Nothing
  , rows: Nothing
  , disabled: false
  }

textarea
  :: forall action output i
   . F.FieldState String V.FormError output
  -> F.FieldAction action String V.FormError output
  -> TextareaProps action
  -> HH.HTML i action
textarea state actions groupProps =
  HH.fieldset
    [ HP.classes [ T.wFull ] ]
    [ Input.fieldLabel groupProps
    , HH.div
        [ HP.classes [ T.mt1, T.relative, T.roundedMd ] ]
        [ HH.textarea
            ( append
                ( catMaybes
                    [ Just $ HP.value state.value
                    , Just $ HE.onValueInput actions.handleChange
                    , Just $ HE.onBlur actions.handleBlur
                    , Just $ HP.classes $ Input.fieldInputClasses
                        { hasError, iconBefore: false, iconAfter: false }
                    , Just $ HP.required groupProps.required
                    , HP.id <$> groupProps.id
                    , HP.name <$> groupProps.id
                    , HP.placeholder <$> groupProps.placeholder
                    , HP.rows <$> groupProps.rows
                    , Just $ HP.disabled groupProps.disabled
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
            [ HP.classes [ T.mt1, T.textSm, T.textGray500 ] ]
            [ HH.text message ]
    , maybeElem mbError \error ->
        HH.p
          [ HP.classes [ T.mt1, T.textSm, T.textManzana ] ]
          [ HH.text $ V.errorToString error ]
    ]
  where
  mbError = preview (_Just <<< _Left) state.result
  hasError = isJust mbError
