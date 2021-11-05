module Listasio.Component.HTML.Input where

import Prelude

import DOM.HTML.Indexed (HTMLinput)
import Data.Array (catMaybes)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String as S
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Utils (cx, maybeElem, whenElem)
import Listasio.Form.Validation (errorToString)
import Listasio.Form.Validation as V
import Tailwind as T
import Web.UIEvent.KeyboardEvent as KeyboardEvent

type SimpleInputProps act
  = { error :: Maybe V.FormError
    , value :: String
    , action :: String -> act
    , required :: Boolean
    , disabled :: Boolean
    , hideOptional :: Boolean
    , placeholder :: Maybe String
    , id :: Maybe String
    , props :: Array (HH.IProp HTMLinput act)
    , type_ :: HP.InputType
    , message :: Maybe String
    , label :: Maybe String
    , iconBefore :: Maybe Icons.Icon
    , iconAfter :: Maybe Icons.Icon
    }

defaultProps :: forall act. (String -> act) -> SimpleInputProps act
defaultProps action
  = { value: ""
    , error: Nothing
    , action
    , disabled: false
    , required: false
    , hideOptional: false
    , placeholder: Nothing
    , id: Nothing
    , props: []
    , type_: HP.InputText
    , message: Nothing
    , label: Nothing
    , iconBefore: Nothing
    , iconAfter: Nothing
    }

input :: forall p i. SimpleInputProps i -> HH.HTML p i
input groupProps =
  HH.fieldset
    [ HP.classes [ T.wFull ] ]
    [ fieldLabel groupProps
    , HH.div
        [ HP.classes [ cx T.mt1 $ isJust groupProps.label, T.relative, T.roundedMd ] ]
        [ maybeElem groupProps.iconBefore \icon ->
            HH.div
              [ HP.classes
                  [ T.absolute
                  , T.insetY0
                  , T.left0
                  , T.pl3
                  , T.flex
                  , T.itemsCenter
                  , T.pointerEventsNone
                  ]
              ]
              [ icon [ Icons.classes [ T.h5, T.w5, T.textGray300 ] ] ]
          , HH.input
            ( append
                ( catMaybes
                    [ Just $ HP.type_ groupProps.type_
                    , Just $ HP.value groupProps.value
                    , Just $ HE.onValueInput groupProps.action
                    , Just $ HP.classes $ fieldInputClasses
                        { hasError: isJust groupProps.error
                        , iconBefore: isJust groupProps.iconBefore
                        , iconAfter: isJust groupProps.iconAfter
                        }
                    , Just $ HP.required groupProps.required
                    , Just $ HP.disabled groupProps.disabled
                    , HP.id <$> groupProps.id
                    , HP.name <$> groupProps.id
                    , HP.placeholder <$> groupProps.placeholder
                    ]
                )
                groupProps.props
            )
          -- TODO: extract as function for both input & textarea
        , case groupProps.error of
            Just _ ->
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
            Nothing ->
              maybeElem groupProps.iconAfter \icon ->
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
                  [ icon [ Icons.classes [ T.h5, T.w5, T.textGray300 ] ] ]
        ]
      -- TODO: extract as function for both input & textarea
    , whenElem (not $ isJust groupProps.error) \_ ->
        maybeElem groupProps.message \message ->
          HH.p
            [ HP.classes [ T.mt1, T.textSm, T.textGray500 ] ]
            [ HH.text message ]
      -- TODO: extract as function for both input & textarea
    , maybeElem groupProps.error \error ->
          HH.p
            [ HP.classes [ T.mt1, T.textSm, T.textManzana ] ]
            [ HH.text $ errorToString error ]
    ]

fieldLabel ::
  forall r i p.
  { label :: Maybe String
  , id :: Maybe String
  , required :: Boolean
  , hideOptional :: Boolean
  | r
  } ->
  HH.HTML p i
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

type ClassesArgs
  = { hasError :: Boolean
    , iconBefore :: Boolean
    , iconAfter :: Boolean
    }

fieldInputClasses :: ClassesArgs -> Array H.ClassName
fieldInputClasses { hasError, iconBefore, iconAfter } =
  [ T.shadowSm
  , T.block
  , T.wFull
  , T.smTextSm
  , T.roundedMd
  , T.disabledCursorNotAllowed
  , T.disabledBgGray50
  , cx T.textGray400 $ not hasError
  , cx T.pl10 $ iconBefore
  , cx T.pr10 $ iconAfter
  , cx T.borderGray300 $ not hasError
  , cx T.focusRingKiwi $ not hasError
  , cx T.focusBorderKiwi $ not hasError
  , cx T.pr10 hasError
  , cx T.borderManzana hasError
  , cx T.focusRingManzana hasError
  , cx T.focusBorderManzana hasError
  , cx T.textRed900 hasError
  , cx T.placeholderRed300 hasError
  , cx T.placeholderGray200 $ not hasError
  ]

type SearchProps i =
  { value :: String
  , placeholder :: Maybe String
  , onValueInput :: String -> i
  , onEscape :: i
  , noOp :: i
  }

search :: forall i p. SearchProps i -> HH.HTML p i
search props =
  HH.div
    [ HP.classes [ T.relative ] ]
    [ HH.input
        [ HP.placeholder $ fromMaybe "" props.placeholder
        , HP.classes
            [ T.wFull
            , T.roundedLg
            , T.smTextSm
            , T.border
            , T.borderGray300
            , T.pl4
            , T.pr8
            , T.py2
            , T.focusRing2
            , T.focusRingKiwi
            , T.focusOutlineNone
            , T.focusBorderKiwi
            , T.focusZ10
            ]
        , HP.value props.value
        , HE.onValueInput props.onValueInput
        , HE.onKeyDown \e ->
            case KeyboardEvent.code e of
              "Escape" -> props.onEscape
              _ -> props.noOp
        ]
    , HH.button
        [ HP.classes
            [ T.absolute
            , T.right2
            , T.top2
            , T.bottom2
            , T.roundedMd
            , T.textGray300
            , T.hoverTextKiwi
            , T.cursorPointer
            , T.focusOutlineNone
            , T.focusRing2
            , T.focusRingKiwi
            , T.focusBorderKiwi
            ]
        , HE.onClick $ const props.onEscape
        , HP.disabled $ S.null props.value
        ]
        [ case props.value of
           "" -> Icons.search [ Icons.classes [ T.h4, T.w4 ] ]
           _ -> Icons.x [ Icons.classes [ T.h4, T.w4 ] ]
        ]
    ]
