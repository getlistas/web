module Listasio.Component.HTML.Input where

import Prelude

import DOM.HTML.Indexed (HTMLinput)
import Data.Array (catMaybes)
import Data.Maybe (Maybe(..), isJust)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Utils (cx, maybeElem, whenElem)
import Listasio.Form.Validation (errorToString)
import Listasio.Form.Validation as V
import Tailwind as T

type SimpleInputProps act
  = { error :: Maybe V.FormError
    , value :: String
    , action :: String -> Maybe act
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

defaultProps :: forall act. SimpleInputProps act
defaultProps
  = { value: ""
    , error: Nothing
    , action: const Nothing
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
        [ HP.classes [ T.mt1, T.relative, T.roundedMd ] ]
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
                    , HP.id_ <$> groupProps.id
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
            [ HP.classes [ T.mt2, T.textSm, T.textGray500 ] ]
            [ HH.text message ]
      -- TODO: extract as function for both input & textarea
    , maybeElem groupProps.error \error ->
          HH.p
            [ HP.classes [ T.mt2, T.textSm, T.textManzana ] ]
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
  ]
