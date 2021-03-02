module Listasio.Component.HTML.ListForm where

import Prelude

import Data.Filterable (filter)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Newtype (class Newtype)
import Data.String (joinWith)
import Data.String as String
import Data.String.Common (split, trim)
import Data.String.Pattern (Pattern(..))
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Component.HTML.Utils (whenElem)
import Listasio.Data.List (CreateListFields, ListWithIdAndUser)
import Listasio.Form.Field as Field
import Listasio.Form.Validation ((<?>))
import Listasio.Form.Validation as V
import Network.RemoteData (RemoteData(..), isFailure, isLoading)
import Tailwind as T
import Web.Event.Event as Event

type Slot
  = F.Slot ListForm FormQuery () CreateListFields Unit

newtype ListForm r f
  = ListForm
  ( r
      ( title :: f V.FormError String String
      , description :: f V.FormError String (Maybe String)
      , tags :: f V.FormError String (Array String)
      , is_public :: f V.FormError Boolean Boolean
      )
  )

derive instance newtypeListForm :: Newtype (ListForm r f) _

data FormQuery a
  = SetCreateStatus (RemoteData String ListWithIdAndUser) a

derive instance functorFormQuery :: Functor FormQuery

data FormAction
  = Submit Event.Event

type FormInput
  -- TODO: accept the Row Type version with as less fields as possible
  = { list :: Maybe ListWithIdAndUser }

type ListState
  = ( status :: RemoteData String Unit
    , isNew :: Boolean
    , initialList :: Maybe ListWithIdAndUser
    )

formComponent ::
  forall slots m.
  MonadAff m =>
  F.Component ListForm FormQuery slots FormInput CreateListFields m
formComponent =
  F.component formInput
    $ F.defaultSpec
        { render = render
        , handleEvent = handleEvent
        , handleQuery = handleQuery
        , handleAction = handleAction
        }
  where
  formInput :: FormInput -> F.Input ListForm ListState m
  formInput { list } =
    { validators:
        ListForm
          { title: V.required >>> V.maxLength 100
          , description: V.toOptional $ V.maxLength 500
          , tags: V.maxLengthArr 4
                    <<< F.hoistFn_ (filter (not <<< String.null) <<< map trim <<< split (Pattern ","))
                    <?> V.WithMsg "Cannot have more than 4 tags"
          , is_public: F.noValidation
          }
    , initialInputs: map initialInputs list
    , initialList: list
    , status: NotAsked
    , isNew: isNothing list
    }

  initialInputs { title, description, tags, is_public } = F.wrapInputFields
    { title
    , description: fromMaybe "" description
    , tags: joinWith ", " tags
    , is_public
    }

  eval act = F.handleAction handleAction handleEvent act

  handleEvent = F.raiseResult

  handleAction = case _ of
    Submit event -> do
      H.liftEffect $ Event.preventDefault event
      { status, dirty } <- H.get
      let shouldSubmit = dirty && not (isLoading status)
      when shouldSubmit do eval F.submit

  handleQuery :: forall a. FormQuery a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    SetCreateStatus (Success newList@{title, description, tags, is_public}) a -> do
      H.modify_ _ { status = Success unit, initialList = Just newList }
      eval $ F.loadForm $ initialInputs newList
      pure (Just a)
    SetCreateStatus status a -> do
      H.modify_ _ { status = map (const unit) status }
      pure (Just a)

  proxies = F.mkSProxies (F.FormProxy :: _ ListForm)

  render { dirty, form, status, submitting, isNew, initialList } =
    HH.form
      [ HE.onSubmit \ev -> Just $ F.injAction $ Submit ev ]
      [ HH.fieldset_
          [ title
          , HH.div
              [ HP.classes [ T.mt6 ] ]
              [ description
              ]
          , HH.div [ HP.classes [ T.mt6 ] ] [ tags ]
          , HH.label
              [ HP.classes [ T.flex, T.itemsCenter, T.my6, T.cursorPointer ] ]
              [ HH.input
                  [ HP.type_ HP.InputCheckbox
                  , HP.checked $ F.getInput proxies.is_public form
                  , HE.onChange $ \_ -> Just $ F.modify proxies.is_public not
                  , HP.classes
                      [ T.h6
                      , T.w6
                      , T.borderGray300
                      , T.roundedMd
                      , T.checkedBgKiwi
                      , T.focusRingKiwi
                      , T.textKiwi
                      ]
                  ]
              , HH.span
                  [ HP.classes [ T.fontMedium, T.ml2, T.textGray400 ] ]
                  [ HH.text "This is a public list" ]
              ]

          , whenElem (isFailure status) \_ ->
              HH.div
                [ HP.classes [ T.textRed500, T.my6 ] ]
                [ HH.text "Could not create list :(" ]

          , Field.submit
              (if isNew then "Create" else "Save")
              (not dirty || submitting || isLoading status)
          ]
      ]

    where
    title =
      Field.input proxies.title form $ Field.defaultProps
        { label = Just "Title"
        , id = Just "title"
        , placeholder = Just "YouTube videos" -- TODO better placeholder
        , required = true
        }

    tags =
      Field.input proxies.tags form $ Field.defaultProps
        { label = Just "Tags"
        , id = Just "tags"
        , placeholder = Just "videos, chill" -- TODO better placeholder
        }

    description =
      Field.textarea proxies.description form $ Field.textareaDefaultProps
        { label = Just "Description"
        , id = Just "description"
        , props = [HP.rows 3]
        }
