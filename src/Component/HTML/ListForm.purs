module Listasio.Component.HTML.ListForm where

import Prelude

import Data.Either (Either(..))
import Data.Filterable (filter)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.String (joinWith)
import Data.String as String
import Data.String.Common (split, trim)
import Data.String.Pattern (Pattern(..))
import Data.Traversable (for_)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Component.HTML.Utils (whenElem)
import Listasio.Data.List (CreateListFields, ListWithIdUserAndMeta)
import Listasio.Form.Field as Field
import Listasio.Form.Validation ((<?>))
import Listasio.Form.Validation as V
import Network.RemoteData (RemoteData(..), isFailure, isLoading)
import Tailwind as T
import Type.Proxy (Proxy(..))

_slot :: Proxy "listForm"
_slot = Proxy

type Slot = H.Slot Query Output Unit

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( title :: f String V.FormError String
  , description :: f String V.FormError (Maybe String)
  , tags :: f String V.FormError (Array String)
  , is_public :: f Boolean V.FormError Boolean
  )

type FormInputs = { | Form F.FieldInput }

data Query a
  = SetCreateStatus (RemoteData String ListWithIdUserAndMeta) a

type Input
  = { list :: Maybe ListWithIdUserAndMeta }

type Output = CreateListFields

type FormContext
  = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Input Action

type FormlessAction = F.FormlessAction (Form F.FieldState)

data Action
  = Initialize
  -- Formless actions
  | Receive FormContext
  | Eval FormlessAction

type State
  =
  { context :: FormContext
  , status :: RemoteData String Unit
  , isNew :: Boolean
  , initialList :: Maybe ListWithIdUserAndMeta
  }

initialInputs :: ListWithIdUserAndMeta -> FormInputs
initialInputs { title, description, tags, is_public } =
  { title
  , description: fromMaybe "" description
  , tags: joinWith ", " tags
  , is_public
  }

splitTags :: String -> (Array String)
splitTags = filter (not <<< String.null) <<< map trim <<< split (Pattern ",")

component
  :: forall m
   . MonadAff m
  => H.Component Query Input Output m
component =
  F.formless
    { liftAction: Eval }
    { title: "", description: "", tags: "", is_public: false } $ H.mkComponent
    { initialState: \context ->
        { context
        , status: NotAsked
        , isNew: isNothing context.input.list
        , initialList: context.input.list
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { initialize = Just Initialize
        , receive = Just <<< Receive
        , handleAction = handleAction
        , handleQuery = handleQuery
        }
    }
  where
  handleAction :: Action -> H.HalogenM _ _ _ _ m Unit
  handleAction = case _ of
    Initialize -> do
      { context } <- H.get

      for_ context.input.list \list ->
        handleAction $ context.formActions.setFields $ F.mkFieldStates $ initialInputs list

    -- Formless actions
    Receive context -> H.modify_ _ { context = context }

    Eval action -> F.eval action

  handleQuery :: forall a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ m (Maybe a)
  handleQuery = case _ of
    F.Query (SetCreateStatus (Success newList) a) -> do
      H.modify_ _ { status = Success unit, initialList = Just newList }
      formActions <- H.gets _.context.formActions
      handleAction $ formActions.reset
      handleAction $ formActions.setFields $ F.mkFieldStates $ initialInputs newList
      pure $ Just a

    F.Query (SetCreateStatus status a) -> do
      H.modify_ _ { status = map (const unit) status }
      pure $ Just a

    F.Validate changed reply ->
      pure $ Just $ reply $ F.validate changed
        { title: V.required <=< V.maxLength 100
        , description: V.toOptional $ V.maxLength 500
        , tags: V.maxLengthArr 4 <=< (Right <<< splitTags) <?> V.WithMsg "Cannot have more than 4 tags"
        , is_public: Right
        }

    F.Submit output a -> do
      { status, context } <- H.get
      when (not $ isLoading status) do
        F.raise output
        handleAction context.formActions.reset
      pure $ Just a

    _ -> pure Nothing

  render :: State -> H.ComponentHTML Action () m
  render { status, isNew, context: { formActions, formState, fields, actions } } =
    HH.form
      [ HE.onSubmit formActions.handleSubmit
      , HP.noValidate true
      ]
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
                  , HP.checked fields.is_public.value
                  , HE.onChecked actions.is_public.handleChange
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
              (formState.allTouched || isLoading status)
          ]
      ]

    where
    title =
      Field.input fields.title actions.title $ Field.defaultProps
        { label = Just "Title"
        , id = Just "title"
        , placeholder = Just "Learning How to Cook"
        , required = true
        }

    tags =
      Field.input fields.tags actions.tags $ Field.defaultProps
        { label = Just "Tags"
        , id = Just "tags"
        , message = Just "Separated by commas"
        }

    description =
      Field.textarea fields.description actions.description $ Field.textareaDefaultProps
        { label = Just "Description"
        , id = Just "description"
        , props = [ HP.rows 3 ]
        }
