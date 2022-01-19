module Listasio.Component.HTML.ResourceForm where

import Prelude

import Control.Alt ((<|>))
import Data.Array (find) as A
import Data.Either (Either(..))
import Data.Filterable (filter)
import Data.Lens (_Just, _Left, preview)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType.Common as MediaType
import Data.Newtype (class Newtype, unwrap)
import Data.String (Pattern(..), Replacement(..))
import Data.String (null, take) as String
import Data.String.Common (split, trim, joinWith, replaceAll)
import Data.Traversable (for_, traverse)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Capability.Resource.Resource (class ManageResource, getMeta)
import Listasio.Component.HTML.Dropdown as DD
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Resource as ResourceComponent
import Listasio.Component.HTML.Utils (maybeElem, safeHref, whenElem)
import Listasio.Data.ID (ID)
import Listasio.Data.List (ListWithIdUserAndMeta)
import Listasio.Data.Resource (ListResource)
import Listasio.Data.ResourceMetadata (ResourceMeta)
import Listasio.Data.Route (Route(..))
import Listasio.Form.Field as Field
import Listasio.Form.Validation (class ToText, (<?>))
import Listasio.Form.Validation as V
import Network.RemoteData (RemoteData(..), isFailure, isLoading)
import Select as Select
import Tailwind as T
import Type.Proxy (Proxy(..))
import Util as Util
import Web.Clipboard.ClipboardEvent as Clipboard
import Web.Event.Event as Event
import Web.HTML.Event.DataTransfer as DataTransfer
import Web.UIEvent.MouseEvent as Mouse

newtype DDItem = DDItem { label :: String, value :: ID }

derive instance eqDDItem :: Eq DDItem
derive instance newtypeDDItem :: Newtype DDItem _

instance toTextDDItem :: ToText DDItem where
  toText = _.label <<< unwrap

_slot :: Proxy "resourceForm"
_slot = Proxy

type Slot = H.Slot Query Output Unit

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( title :: f String V.FormError (Maybe String)
  , url :: f String V.FormError String
  , description :: f String V.FormError (Maybe String)
  , tags :: f String V.FormError (Array String)
  , list :: f (Maybe ID) V.FormError ID
  , thumbnail :: f (Maybe String) V.FormError (Maybe String)
  )

type FormInputs = { | Form F.FieldInput }

data Query a
  = SetCreateStatus (RemoteData String ListResource) a

type Input
  =
  { lists :: Array ListWithIdUserAndMeta
  , selectedList :: Maybe ID
  , initialInput :: InitialInput
  }

type Output = { | Form F.FieldOutput }

type FormContext
  = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Input Action

type FormlessAction = F.FormlessAction (Form F.FieldState)

data Action
  = Initialize
  | HandleDropdown (DD.Message DDItem)
  | FetchMeta String
  | PasteUrl Clipboard.ClipboardEvent
  | Navigate Route Event.Event
  -- Formless actions
  | Receive FormContext
  | Eval FormlessAction

type CreateInput
  =
  { url :: Maybe String
  , title :: Maybe String
  , text :: Maybe String
  }

data InitialInput
  = InputToCreate CreateInput
  | InputToEdit ListResource

urlFromInput :: InitialInput -> Maybe String
urlFromInput (InputToCreate { url }) = url
urlFromInput _ = Nothing

textFromInput :: InitialInput -> Maybe String
textFromInput (InputToCreate { text }) = text
textFromInput _ = Nothing

resourceFromInput :: InitialInput -> Maybe ListResource
resourceFromInput (InputToEdit resource) = Just resource
resourceFromInput _ = Nothing

isEdit :: InitialInput -> Boolean
isEdit (InputToEdit _) = true
isEdit _ = false

initialInputs :: InitialInput -> FormInputs
initialInputs (InputToCreate { url, title, text }) =
  { url: fromMaybe "" $ url <|> (filter Util.isUrl text)
  -- On Android PWA share the title sometimes has `+` instead of spaces
  , title: fromMaybe "" $ replaceAll (Pattern "+") (Replacement " ") <$> String.take 500 <$> title
  , description: fromMaybe "" $ filter (not <<< Util.isUrl) text
  , thumbnail: Nothing
  , list: Nothing
  , tags: ""
  }
initialInputs (InputToEdit { url, title, description, thumbnail, list, tags }) =
  { url: url
  , title: fromMaybe "" title
  , description: fromMaybe "" description
  , thumbnail: thumbnail
  , list: Just list
  , tags: joinWith ", " tags
  }

type ChildSlots = (dropdown :: DD.Slot DDItem Unit)

type State =
  { context :: FormContext
  , status :: RemoteData String Unit
  , meta :: RemoteData String ResourceMeta
  , lists :: Array ListWithIdUserAndMeta
  , selectedList :: Maybe ID
  , pastedUrl :: Maybe String
  , initialResource :: Maybe ListResource
  , isNew :: Boolean
  }

splitTags :: String -> (Array String)
splitTags = filter (not <<< String.null) <<< map trim <<< split (Pattern ",")

component
  :: forall m
   . MonadAff m
  => ManageResource m
  => Navigate m
  => H.Component Query Input Output m
component = F.formless { liftAction: Eval } mempty $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { initialize = Just Initialize
      , receive = Just <<< Receive
      , handleQuery = handleQuery
      , handleAction = handleAction
      }
  }
  where
  initialState context@{ input: { initialInput, selectedList, lists } } =
    { context
    , status: NotAsked
    , meta: NotAsked
    , lists
    , pastedUrl: urlFromInput initialInput <|> (filter Util.isUrl $ textFromInput initialInput)
    , selectedList
    , initialResource: resourceFromInput initialInput
    , isNew: not $ isEdit initialInput
    }

  handleAction :: Action -> H.HalogenM _ _ _ _ m Unit
  handleAction = case _ of
    Initialize -> do
      { pastedUrl, selectedList, lists, context } <- H.get

      let mbSelectedItem = listToItem <$> ((\id -> A.find ((id == _) <<< _.id) lists) =<< selectedList)

      for_ mbSelectedItem \item ->
        H.query DD._dropdown unit (DD.select item)

      void $ H.fork $ for_ pastedUrl \url -> handleAction $ FetchMeta url

      handleAction $ context.formActions.setFields $ F.mkFieldStates $ initialInputs context.input.initialInput

    PasteUrl event -> do
      mbUrl <- H.liftEffect $ filter Util.isUrl <$> traverse (DataTransfer.getData MediaType.textPlain) (Clipboard.clipboardData event)

      for_ mbUrl \url -> handleAction $ FetchMeta url

    FetchMeta url -> do
      H.modify_ _ { meta = Loading }
      mbMeta <- getMeta url
      case mbMeta of
        Just meta -> do
          H.modify_ _ { meta = Success meta }
          { actions, fields } <- H.gets _.context
          handleAction $ actions.thumbnail.modify $ _ { value = meta.thumbnail }
          for_ (filter (const $ String.null $ fields.title.value) meta.title) \title ->
            handleAction $ actions.title.modify $ _ { value = title }
          for_ (filter (const $ String.null $ fields.description.value) meta.description) \description -> do
            handleAction $ actions.description.modify $ _ { value = description }
        Nothing ->
          H.modify_ _ { meta = Failure "Couldn't get suggestions" }

    HandleDropdown (DD.Selected (DDItem { value })) -> do
      actions <- H.gets _.context.actions
      handleAction $ actions.list.modify $ _ { value = Just value }
      handleAction actions.list.validate

    HandleDropdown DD.Cleared -> do
      actions <- H.gets _.context.actions
      handleAction $ actions.list.modify $ _ { value = Nothing }
      handleAction actions.list.validate

    Navigate route e -> navigate_ e route

    -- Formless actions

    Receive context -> H.modify_ _ { context = context }

    Eval action -> F.eval action

  handleQuery :: forall a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ m (Maybe a)
  handleQuery = case _ of
    F.Query (SetCreateStatus (Success newResource) a) -> do
      H.modify_ _ { status = Success unit, initialResource = Just newResource }
      formActions <- H.gets _.context.formActions
      handleAction $ formActions.reset
      handleAction $ formActions.setFields $ F.mkFieldStates $ initialInputs $ InputToEdit newResource
      pure $ Just a

    F.Query (SetCreateStatus status a) -> do
      H.modify_ _ { status = map (const unit) status }
      pure $ Just a

    F.Validate changed reply ->
      pure $ Just $ reply $ F.validate changed
        { title: V.toOptional $ V.maxLength 150
        , url: V.required <=< V.maxLength 500 -- TODO URL validation ???
        , description: V.toOptional $ V.maxLength 500
        , thumbnail: Right
        , list: V.requiredFromOptional Right <?> V.WithMsg "Please select a list"
        , tags: V.maxLengthArr 4 <=< (Right <<< splitTags) <?> V.WithMsg "Cannot have more than 4 tags"
        }

    F.Submit output a -> do
      { status } <- H.get
      when (not $ isLoading status) $ F.raise output
      pure $ Just a

    _ -> pure Nothing

  listToItem { id, title } = DDItem { value: id, label: title }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { status, lists, meta, isNew, context: { formActions, fields, actions } } =
    HH.form
      [ HE.onSubmit formActions.handleSubmit
      , HP.noValidate true
      ]
      [ whenElem (isFailure status) \_ ->
          HH.div
            []
            [ HH.text "Failed to add resource" ]
      , HH.fieldset
          []
          [ url
          , case meta of
              Success { can_resolve } | not can_resolve -> HH.text ""

              Success { resource: Just resource } ->
                HH.div
                  [ HP.classes [ T.mt2, T.mb4 ] ]
                  [ HH.div
                      [ HP.classes [ T.textManzana, T.textSm, T.mb2 ] ]
                      [ HH.text "This resource already exists. Are you sure you want to add it again?" ]
                  , ResourceComponent.resource Nothing Nothing lists resource
                  ]

              _ -> HH.text ""

          , HH.div
              [ HP.classes [ T.mt4 ] ]
              [ HH.div
                  [ HP.classes [ T.mb1 ] ]
                  [ HH.label
                      [ HP.classes [ T.block, T.textSm, T.fontMedium, T.textGray400 ] ]
                      [ HH.text "List" ]
                  ]

              , HH.slot DD._dropdown unit (Select.component DD.input DD.spec) ddInput handler

              , let
                  mbError = preview (_Just <<< _Left) fields.list.result
                in
                  maybeElem mbError \err ->
                    HH.div
                      [ HP.classes [ T.textManzana, T.mt2 ] ]
                      [ HH.text $ V.errorToString err ]
              ]

          , HH.div
              [ HP.classes [ T.flex, T.flexCol, T.gap4, T.mt4 ] ]
              [ HH.div [] [ titleField ]
              , HH.div [] [ tagsField ]
              , HH.div [] [ description ]
              ]

          , whenElem (isFailure status) \_ ->
              HH.div
                [ HP.classes [ T.textManzana, T.my4 ] ]
                [ HH.text "Could not create resource :(" ]

          , HH.div
              [ HP.classes [ T.mt4 ] ]
              [ Field.submit
                  (if isNew then "Add resource" else "Save")
                  (isLoading status)
              ]
          ]
      , HH.div
          [ HP.classes [ T.mt4 ] ]
          [ HH.a
              [ safeHref HowTo
              , HE.onClick $ Navigate HowTo <<< Mouse.toEvent
              , HP.classes
                  [ T.textGray300
                  , T.hoverTextKiwi
                  , T.textSm
                  , T.flex
                  , T.gap2
                  ]
              ]
              [ Icons.info [ Icons.classes [ T.h5, T.w5 ] ]
              , HH.text "How to add resources"
              ]
          ]
      ]
    where
    handler = HandleDropdown
    ddInput = { placeholder: "Choose a list", items: map listToItem lists }

    url =
      Field.input fields.url actions.url $ Field.defaultProps
        { label = Just "Link"
        , id = Just "url"
        , placeholder = Just "https://blog.com/some-blogpost"
        , required = true
        , props = [ HE.onPaste PasteUrl ]
        , message = case meta of
            Loading -> Just "Fetching title and description ..."
            Success { can_resolve } | not can_resolve -> Just "Failed to load metadata for this link"
            _ -> Nothing
        }

    titleField =
      Field.input fields.title actions.title $ Field.defaultProps
        { label = Just "Title"
        , id = Just "title"
        , placeholder = Nothing
        }

    description =
      Field.textarea fields.description actions.description $ Field.textareaDefaultProps
        { label = Just "Description"
        , id = Just "description"
        , placeholder = Nothing
        , props = [ HP.rows 3 ]
        }

    tagsField =
      Field.input fields.tags actions.tags $ Field.defaultProps
        { label = Just "Tags"
        , id = Just "tags"
        , placeholder = Nothing
        , message = Just "Separated by commas"
        }

