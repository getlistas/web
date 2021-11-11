module Listasio.Component.HTML.ResourceForm where

import Prelude

import Control.Alt ((<|>))
import Data.Array (find) as A
import Data.Filterable (filter)
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
import Listasio.Data.Resource (Resource, ListResource)
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

newtype DDItem = DDItem {label :: String, value :: ID}

derive instance eqDDItem :: Eq DDItem
derive instance newtypeDDItem :: Newtype DDItem _

instance toTextDDItem :: ToText DDItem where
  toText = _.label <<< unwrap

type Slot
  = F.Slot ResourceForm FormQuery FormChildSlots Resource Unit

newtype ResourceForm (r :: Row Type -> Type) f = ResourceForm (r (FormRow f))
derive instance Newtype (ResourceForm r f) _

type FormRow :: (Type -> Type -> Type -> Type) -> Row Type
type FormRow f =
  ( title :: f V.FormError String (Maybe String)
  , url :: f V.FormError String String
  , description :: f V.FormError String (Maybe String)
  , thumbnail :: f V.FormError (Maybe String) (Maybe String)
  , list :: f V.FormError (Maybe ID) ID
  , tags :: f V.FormError String (Array String)
  )

data FormQuery a
  = SetCreateStatus (RemoteData String ListResource) a

derive instance functorFormQuery :: Functor FormQuery

data FormAction
  = FormInitialize
  | Submit Event.Event
  | HandleDropdown (DD.Message DDItem)
  | FetchMeta String
  | PasteUrl Clipboard.ClipboardEvent
  | Navigate Route Event.Event

type CreateInput
  = { url :: Maybe String
    , title :: Maybe String
    , text :: Maybe String
    }

data InitialInput
  = InputToCreate CreateInput
  | InputToEdit ListResource

urlFromInput :: InitialInput -> Maybe String
urlFromInput (InputToCreate {url}) = url
urlFromInput _ = Nothing

textFromInput :: InitialInput -> Maybe String
textFromInput (InputToCreate {text}) = text
textFromInput _ = Nothing

resourceFromInput :: InitialInput -> Maybe ListResource
resourceFromInput (InputToEdit resource) = Just resource
resourceFromInput _ = Nothing

isEdit :: InitialInput -> Boolean
isEdit (InputToEdit _) = true
isEdit _ = false

type FormInput
  = { lists :: Array ListWithIdUserAndMeta
    , selectedList :: Maybe ID
    , initialInput :: InitialInput
    }

type FormChildSlots = ( dropdown :: DD.Slot DDItem Unit )

type FormState =
  ( status :: RemoteData String Unit
  , meta :: RemoteData String ResourceMeta
  , lists :: Array ListWithIdUserAndMeta
  , selectedList :: Maybe ID
  , pastedUrl :: Maybe String
  , initialResource :: Maybe ListResource
  , isNew :: Boolean
  )

formComponent ::
  forall m.
  MonadAff m =>
  ManageResource m =>
  Navigate m =>
  F.Component ResourceForm FormQuery FormChildSlots FormInput Resource m
formComponent = F.component formInput $ F.defaultSpec
  { render = renderCreateResource
  , handleEvent = handleEvent
  , handleQuery = handleQuery
  , handleAction = handleAction
  , initialize = Just FormInitialize
  }
  where
  formInput :: FormInput -> F.Input ResourceForm FormState m
  formInput {lists, selectedList, initialInput} =
    { validators:
        ResourceForm
          { title: V.toOptional $ V.maxLength 150
          , url: V.required >>> V.maxLength 500 -- TODO URL validation ???
          , description:  V.toOptional $ V.maxLength 500
          , thumbnail: F.noValidation
          , list: V.requiredFromOptional F.noValidation
                   <?> V.WithMsg "Please select a list"
          , tags: V.maxLengthArr 4
                    <<< F.hoistFn_ (filter (not <<< String.null) <<< map trim <<< split (Pattern ","))
                    <?> V.WithMsg "Cannot have more than 4 tags"
          }
    , initialInputs: Just $ initialInputs initialInput
    , status: NotAsked
    , meta: NotAsked
    , lists
    , pastedUrl: urlFromInput initialInput <|> (filter Util.isUrl $ textFromInput initialInput)
    , selectedList
    , initialResource: resourceFromInput initialInput
    , isNew: not $ isEdit initialInput
    }

  initialInputs (InputToCreate {url, title, text}) = F.wrapInputFields
    { url: fromMaybe "" $ url <|> (filter Util.isUrl text)
      -- On Android PWA share the title sometimes has `+` instead of spaces
    , title: fromMaybe "" $ replaceAll (Pattern "+") (Replacement " ") <$> String.take 500 <$> title
    , description: fromMaybe "" $ filter (not <<< Util.isUrl) text
    , thumbnail: Nothing
    , list: Nothing
    , tags: ""
    }
  initialInputs (InputToEdit {url, title, description, thumbnail, list, tags}) = F.wrapInputFields
    { url: url
    , title: fromMaybe "" title
    , description: fromMaybe "" description
    , thumbnail: thumbnail
    , list: Just list
    , tags: joinWith ", " tags
    }

  handleEvent = F.raiseResult

  handleAction = case _ of
    Navigate route e -> navigate_ e route

    FormInitialize -> do
      {pastedUrl, selectedList, lists} <- H.get

      let mbSelectedItem = listToItem <$> ((\id -> A.find ((id == _) <<< _.id) lists) =<< selectedList)

      for_ mbSelectedItem \item ->
        H.query DD._dropdown unit (DD.select item)

      void $ H.fork $ for_ pastedUrl \url -> handleAction $ FetchMeta url

    PasteUrl event -> do
      mbUrl <- H.liftEffect $ filter Util.isUrl <$> traverse (DataTransfer.getData MediaType.textPlain) (Clipboard.clipboardData event)
      case mbUrl of
        Just url -> handleAction $ FetchMeta url
        Nothing -> pure unit

    FetchMeta url -> do
      H.modify_ _ {meta = Loading}
      mbMeta <- getMeta url
      case mbMeta of
        Just meta -> do
          H.modify_ _ {meta = Success meta}
          eval $ F.setValidate proxies.thumbnail meta.thumbnail
          {form} <- H.get
          for_
            (filter (const $ String.null $ F.getInput proxies.title form) meta.title)
            (eval <<< F.setValidate proxies.title)
          for_
            (filter (const $ String.null $ F.getInput proxies.description form) meta.description)
            (eval <<< F.setValidate proxies.description)
        Nothing ->
          H.modify_ _ {meta = Failure "Couldn't get suggestions"}

    Submit event -> do
      H.liftEffect $ Event.preventDefault event
      {status} <- H.get
      when (not $ isLoading status) do eval F.submit

    HandleDropdown (DD.Selected (DDItem {value})) ->
      eval $ F.setValidate proxies.list (Just value)

    HandleDropdown DD.Cleared ->
      eval $ F.setValidate proxies.list Nothing

    where
    eval act = F.handleAction handleAction handleEvent act

  handleQuery :: forall a. FormQuery a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    SetCreateStatus (Success newResource) a -> do
      H.modify_ _ {status = Success unit, initialResource = Just newResource}
      eval $ F.loadForm $ initialInputs $ InputToEdit newResource
      pure (Just a)

    SetCreateStatus status a -> do
      H.modify_ _ {status = map (const unit) status}
      pure $ Just a

    where
    eval act = F.handleAction handleAction handleEvent act

  proxies = F.mkSProxies (Proxy :: Proxy ResourceForm)

  renderCreateResource {form, status, lists, submitting, meta, isNew} =
    HH.form
      [ HE.onSubmit $ F.injAction <<< Submit
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
              Success {can_resolve} | not can_resolve -> HH.text ""

              Success {resource: Just resource} ->
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

              , let mbError = filter (const $ F.getTouched proxies.list form) $ F.getError proxies.list form
                 in maybeElem mbError \err ->
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
                  (submitting || isLoading status)
              ]
          ]
      , HH.div
          [ HP.classes [ T.mt4 ] ]
          [ HH.a
              [ safeHref HowTo
              , HE.onClick $ F.injAction <<< Navigate HowTo <<< Mouse.toEvent
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
    handler = F.injAction <<< HandleDropdown
    ddInput = {placeholder: "Choose a list", items: map listToItem lists}

    url =
      Field.input proxies.url form $ Field.defaultProps
        { label = Just "Link"
        , id = Just "url"
        , placeholder = Just "https://blog.com/some-blogpost"
        , required = true
        , props = [ HE.onPaste $ F.injAction <<< PasteUrl ]
        , message = case meta of
            Loading -> Just "Fetching title and description ..."
            Success {can_resolve} | not can_resolve -> Just "Failed to load metadata for this link"
            _ -> Nothing
        }

    titleField =
      Field.input proxies.title form $ Field.defaultProps
        { label = Just "Title"
        , id = Just "title"
        , placeholder = Nothing
        }

    description =
      Field.textarea proxies.description form $ Field.textareaDefaultProps
        { label = Just "Description"
        , id = Just "description"
        , placeholder = Nothing
        , props = [HP.rows 3]
        }

    tagsField =
      Field.input proxies.tags form $ Field.defaultProps
        { label = Just "Tags"
        , id = Just "tags"
        , placeholder = Nothing
        , message = Just "Separated by commas"
        }

  listToItem {id, title} = DDItem {value: id, label: title}
