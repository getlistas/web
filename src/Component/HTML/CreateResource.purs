module Listasio.Component.HTML.CreateResource where

import Prelude

import Data.Filterable (filter)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType.Common as MediaType
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Capability.Resource.Resource (class ManageResource, createResource, getMeta)
import Listasio.Component.HTML.Dropdown as DD
import Listasio.Component.HTML.Resource as ResourceComponent
import Listasio.Component.HTML.Utils (whenElem)
import Listasio.Data.ID (ID)
import Listasio.Data.List (ListWithIdAndUser)
import Listasio.Data.Resource (ListResource, Resource)
import Listasio.Data.ResourceMetadata (ResourceMeta)
import Listasio.Form.Field as Field
import Listasio.Form.Validation (class ToText)
import Listasio.Form.Validation as V
import Network.RemoteData (RemoteData(..))
import Select as Select
import Tailwind as T
import Util as Util
import Web.Clipboard.ClipboardEvent as Clipboard
import Web.Event.Event as Event
import Web.HTML.Event.DataTransfer as DataTransfer

type Slot = forall query. H.Slot query Output Unit

_createResource = SProxy :: SProxy "createResource"

data Action
  = Initialize
  | HandleFormMessage FormMessage

type State
  = { lists :: Array ListWithIdAndUser
    , url :: Maybe String
    }

type Input
  = { lists :: Array ListWithIdAndUser
    , url :: Maybe String
    }

data Output
  = Created ListResource

type ChildSlots
  = ( formless :: FormSlot )

component :: forall query m.
     MonadAff m
  => ManageResource m
  => H.Component HH.HTML query Input Output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }
  where
  initialState = identity

  handleAction :: Action -> H.HalogenM State Action ChildSlots Output m Unit
  handleAction = case _ of
    Initialize ->
      pure unit

    HandleFormMessage (FormSubmitted newResource) -> do
      { lists } <- H.get
      mbNewResource <- createResource newResource
      case mbNewResource of
        Just resource -> do
          H.raise $ Created resource
          void $ H.query F._formless unit $ F.injQuery $ SetCreateError false unit

        Nothing -> void $ H.query F._formless unit $ F.injQuery $ SetCreateError true unit

  render :: State -> HH.ComponentHTML Action ChildSlots m
  render { lists, url } =
    HH.div
      []
      [ HH.slot F._formless unit formComponent { lists, url } (Just <<< HandleFormMessage) ]

newtype DDItem = DDItem { label :: String, value :: ID }

derive instance eqDDItem :: Eq DDItem
derive instance newtypeDDItem :: Newtype DDItem _

instance toTextDDItem :: ToText DDItem where
  toText = _.label <<< unwrap

data FormMessage
  = FormSubmitted Resource

type FormSlot
  = F.Slot CreateResourceForm FormQuery FormChildSlots FormMessage Unit

newtype CreateResourceForm r f
  = CreateResourceForm
  ( r
      ( title :: f V.FormError String String
      , url :: f V.FormError String String
      , description :: f V.FormError String (Maybe String)
      , thumbnail :: f V.FormError (Maybe String) (Maybe String)
      , list :: f V.FormError (Maybe ID) ID
      )
  )

derive instance newtypeCreateResourceForm :: Newtype (CreateResourceForm r f) _

data FormQuery a
  = SetCreateError Boolean a

derive instance functorFormQuery :: Functor FormQuery

data FormAction
  = FormInitialize
  | Submit Event.Event
  | HandleDropdown (DD.Message DDItem)
  | FetchMeta String
  | PasteUrl Clipboard.ClipboardEvent

type FormInput
  = { lists :: Array ListWithIdAndUser
    , url :: Maybe String
    }

type FormChildSlots = ( dropdown :: DD.Slot DDItem Unit )

type FormState =
  ( createError :: Boolean
  , lists :: Array ListWithIdAndUser
  , meta :: RemoteData String ResourceMeta
  , pastedUrl :: Maybe String
  )

formComponent ::
  forall m.
  MonadAff m =>
  ManageResource m =>
  F.Component CreateResourceForm FormQuery FormChildSlots FormInput FormMessage m
formComponent = F.component formInput $ F.defaultSpec
  { render = renderCreateResource
  , handleEvent = handleEvent
  , handleQuery = handleQuery
  , handleAction = handleAction
  , initialize = Just FormInitialize
  }
  where
  formInput :: FormInput -> F.Input CreateResourceForm FormState m
  formInput { lists, url } =
    { validators:
        CreateResourceForm
          { title: V.required >>> V.minLength 1 >>> V.maxLength 150
          , url: V.required >>> V.minLength 1 >>> V.maxLength 500 -- TODO URL validation ???
          , description:  V.toOptional $ V.minLength 1 >>> V.maxLength 500
          , thumbnail: F.noValidation
          , list: V.requiredFromOptional F.noValidation
          }
    , initialInputs: Just $ initialInputs url
    , createError: false
    , lists
    , meta: NotAsked
    , pastedUrl: url
    }

  initialInputs url = F.wrapInputFields
    { url: fromMaybe "" url
    , title: ""
    , description: ""
    , thumbnail: Nothing
    , list: Nothing
    }

  handleEvent = case _ of
    F.Submitted outputs ->
      H.raise $ FormSubmitted $ F.unwrapOutputFields outputs
    _ -> pure unit

  handleAction = case _ of
    FormInitialize -> do
      { pastedUrl } <- H.get
      case pastedUrl of
        Just url -> handleAction $ FetchMeta url
        Nothing -> pure unit

    PasteUrl event -> do
      mbUrl <- H.liftEffect $ filter Util.isUrl <$> traverse (DataTransfer.getData MediaType.textPlain) (Clipboard.clipboardData event)
      case mbUrl of
        Just url -> handleAction $ FetchMeta url
        Nothing -> pure unit

    FetchMeta url -> do
      H.modify_ _ { meta = Loading }
      mbMeta <- getMeta url
      case mbMeta of
        Just meta -> do
          H.modify_ _ { meta = Success meta }
          eval $ F.setValidate proxies.thumbnail meta.thumbnail
          case meta.title of
            Just title -> eval $ F.setValidate proxies.title title
            Nothing -> pure unit
          case meta.description of
            Just description -> eval $ F.setValidate proxies.description description
            Nothing -> pure unit
        Nothing ->
          H.modify_ _ { meta = Failure "Couldn't gett suggestions" }

    Submit event -> do
      H.liftEffect $ Event.preventDefault event
      eval F.submit

    HandleDropdown msg ->
      case msg of
        DD.Selected (DDItem { value }) ->
          eval $ F.setValidate proxies.list (Just value)

        DD.Cleared ->
          eval $ F.setValidate proxies.list Nothing

    where
    eval act = F.handleAction handleAction handleEvent act

  handleQuery :: forall a. FormQuery a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    SetCreateError createError a -> do
      H.modify_ _ { createError = createError }
      when (not createError) do
        eval F.resetAll
        void $ H.query DD._dropdown unit DD.clear
      pure $ Just a

    where
    eval act = F.handleAction handleAction handleEvent act

  proxies = F.mkSProxies (F.FormProxy :: _ CreateResourceForm)

  -- TODO: submitting is only true while the form component is submitting
  --       but the async action actually happens in the parent component
  renderCreateResource { form, createError, lists, submitting, dirty, meta } =
    HH.form
      [ HE.onSubmit $ Just <<< F.injAction <<< Submit ]
      [ whenElem createError \_ ->
          HH.div
            []
            [ HH.text "Failed to add resource" ]
      , HH.fieldset
          []
          [ Field.input (Just "Link") proxies.url form
              [ HP.placeholder "https://blog.com/some-blogpost"
              , HP.type_ HP.InputText
              , HE.onPaste $ Just <<< F.injAction <<< PasteUrl
              ]

          , case meta of
              Loading ->
                HH.div
                  [ HP.classes [ T.textSm, T.pt2, T.textGray300 ] ]
                  [ HH.text "Fetching title and description ..." ]

              Success {can_resolve} | not can_resolve ->
                HH.div
                  [ HP.classes [ T.textSm, T.pt2, T.textManzana ] ]
                  [ HH.text "Looks like the link does not exist. Are you sure you got the right one?" ]

              Success {resource: Just resource} ->
                HH.div
                  [ HP.classes [ T.mt2, T.mb4 ] ]
                  [ HH.div
                      [ HP.classes [ T.textManzana, T.textSm, T.mb2 ] ]
                      [ HH.text "This resource already exists. Are you sure you want to add it again?" ]
                  , ResourceComponent.resource lists resource
                  ]

              _ ->
                HH.text ""

          , HH.div
              [ HP.classes [ T.mt4 ] ]
              [ HH.div
                  [ HP.classes [ T.mb2 ] ]
                  [ HH.label
                    [ HP.classes [ T.textGray400, T.textLg ] ]
                    [ HH.text "List" ]
                  ]
              , HH.slot DD._dropdown unit (Select.component DD.input DD.spec) ddInput handler
              ]

          , HH.div
              [ HP.classes [ T.flex, T.spaceX4 ] ]
              [ HH.div
                  [ HP.classes[ T.w40, T.h40, T.mt14 ]
                  ]
                  [ case meta of
                      Success {thumbnail: Just thumbnail} ->
                        HH.img
                          [ HP.src thumbnail
                          , HP.classes [ T.w40, T.h40, T.objectCover, T.roundedLg ]
                          ]

                      Loading ->
                        HH.div
                          [ HP.classes
                              [ T.w40
                              , T.h40
                              , T.flex
                              , T.flexCol
                              , T.justifyCenter
                              , T.itemsCenter
                              , T.textGray400
                              , T.bgGray100
                              , T.roundedLg
                              , T.text4xl
                              ]
                          ]
                          [ HH.text "⌛" ]

                      _ ->
                        HH.div
                          [ HP.classes
                              [ T.w40
                              , T.h40
                              , T.flex
                              , T.flexCol
                              , T.justifyCenter
                              , T.itemsCenter
                              , T.textGray400
                              , T.bgGray100
                              , T.roundedLg
                              , T.text4xl
                              ]
                          ]
                          [ HH.text "404" ]
                  ]
              , HH.div
                  []
                  [ HH.div
                      [ HP.classes [ T.mt4 ] ]
                      [ Field.input (Just "Title") proxies.title form
                          [ HP.placeholder "Some blogpost"
                          , HP.type_ HP.InputText
                          ]
                      ]
                  , HH.div
                      [ HP.classes [ T.mt4 ] ]
                      [ Field.textarea (Just "Description") proxies.description form
                          [ HP.placeholder "Such description. Much wow."
                          , HP.rows 2
                          ]
                      ]
                  ]
              ]

          , HH.div
              [ HP.classes [ T.mt4 ] ]
              [ Field.submit "Add resource" submitting ]
          ]
      ]
    where handler = Just <<< F.injAction <<< HandleDropdown
          listToItem { id, title } = DDItem { value: id, label: title }
          ddInput = { placeholder: "Choose a list", items: map listToItem lists }
