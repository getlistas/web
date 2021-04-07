module Listasio.Component.HTML.CreateResource where

import Prelude

import Control.Alt ((<|>))
import Data.Array (sortWith, find)
import Data.Char.Unicode as Char
import Data.Filterable (filter)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType.Common as MediaType
import Data.Newtype (class Newtype, unwrap)
import Data.String (null, take) as String
import Data.String.CodeUnits (fromCharArray, toCharArray) as String
import Data.Symbol (SProxy(..))
import Data.Traversable (for_, traverse)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Capability.Resource.Resource (class ManageResource, createResource, getMeta)
import Listasio.Component.HTML.Dropdown as DD
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Resource as ResourceComponent
import Listasio.Component.HTML.Utils (maybeElem, whenElem)
import Listasio.Data.ID (ID)
import Listasio.Data.List (ListWithIdUserAndMeta)
import Listasio.Data.Resource (ListResource, Resource)
import Listasio.Data.ResourceMetadata (ResourceMeta)
import Listasio.Form.Field as Field
import Listasio.Form.Validation (class ToText, (<?>))
import Listasio.Form.Validation as V
import Network.RemoteData (RemoteData(..), isFailure, isLoading, isSuccess)
import Select as Select
import Tailwind as T
import Util as Util
import Web.Clipboard.ClipboardEvent as Clipboard
import Web.Event.Event as Event
import Web.HTML.Event.DataTransfer as DataTransfer

type Slot = forall query. H.Slot query Output Unit

_createResource = SProxy :: SProxy "createResource"

data Action
  = HandleFormMessage Resource

type State
  = { lists :: Array ListWithIdUserAndMeta
    , selectedList :: Maybe ID
    , url :: Maybe String
    , title :: Maybe String
    , text :: Maybe String
    }

type Input
  = { lists :: Array ListWithIdUserAndMeta
    , selectedList :: Maybe ID
    , url :: Maybe String
    , title :: Maybe String
    , text :: Maybe String
    }

data Output
  = Created ListResource

type ChildSlots
  = ( formless :: FormSlot )

filterNonAlphanum :: String -> String
filterNonAlphanum =
  String.fromCharArray <<< filter Char.isAlphaNum <<< String.toCharArray

component :: forall query m.
     MonadAff m
  => ManageResource m
  => H.Component HH.HTML query Input Output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      }
  }
  where
  initialState {lists, selectedList, url, title, text} =
    { lists: sortWith (filterNonAlphanum <<< _.title) lists
    , selectedList
    , url
    , title
    , text
    }

  handleAction :: Action -> H.HalogenM State Action ChildSlots Output m Unit
  handleAction = case _ of
    HandleFormMessage newResource -> do
      void $ H.query F._formless unit $ F.injQuery $ SetCreateStatus Loading unit

      mbNewResource <- createResource newResource

      case mbNewResource of
        Just resource -> do
          H.raise $ Created resource
          void $ H.query F._formless unit $ F.injQuery $ SetCreateStatus (Success unit) unit

        Nothing ->
          void $ H.query F._formless unit $ F.injQuery $ SetCreateStatus (Failure "Could not create resource") unit

  render :: State -> HH.ComponentHTML Action ChildSlots m
  render state =
    HH.div
      []
      [ HH.slot F._formless unit formComponent state (Just <<< HandleFormMessage) ]

newtype DDItem = DDItem {label :: String, value :: ID}

derive instance eqDDItem :: Eq DDItem
derive instance newtypeDDItem :: Newtype DDItem _

instance toTextDDItem :: ToText DDItem where
  toText = _.label <<< unwrap

type FormSlot
  = F.Slot CreateResourceForm FormQuery FormChildSlots Resource Unit

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
  = SetCreateStatus (RemoteData String Unit) a

derive instance functorFormQuery :: Functor FormQuery

data FormAction
  = FormInitialize
  | Submit Event.Event
  | HandleDropdown (DD.Message DDItem)
  | FetchMeta String
  | PasteUrl Clipboard.ClipboardEvent

type FormInput
  = { lists :: Array ListWithIdUserAndMeta
    , selectedList :: Maybe ID
    , url :: Maybe String
    , title :: Maybe String
    , text :: Maybe String
    }

type FormChildSlots = ( dropdown :: DD.Slot DDItem Unit )

type FormState =
  ( status :: RemoteData String Unit
  , meta :: RemoteData String ResourceMeta
  , pastedUrl :: Maybe String
  , lists :: Array ListWithIdUserAndMeta
  , selectedList :: Maybe ID
  )

formComponent ::
  forall m.
  MonadAff m =>
  ManageResource m =>
  F.Component CreateResourceForm FormQuery FormChildSlots FormInput Resource m
formComponent = F.component formInput $ F.defaultSpec
  { render = renderCreateResource
  , handleEvent = handleEvent
  , handleQuery = handleQuery
  , handleAction = handleAction
  , initialize = Just FormInitialize
  }
  where
  formInput :: FormInput -> F.Input CreateResourceForm FormState m
  formInput {lists, url, title, text, selectedList} =
    { validators:
        CreateResourceForm
          { title: V.required >>> V.maxLength 150
          , url: V.required >>> V.maxLength 500 -- TODO URL validation ???
          , description:  V.toOptional $ V.maxLength 500
          , thumbnail: F.noValidation
          , list: V.requiredFromOptional F.noValidation
                   <?> V.WithMsg "Please select a list"
          }
    , initialInputs: Just $ initialInputs {url, title, text}
    , status: NotAsked
    , meta: NotAsked
    , lists
    , pastedUrl: url <|> (filter Util.isUrl text)
    , selectedList
    }

  initialInputs  {url, title, text} = F.wrapInputFields
    { url: fromMaybe "" $ url <|> (filter Util.isUrl text)
    , title: fromMaybe "" $ String.take 500 <$> title
    , description: fromMaybe "" $ filter (not <<< Util.isUrl) text
    , thumbnail: Nothing
    , list: Nothing
    }

  handleEvent = F.raiseResult

  handleAction = case _ of
    FormInitialize -> do
      {pastedUrl, form, selectedList, lists} <- H.get

      let mbSelectedItem = listToItem <$> ((\id -> find ((id == _) <<< _.id) lists) =<< selectedList)

      for_ mbSelectedItem \item ->
        void $ H.query DD._dropdown unit (DD.select item)

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
    SetCreateStatus status a -> do
      H.modify_ _ {status = status}
      when (isSuccess status) do
        eval F.resetAll
        void $ H.query DD._dropdown unit DD.clear
      pure $ Just a

    where
    eval act = F.handleAction handleAction handleEvent act

  proxies = F.mkSProxies (F.FormProxy :: _ CreateResourceForm)

  renderCreateResource {form, status, lists, submitting, dirty, meta} =
    HH.form
      [ HE.onSubmit $ Just <<< F.injAction <<< Submit
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
                  , ResourceComponent.resource lists resource
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
              [ HP.classes [ T.grid, T.gridCols1, T.smGridCols5, T.gap4, T.mt4 ] ]
              [ HH.div
                  [ HP.classes[ T.wFull, T.hFull, T.hidden, T.smBlock, T.colSpan2 ] ]
                  [ case meta of
                      Success {thumbnail: Just thumbnail} ->
                        HH.img
                          [ HP.src thumbnail
                          , HP.classes [ T.wFull, T.hFull, T.objectCover, T.roundedLg ]
                          ]

                      Loading ->
                        HH.div
                          [ HP.classes
                              [ T.wFull
                              , T.hFull
                              , T.flex
                              , T.flexCol
                              , T.justifyCenter
                              , T.itemsCenter
                              , T.textGray200
                              , T.bgGray100
                              , T.roundedLg
                              ]
                          ]
                          [ Icons.loader [ Icons.classes [ T.animateSpinSlow, T.h10, T.w10 ] ] ]

                      _ ->
                        HH.div
                          [ HP.classes
                              [ T.wFull
                              , T.hFull
                              , T.flex
                              , T.flexCol
                              , T.justifyCenter
                              , T.itemsCenter
                              , T.textGray200
                              , T.bgGray100
                              , T.roundedLg
                              ]
                          ]
                          [ Icons.photo [ Icons.classes [ T.h20, T.w20 ] ] ]
                  ]
              , HH.div
                  [ HP.classes [ T.colSpan1, T.smColSpan3 ] ]
                  [ HH.div [] [ titleField ]
                  , HH.div [ HP.classes [ T.mt4 ] ] [ description ]
                  ]
              ]

          , whenElem (isFailure status) \_ ->
              HH.div
                [ HP.classes [ T.textRed500, T.my4 ] ]
                [ HH.text "Could not create resource :(" ]

          , HH.div
              [ HP.classes [ T.mt4 ] ]
              [ Field.submit "Add resource" (submitting || isLoading status) ]
          ]
      ]
    where
    handler = Just <<< F.injAction <<< HandleDropdown
    ddInput = {placeholder: "Choose a list", items: map listToItem lists}

    url =
      Field.input proxies.url form $ Field.defaultProps
        { label = Just "Link"
        , id = Just "url"
        , placeholder = Just "https://blog.com/some-blogpost"
        , required = true
        , props = [ HE.onPaste $ Just <<< F.injAction <<< PasteUrl ]
        , message = case meta of
            Loading -> Just "Fetching title and description ..."
            Success {can_resolve} | not can_resolve -> Just "Looks like the link does not exist. Are you sure you got the right one?"
            _ -> Nothing
        }


    titleField =
      Field.input proxies.title form $ Field.defaultProps
        { label = Just "Title"
        , id = Just "title"
        , placeholder = Just "Some blogpost"
        , required = true
        }

    description =
      Field.textarea proxies.description form $ Field.textareaDefaultProps
        { label = Just "Description"
        , id = Just "description"
        , placeholder = Just "Such description. Much wow."
        , props = [HP.rows 3]
        }

  listToItem {id, title} = DDItem {value: id, label: title}
