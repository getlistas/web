module Listasio.Component.HTML.CreateResource where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Capability.Resource.Resource (class ManageResource, createResource)
import Listasio.Component.HTML.Dropdown as DD
import Listasio.Component.HTML.Utils (whenElem)
import Listasio.Data.List (ListWithIdAndUser)
import Listasio.Data.Resource (ListResource, Resource)
import Listasio.Form.Field as Field
import Listasio.Form.Validation (class ToText)
import Listasio.Form.Validation as V
import Select as Select
import Tailwind as T
import Web.Event.Event as Event

type Slot = forall query. H.Slot query Output Unit

_createResource = SProxy :: SProxy "createResource"

data Action
  = HandleFormMessage FormMessage

type State
  = { lists :: Array ListWithIdAndUser  }

type Input = { lists :: Array ListWithIdAndUser  }

data Output
  = Created ListResource
  | Cancel

type ChildSlots
  = ( formless :: FormSlot )

component :: forall q m.
     MonadAff m
  => ManageResource m
  => H.Component HH.HTML q Input Output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  initialState = identity

  handleAction :: Action -> H.HalogenM State Action ChildSlots Output m Unit
  handleAction = case _ of
    HandleFormMessage FormCancel -> H.raise Cancel

    HandleFormMessage (FormSubmitted { description, title, url, list }) -> do
      { lists } <- H.get
      mbNewResource <- createResource { description, title, url, list }
      case mbNewResource of
        Just resource -> do
          H.raise $ Created resource
          void $ H.query F._formless unit $ F.injQuery $ SetCreateError false unit

        Nothing -> void $ H.query F._formless unit $ F.injQuery $ SetCreateError true unit

  render :: State -> HH.ComponentHTML Action ChildSlots m
  render { lists } =
    HH.div
      []
      [ HH.slot F._formless unit formComponent { lists, showCancel: true } (Just <<< HandleFormMessage) ]

newtype DDItem = DDItem { label :: String, value :: String }

derive instance eqDDItem :: Eq DDItem
derive instance newtypeDDItem :: Newtype DDItem _

instance toTextDDItem :: ToText DDItem where
  toText = _.label <<< unwrap

data FormMessage
  = FormCancel
  | FormSubmitted Resource

type FormSlot
  = F.Slot CreateResourceForm FormQuery FormChildSlots FormMessage Unit

newtype CreateResourceForm r f
  = CreateResourceForm
  ( r
      ( title :: f V.FormError String String
      , url :: f V.FormError String String
      , description :: f V.FormError String (Maybe String)
      , list :: f V.FormError (Maybe String) String
      )
  )

derive instance newtypeCreateResourceForm :: Newtype (CreateResourceForm r f) _

data FormQuery a
  = SetCreateError Boolean a

derive instance functorFormQuery :: Functor FormQuery

data FormAction
  = Submit Event.Event
  | HandleDropdown (DD.Message DDItem)
  | Reset

type FormInput
  = { lists :: Array ListWithIdAndUser
    , showCancel :: Boolean
    }

type FormChildSlots = ( dropdown :: DD.Slot DDItem Unit )

type FormState =
  ( createError :: Boolean
  , showCancel :: Boolean
  , lists :: Array ListWithIdAndUser
  )

formComponent ::
  forall m.
  MonadAff m =>
  F.Component CreateResourceForm FormQuery FormChildSlots FormInput FormMessage m
formComponent =
  F.component formInput
    $ F.defaultSpec
        { render = renderCreateResource
        , handleEvent = handleEvent
        , handleQuery = handleQuery
        , handleAction = handleAction
        }
  where
  formInput :: FormInput -> F.Input CreateResourceForm FormState m
  formInput { lists, showCancel } =
    { validators:
        CreateResourceForm
          { title: V.required >>> V.minLength 3 >>> V.maxLength 150
          , url: V.required >>> V.minLength 5 >>> V.maxLength 500
          , description:  V.toOptional $ V.minLength 5 >>> V.maxLength 500
          , list: V.requiredFromOptional $ F.noValidation
          }
    , initialInputs: Nothing
    , createError: false
    , showCancel
    , lists
    }

  handleEvent = case _ of
    F.Submitted outputs ->
      H.raise $ FormSubmitted $ F.unwrapOutputFields outputs
    _ -> pure unit

  handleAction = case _ of
    Submit event -> do
      H.liftEffect $ Event.preventDefault event
      eval F.submit

    HandleDropdown msg ->
      case msg of
        DD.Selected (DDItem { value }) ->
          eval $ F.setValidate proxies.list (Just value)

        DD.Cleared ->
          eval $ F.setValidate proxies.list Nothing

    -- TODO: do handle this action while submitting
    Reset -> do
      void $ H.query DD._dropdown unit DD.clear
      eval F.resetAll
      H.raise FormCancel

    where
    eval act = F.handleAction handleAction handleEvent act

  handleQuery :: forall a. FormQuery a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    SetCreateError createError a -> do
      H.modify_ _ { createError = createError }
      when (not createError) do
        eval F.resetAll
        void $ H.query DD._dropdown unit DD.clear
      pure (Just a)

    where
    eval act = F.handleAction handleAction handleEvent act

  proxies = F.mkSProxies (F.FormProxy :: _ CreateResourceForm)

  -- TODO: submitting is only true while the form component is submitting
  --       but the async action actually happens in the parent component
  renderCreateResource { form, createError, lists, submitting, dirty, showCancel } =
    HH.form
      [ HP.classes [ T.relative, T.p6, T.border4, T.borderKiwi, T.roundedMd ]
      , HE.onSubmit \ev -> Just $ F.injAction $ Submit ev
      ]
      [ whenElem createError \_ ->
          HH.div
            []
            [ HH.text "Failed to add resource" ]
      , HH.fieldset
          []
          [ Field.input Nothing proxies.url form
              [ HP.placeholder "https://blog.com/some-article"
              , HP.type_ HP.InputText
              ]
          , HH.div
              [ HP.classes [ T.mt4 ] ]
              [ HH.slot DD._dropdown unit (Select.component DD.input DD.spec) ddInput handler ]
          , HH.div
              [ HP.classes [ T.mt4 ] ]
              [ Field.input Nothing proxies.title form
                  [ HP.placeholder "Title"
                  , HP.type_ HP.InputText
                  ]
              ]
          , HH.div
              [ HP.classes [ T.mt4 ] ]
              [ Field.textarea Nothing proxies.description form
                  [ HP.placeholder "Description"
                  , HP.rows 2
                  ]
              ]
          , HH.div
              [ HP.classes [ T.mt4 ] ]
              [ Field.submit "Add" submitting ]
          , whenElem showCancel \_ ->
              HH.button
                [ HP.classes
                    [ T.mr2
                    , T.pt1
                    , T.px1
                    , T.absolute
                    , T.top1
                    , T.right0
                    , T.fontMono
                    , T.textKiwi
                    , T.fontBold
                    , T.focusOutlineNone
                    , T.focusRing2
                    , T.focusRingKiwi
                    , T.leadingNone
                    ]
                , HP.type_ HP.ButtonReset
                , HE.onClick \_ -> Just $ F.injAction Reset
                ]
                [ HH.text "X" ]
          ]
      ]
    where handler = Just <<< F.injAction <<< HandleDropdown
          listToItem { id, title } = DDItem { value: id, label: title }
          ddInput = { placeholder: "Choose a list", items: map listToItem lists }
