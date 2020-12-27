module Listasio.Component.HTML.CreateResource where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Component.HTML.Dropdown as DD
import Listasio.Component.HTML.Utils (whenElem)
import Listasio.Data.List (ListWithIdAndUser)
import Listasio.Data.Resource (ResourceRep)
import Listasio.Form.Field as Field
import Listasio.Form.Validation (class ToText)
import Listasio.Form.Validation as V
import Select as Select
import Tailwind as T
import Web.Event.Event as Event

newtype DDItem = DDItem { label :: String, value :: String }

derive instance eqDDItem :: Eq DDItem
derive instance newtypeDDItem :: Newtype DDItem _

instance toTextDDItem :: ToText DDItem where
  toText = _.label <<< unwrap

type Slot
  = F.Slot CreateResourceForm FormQuery ChildSlots ResourceWithList Unit

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

type Input = { lists :: Array ListWithIdAndUser  }

type ResourceWithList = { | ResourceRep ( list :: String ) }

type ChildSlots = ( dropdown :: DD.Slot DDItem Unit )

formComponent ::
  forall m.
  MonadAff m =>
  F.Component CreateResourceForm FormQuery ChildSlots Input ResourceWithList m
formComponent =
  F.component formInput
    $ F.defaultSpec
        { render = renderCreateResource
        , handleEvent = handleEvent
        , handleQuery = handleQuery
        , handleAction = handleAction
        }
  where
  formInput :: Input -> F.Input CreateResourceForm ( createError :: Boolean, lists :: Array ListWithIdAndUser ) m
  formInput { lists } =
    { validators:
        CreateResourceForm
          { title: V.required >>> V.minLength 3 >>> V.maxLength 50
          , url: V.required >>> V.minLength 5 >>> V.maxLength 500
          , description:  V.toOptional $ V.minLength 5 >>> V.maxLength 500
          , list: V.requiredFromOptional $ F.noValidation
          }
    , initialInputs: Nothing
    , createError: false
    , lists
    }

  handleEvent = F.raiseResult

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

    Reset -> do
      void $ H.query DD._dropdown unit DD.clear
      eval F.resetAll

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

  renderCreateResource { form, createError, lists, submitting, dirty } =
    HH.form
      [ HP.classes
          [ T.p6
          , T.m6
          , T.border4
          , T.borderGreen300
          ]
      , HE.onSubmit \ev -> Just $ F.injAction $ Submit ev
      ]
      [ whenElem createError \_ ->
          HH.div
            []
            [ HH.text "Failed to add resource" ]
      , HH.fieldset_
          [ Field.input proxies.title form
              [ HP.placeholder "Title"
              , HP.type_ HP.InputText
              ]
          , Field.input proxies.url form
              [ HP.placeholder "URL"
              , HP.type_ HP.InputText
              ]
          , Field.input proxies.description form
              [ HP.placeholder "Description"
              , HP.type_ HP.InputText
              ]
          , HH.slot DD._dropdown unit (Select.component DD.input DD.spec) ddInput handler
          , HH.div
              [ HP.classes [ T.flex, T.justifyEnd ] ]
              [ HH.div
                  [ HP.classes [ T.mr2 ] ]
                  [ Field.cancel "Clear" (submitting || not dirty) $ F.injAction Reset ]
              , Field.submit "Add resource" submitting
              ]
          ]
      ]
    where handler = Just <<< F.injAction <<< HandleDropdown
          listToItem { _id, title } = DDItem { value: _id."$oid", label: title }
          ddInput = { placeholder: "Choose a list", items: map listToItem lists }
