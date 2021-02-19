module Listasio.Component.HTML.ListForm where

import Prelude

import Data.Filterable (filter)
import Data.Maybe (Maybe(..), fromMaybe)
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
import Listasio.Data.List (CreateListFields, ListWithIdUserAndMeta)
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
  = SetCreateStatus (RemoteData String Unit) a

derive instance functorFormQuery :: Functor FormQuery

data FormAction
  = Submit Event.Event

type FormInput
  = { list :: Maybe ListWithIdUserAndMeta }

formComponent ::
  forall slots m.
  MonadAff m =>
  F.Component ListForm FormQuery slots FormInput CreateListFields m
formComponent =
  F.component formInput
    $ F.defaultSpec
        { render = renderLogin
        , handleEvent = handleEvent
        , handleQuery = handleQuery
        , handleAction = handleAction
        }
  where
  formInput :: FormInput -> F.Input ListForm ( status :: RemoteData String Unit ) m
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
    , status: NotAsked
    }

  initialInputs { title, description, tags, is_public } = F.wrapInputFields
    { title
    , description: fromMaybe "" description
    , tags: joinWith ", " tags
    , is_public
    }

  handleEvent = F.raiseResult

  handleAction = case _ of
    Submit event -> do
      { status } <- H.get
      when (not $ isLoading status) do
        H.liftEffect $ Event.preventDefault event
        eval F.submit
    where
    eval act = F.handleAction handleAction handleEvent act

  handleQuery :: forall a. FormQuery a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    SetCreateStatus status a -> do
      H.modify_ _ { status = status }
      pure (Just a)

  proxies = F.mkSProxies (F.FormProxy :: _ ListForm)

  renderLogin { form, status, submitting } =
    HH.form
      [ HE.onSubmit \ev -> Just $ F.injAction $ Submit ev ]
      [ HH.fieldset_
          [ Field.input (Just "Title") proxies.title form
              [ HP.placeholder "YouTube Videos"
              , HP.type_ HP.InputText
              ]
          , HH.div
              [ HP.classes [ T.mt6 ] ]
              [ Field.textarea (Just "Description") proxies.description form
                  [ HP.placeholder "Videos to watch all night long"
                  , HP.rows 3
                  ]
              ]
          , HH.div
              [ HP.classes [ T.mt6 ] ]
              [ Field.input (Just "Tags") proxies.tags form
                  [ HP.placeholder "videos,chill"
                  , HP.type_ HP.InputText
                  ]
              ]
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

          , Field.submit "Create" (submitting || isLoading status)
          ]
      ]
