module Listasio.Page.CreateList where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Filterable (filter)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String as String
import Data.String.Common (split, trim)
import Data.String.Pattern (Pattern(..))
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Capability.Navigate (class Navigate, navigate, navigate_)
import Listasio.Capability.Resource.List (class ManageList, createList)
import Listasio.Component.HTML.Layout as Layout
import Listasio.Component.HTML.Utils (safeHref, whenElem)
import Listasio.Data.List (List)
import Listasio.Data.Profile (Profile)
import Listasio.Data.Route (Route(..))
import Listasio.Env (UserEnv)
import Listasio.Form.Field as Field
import Listasio.Form.Validation ((<?>))
import Listasio.Form.Validation as V
import Network.RemoteData (RemoteData(..), isFailure, isLoading)
import Tailwind as T
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent (toEvent)

data Action
  = Initialize
  | Receive { currentUser :: Maybe Profile }
  | HandleCreateForm List
  | Navigate Route Event.Event

type State = {currentUser :: Maybe Profile}

type ChildSlots
  = ( formless :: F.Slot CreateListForm FormQuery () List Unit )

component
  :: forall q o m r
   . MonadAff m
  => MonadAsk { userEnv :: UserEnv | r } m
  => ManageList m
  => Navigate m
  => H.Component HH.HTML q {} o m
component = Connect.component $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      , initialize = Just Initialize
      }
  }
  where
  initialState { currentUser } = { currentUser }

  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> pure unit

    Receive { currentUser } ->
      H.modify_ _ { currentUser = currentUser }

    Navigate route e -> navigate_ e route

    HandleCreateForm newList -> do
      void $ H.query F._formless unit $ F.injQuery $ SetCreateStatus Loading unit

      mbCreatedList <- createList newList

      case mbCreatedList of
        Just _ -> do
           void $ H.query F._formless unit $ F.injQuery $ SetCreateStatus (Success unit) unit
           navigate Dashboard
        Nothing ->
          void $ H.query F._formless unit $ F.injQuery $ SetCreateStatus (Failure "Could not create list") unit

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { currentUser } =
    Layout.dashboard
      currentUser
      Navigate
      Nothing
      $ HH.div
          []
          [ title
          , HH.slot F._formless unit formComponent unit (Just <<< HandleCreateForm)
          ]
    where
    title =
      HH.div
        [ HP.classes [ T.flex, T.itemsCenter, T.textGray400, T.mb6, T.text4xl, T.fontBold  ] ]
        [ HH.a
            [ safeHref Dashboard
            , HE.onClick \e -> Just $ Navigate Dashboard $ toEvent e
            , HP.classes [ T.textGray200, T.mr8 ]
            ]
            [ HH.text "ᐸ" ]
        , HH.text "Create List"
        ]

newtype CreateListForm r f
  = CreateListForm
  ( r
      ( title :: f V.FormError String String
      , description :: f V.FormError String (Maybe String)
      , tags :: f V.FormError String (Array String)
      , is_public :: f V.FormError Boolean Boolean
      )
  )

derive instance newtypeCreateListForm :: Newtype (CreateListForm r f) _

data FormQuery a
  = SetCreateStatus (RemoteData String Unit) a

derive instance functorFormQuery :: Functor FormQuery

data FormAction
  = Submit Event.Event

formComponent ::
  forall i slots m.
  MonadAff m =>
  F.Component CreateListForm FormQuery slots i List m
formComponent =
  F.component formInput
    $ F.defaultSpec
        { render = renderLogin
        , handleEvent = handleEvent
        , handleQuery = handleQuery
        , handleAction = handleAction
        }
  where
  formInput :: i -> F.Input CreateListForm ( status :: RemoteData String Unit ) m
  formInput _ =
    { validators:
        CreateListForm
          { title: V.required >>> V.maxLength 100
          , description: V.toOptional $ V.maxLength 500
          , tags: V.maxLengthArr 4
                    <<< F.hoistFn_ (filter (not <<< String.null) <<< map trim <<< split (Pattern ","))
                    <?> V.WithMsg "Cannot have more than 4 tags"
          , is_public: F.noValidation
          }
    , initialInputs: Nothing
    , status: NotAsked
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

  proxies = F.mkSProxies (F.FormProxy :: _ CreateListForm)

  renderLogin { form, status, submitting } =
    HH.form
      [ HE.onSubmit \ev -> Just $ F.injAction $ Submit ev ]
      [ HH.fieldset_
          [ Field.input (Just "Title") proxies.title form
              [ HP.placeholder "YouTube Videos"
              , HP.type_ HP.InputText
              ]
          , HH.div
              [ HP.classes [ T.mt4 ] ]
              [ Field.input (Just "Description") proxies.description form
                  [ HP.placeholder "Videos to watch all night long"
                  , HP.type_ HP.InputText
                  ]
              ]
          , HH.div
              [ HP.classes [ T.mt4 ] ]
              [ Field.input (Just "Tags") proxies.tags form
                  [ HP.placeholder "videos,chill"
                  , HP.type_ HP.InputText
                  ]
              ]
          , HH.label
              [ HP.classes [ T.flex, T.itemsCenter, T.my4, T.cursorPointer ] ]
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
                [ HP.classes [ T.textRed500, T.my4 ] ]
                [ HH.text "Could not create list :(" ]

          , Field.submit "Create list" (submitting || isLoading status)
          ]
      ]
