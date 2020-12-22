module Listasio.Page.CreateList where

import Prelude

import Component.HOC.Connect as Connect
import Control.Monad.Reader (class MonadAsk)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
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
import Listasio.Component.HTML.Header (header)
import Listasio.Component.HTML.Utils (whenElem)
import Listasio.Data.List (List)
import Listasio.Data.Profile (Profile)
import Listasio.Data.Route (Route(..))
import Listasio.Env (UserEnv)
import Listasio.Form.Field as Field
import Listasio.Form.Validation as V
import Tailwind as T
import Web.Event.Event as Event

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
       mbCreatedList <- createList newList
       case mbCreatedList of
            Just _ -> navigate Dashboard
            Nothing -> void $ H.query F._formless unit $ F.injQuery $ SetCreateError true unit

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { currentUser } =
    HH.div
      [ HP.classes [ T.minHScreen, T.wScreen, T.flex, T.flexCol, T.itemsCenter ] ]
      [ header currentUser Navigate Dashboard -- TODO
      , HH.div
          [ HP.classes [ T.container, T.textCenter, T.mt10 ] ]
          [ HH.text "create list" ]
      , HH.slot F._formless unit formComponent unit (Just <<< HandleCreateForm)
      ]

newtype CreateListForm r f
  = CreateListForm
  ( r
      ( title :: f V.FormError String String
      , description :: f V.FormError String (Maybe String)
      , tags :: f V.FormError String (Array String)
      )
  )

derive instance newtypeCreateListForm :: Newtype (CreateListForm r f) _

data FormQuery a
  = SetCreateError Boolean a

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
  formInput :: i -> F.Input CreateListForm ( createError :: Boolean ) m
  formInput _ =
    { validators:
        CreateListForm
          { title: V.required >>> V.minLength 3 >>> V.maxLength 50
          , description: V.toOptional $ V.minLength 5 >>> V.maxLength 500
          , tags: F.hoistFn_ (map trim <<< split (Pattern ","))
          }
    , initialInputs: Nothing
    , createError: false
    }

  handleEvent = F.raiseResult

  handleAction = case _ of
    Submit event -> do
      H.liftEffect $ Event.preventDefault event
      eval F.submit
    where
    eval act = F.handleAction handleAction handleEvent act

  handleQuery :: forall a. FormQuery a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    SetCreateError bool a -> do
      H.modify_ _ { createError = bool }
      pure (Just a)

  proxies = F.mkSProxies (F.FormProxy :: _ CreateListForm)

  renderLogin { form, createError, submitting } =
    HH.form
      [ HE.onSubmit \ev -> Just $ F.injAction $ Submit ev ]
      [ whenElem createError \_ ->
          HH.div
            []
            [ HH.text "Failed to create form" ]
      , HH.fieldset_
          [ Field.input proxies.title form
              [ HP.placeholder "Title"
              , HP.type_ HP.InputText
              ]
          -- TODO: use textarea
          , Field.input proxies.description form
              [ HP.placeholder "Description"
              , HP.type_ HP.InputText
              ]
          , Field.input proxies.tags form
              [ HP.placeholder "Tags (comman separated)"
              , HP.type_ HP.InputText
              ]
          , Field.submit "Create list" submitting
          ]
      ]
