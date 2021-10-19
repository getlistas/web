module Listasio.Page.ListResourcesImport where

import Prelude

import Data.Either (note)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Capability.Resource.Integration (class ManageIntegration)
import Listasio.Capability.Resource.List (class ManageList, getListBySlug)
import Listasio.Capability.Resource.Resource (class ManageResource, importResources)
import Listasio.Component.HTML.CardsAndSidebar as CardsAndSidebar
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Utils (safeHref)
import Listasio.Data.Integration (Integration(..), ListSubscription, RssIntegration)
import Listasio.Data.List (ListWithIdUserAndMeta)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Route (Route(..))
import Listasio.Form.Field as Field
import Listasio.Form.Validation as V
import Listasio.Store as Store
import Network.RemoteData (RemoteData(..), fromEither, isLoading, toMaybe)
import Network.RemoteData as RemoteData
import Slug (Slug)
import Tailwind as T
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent as Mouse

_slot :: Proxy "listResourcesImport"
_slot = Proxy

type Input
  = {user :: Slug, list :: Slug}

data Action
  = Initialize
  | LoadList
  | Receive (Connected (Maybe ProfileWithIdAndEmail) Input)
  | Navigate Route Event
  | HandleForm {urls :: String}

type State
  = { currentUser :: Maybe ProfileWithIdAndEmail
    , list :: RemoteData String ListWithIdUserAndMeta
    , importStatus :: RemoteData String Unit
    , listSlug :: Slug
    , userSlug :: Slug
    }

type Slots = (importForm :: FormSlot)

getRss :: Integration -> Maybe RssIntegration
getRss (RssIntegration a) = Just a
getRss _ = Nothing

getSubscription :: Integration -> Maybe ListSubscription
getSubscription (ListSubscription a) = Just a
getSubscription _ = Nothing

-- TODO: EditList approach -> only fetching when not present
component
  :: forall q o m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => ManageList m
  => ManageIntegration m
  => ManageResource m
  => H.Component q Input o m
component = connect (selectEq _.currentUser) $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , receive = Just <<< Receive
      }
  }
  where
  initialState {context: currentUser, input: {list, user}} =
    { currentUser
    , userSlug: user
    , listSlug: list
    , list: NotAsked
    , importStatus: NotAsked
    }

  handleAction :: Action -> H.HalogenM State Action Slots o m Unit
  handleAction = case _ of
    Initialize ->
      void $ H.fork $ handleAction LoadList

    LoadList -> do
      st <- H.get
      H.modify_ _ {list = Loading}
      list <- RemoteData.fromEither <$> note "Could not get list" <$> getListBySlug {list: st.listSlug, user: st.userSlug}
      H.modify_ _ {list = list}

    Receive {context: currentUser} -> do
      H.modify_ _ {currentUser = currentUser}

    Navigate route e -> navigate_ e route

    HandleForm {urls} -> do
      {importStatus, list} <- H.get
      when (not $ isLoading importStatus) $ for_ (toMaybe list) \{id} -> do
        void $ H.query _form unit $ F.injQuery $ SetSubmitStatus Loading unit
        H.modify_ _ {importStatus = Loading}

        result <- fromEither <$> note "Failed to register" <$> importResources {list: id, payload: urls}

        void $ H.query _form unit $ F.injQuery $ SetSubmitStatus (unit <$ result) unit
        H.modify_ _ {importStatus = unit <$ result}

  render :: State -> H.ComponentHTML Action Slots m
  render {list: mbList, listSlug, userSlug} =
    HH.div [] [ header, content ]

    where
    header =
      HH.div
        [ HP.classes [ T.pt2 ] ]
        [ HH.div
            [ HP.classes [ T.flex, T.itemsCenter, T.justifyBetween ] ]
            [ HH.h1
                [ HP.classes [ T.textGray400, T.mb6, T.text4xl, T.fontBold ] ]
                [ HH.text $ RemoteData.maybe "..." _.title mbList  ]
            , HH.a
                [ safeHref $ PublicList userSlug listSlug
                , HE.onClick $ Navigate (PublicList userSlug listSlug) <<< Mouse.toEvent
                , HP.classes
                    [ T.flex
                    , T.itemsCenter
                    , T.textGray300
                    ]
                ]
                [ Icons.eye [ Icons.classes [ T.w6, T.h6, T.mr2 ] ]
                , HH.text "View list"
                ]
            ]
        ]

    mkLayout cards =
      CardsAndSidebar.layout
        [ { active: false
          , icon: Icons.userCircle
          , label: "Settings"
          , link:
              Just
                { action: Navigate (EditList userSlug listSlug)
                , route: EditList userSlug listSlug
                }
          }
        , { active: false
          , icon: Icons.gridAdd
          , label: "Integrations"
          , link:
              Just
                { action: Navigate (IntegrationsList userSlug listSlug)
                , route: IntegrationsList userSlug listSlug
                }
          }
        , { active: true
          , icon: Icons.documentAdd
          , label: "Import"
          , link: Nothing
          }
        ]
        cards

    content =
      case mbList of
        Success _ ->
          mkLayout
            [ { cta: Nothing
              , title: "Import from OneTab"
              , description: Nothing
              , content:
                  HH.div
                    []
                    [ HH.slot _form unit formComponent unit HandleForm ]
              }
            , { cta: Nothing
              , title: "More ways to import resources comming soon"
              , description: Nothing
              , content:
                  HH.div
                    []
                    [ HH.p
                        [ HP.classes [ T.textGray400 ] ]
                        [ HH.text "If you have an integration in mind let as know on Twitter "
                        , HH.a
                            [ HP.href "https://twitter.com/getlistas"
                            , HP.target "_blank"
                            , HP.rel "noreferrer noopener nofollow"
                            , HP.classes
                                [ T.textKiwi
                                , T.hoverUnderline
                                , T.focusOutlineNone
                                , T.focusUnderline
                                ]
                            ]
                            [ HH.text "@getlistas" ]
                        ]
                    ]
              }
            ]

        -- TODO: better message
        Failure msg ->
          mkLayout
            [ { cta: Nothing
              , content: HH.div [ HP.classes [ T.textManzana ] ] [ HH.text msg ]
              , title: "RSS feed subscriptions"
              , description: Nothing
              }
            , { cta: Nothing
              , content: HH.div [ HP.classes [ T.textManzana ] ] [ HH.text msg ]
              , title: "Following lists"
              , description: Nothing
              }
            ]

        -- TODO: better message
        _ ->
          mkLayout
            [ { cta: Nothing
              , content: HH.div [ HP.classes [ T.textGray400 ] ] [ HH.text "Loading ..." ]
              , title: "RSS feed subscriptions"
              , description: Nothing
              }
            , { cta: Nothing
              , content: HH.div [ HP.classes [ T.textGray400 ] ] [ HH.text "Loading ..." ]
              , title: "Following lists"
              , description: Nothing
              }
            ]

type FormSlot
  = F.Slot Form FormQuery () {urls:: String} Unit

data FormQuery a
  = SetSubmitStatus (RemoteData String Unit) a

derive instance functorFormQuery :: Functor FormQuery

newtype Form (r :: Row Type -> Type) f = Form (r (FormRow f))
derive instance Newtype (Form r f) _

type FormRow :: (Type -> Type -> Type -> Type) -> Row Type
type FormRow f = ( urls :: f V.FormError String String )

_form = Proxy :: Proxy "importForm"

data FormAction
  = Submit Event

formComponent :: forall m slots.
  MonadAff m =>
  F.Component Form FormQuery slots Unit {urls :: String} m
formComponent =
  F.component formInput
    $ F.defaultSpec
        { render = renderForm
        , handleEvent = handleEvent
        , handleQuery = handleQuery
        , handleAction = handleAction
        }
  where
  formInput :: Unit -> F.Input Form ( status :: RemoteData String Unit )  m
  formInput _ =
    { validators: Form {urls: V.required}
    , initialInputs: Nothing
    , status: NotAsked
    }

  handleEvent = F.raiseResult

  handleAction = case _ of
    Submit event -> do
      H.liftEffect $ Event.preventDefault event
      { status } <- H.get
      when (not $ isLoading status) do eval F.submit

    where
    eval act = F.handleAction handleAction handleEvent act

  handleQuery :: forall a. FormQuery a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    SetSubmitStatus status a -> do
      H.modify_ _ {status = status}
      pure (Just a)

  renderForm {form, submitting} =
    HH.form
      [ HE.onSubmit $ F.injAction <<< Submit
      , HP.noValidate true
      ]
      [ HH.fieldset_
          [ urls
          , HH.div
              [ HP.classes [ T.mt4 ] ]
              [ Field.submit "Save" submitting ]
          ]
      ]
    where
    proxies = F.mkSProxies (Proxy :: Proxy Form)

    urls =
      Field.textarea proxies.urls form $ Field.textareaDefaultProps
        { label = Just "Urls"
        , id = Just "urls"
        , placeholder = Just "https://blog.com/post-1\nhttps://blog.com/post-2"
        , rows = Just 4
        , message = Just "One link per line"
        }

