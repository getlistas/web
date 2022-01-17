module Listasio.Page.ListResourcesImport where

import Prelude

import Data.Either (Either(..), note)
import Data.Foldable (for_)
import Data.Lens (_Just, _Left, preview)
import Data.Maybe (Maybe(..))
import Data.String (null)
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
import Network.RemoteData (RemoteData(..), fromEither, isLoading, isSuccess, toMaybe)
import Network.RemoteData as RemoteData
import Slug (Slug)
import Tailwind as T
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)
import Web.UIEvent.MouseEvent as Mouse

_slot :: Proxy "listResourcesImport"
_slot = Proxy

type Slot = forall query. H.Slot query Output Input

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f = ( urls :: f String V.FormError String )

type FormInputs = { | Form F.FieldInput }

type Input
  = { user :: Slug, list :: Slug }

type ConnectedInput = Connected (Maybe ProfileWithIdAndEmail) Input

type Output = Void

type FormContext
  = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) ConnectedInput Action

type FormlessAction = F.FormlessAction (Form F.FieldState)

data Action
  = Initialize
  | LoadList
  | Navigate Route Event
  | HandleForm { urls :: String }
  -- Formless actions
  | Receive FormContext
  | Eval FormlessAction

type State
  =
  { context :: FormContext
  , currentUser :: Maybe ProfileWithIdAndEmail
  , list :: RemoteData String ListWithIdUserAndMeta
  , importStatus :: RemoteData String Unit
  , listSlug :: Slug
  , userSlug :: Slug
  }

getRss :: Integration -> Maybe RssIntegration
getRss (RssIntegration a) = Just a
getRss _ = Nothing

getSubscription :: Integration -> Maybe ListSubscription
getSubscription (ListSubscription a) = Just a
getSubscription _ = Nothing

-- TODO: EditList approach -> only fetching when not present
component
  :: forall query m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => ManageList m
  => ManageIntegration m
  => ManageResource m
  => H.Component query Input Output m
component =
  connect (selectEq _.currentUser)
    $ F.formless { liftAction: Eval } mempty
    $ H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { initialize = Just Initialize
        , receive = Just <<< Receive
        , handleAction = handleAction
        , handleQuery = handleQuery
        }
    }
  where
  initialState context =
    { context
    , currentUser: context.input.context
    , userSlug: context.input.input.user
    , listSlug: context.input.input.list
    , list: NotAsked
    , importStatus: NotAsked
    }

  handleAction :: Action -> H.HalogenM _ _ _ _ m Unit
  handleAction = case _ of
    Initialize ->
      void $ H.fork $ handleAction LoadList

    LoadList -> do
      st <- H.get
      H.modify_ _ { list = Loading }
      list <- RemoteData.fromEither <$> note "Could not get list" <$> getListBySlug { list: st.listSlug, user: st.userSlug }
      H.modify_ _ { list = list }

    Navigate route e -> navigate_ e route

    HandleForm { urls } -> do
      { importStatus, list, context } <- H.get
      when (not (isLoading importStatus) && not (null context.fields.urls.value)) $ for_ (toMaybe list) \{ id } -> do
        H.modify_ _ { importStatus = Loading }
        result <- fromEither <$> note "Failed to register" <$> importResources { list: id, payload: urls }
        H.modify_ _ { importStatus = unit <$ result }

        when (isSuccess result) do handleAction context.formActions.reset

    -- Formless actions

    Receive context -> do
      H.modify_ _ { context = context, currentUser = context.input.context }

    Eval action -> F.eval action

  handleQuery :: forall a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ m (Maybe a)
  handleQuery =
    -- TODO: remove nullcheck in HandleForm and introduce this validation instead
    -- F.handleSubmitValidate (handleAction <<< HandleForm) F.validate { urls: V.required }
    F.handleSubmitValidate (handleAction <<< HandleForm) F.validate { urls: Right }

  render :: State -> H.ComponentHTML Action () m
  render { importStatus, list: mbList, listSlug, userSlug, context: { formActions, fields, actions } } =
    HH.div [] [ header, content ]

    where
    header =
      HH.div
        [ HP.classes [ T.pt2 ] ]
        [ HH.div
            [ HP.classes [ T.flex, T.itemsCenter, T.justifyBetween ] ]
            [ HH.h1
                [ HP.classes [ T.textGray400, T.mb6, T.text4xl, T.fontBold ] ]
                [ HH.text $ RemoteData.maybe "..." _.title mbList ]
            , HH.a
                [ safeHref $ PublicList userSlug listSlug
                , HE.onClick $ Navigate (PublicList userSlug listSlug) <<< Mouse.toEvent
                , HP.classes
                    [ T.flex
                    , T.itemsCenter
                    , T.textGray300
                    , T.flexShrink0
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
              , title: "Bulk import"
              , description: Nothing
              , content:
                  HH.div
                    []
                    [ HH.div
                        [ HP.classes [ T.mb4 ] ]
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
                    , form
                    ]
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
    form =
      HH.form
        [ HE.onSubmit formActions.handleSubmit
        , HP.noValidate true
        ]
        [ HH.fieldset_
            [ urlsField
            , HH.div
                [ HP.classes [ T.mt4 ] ]
                [ Field.submit "Save" (isSuccess importStatus) ]
            ]
        ]

    urlsField =
      Field.textarea fields.urls actions.urls $ Field.textareaDefaultProps
        { label = Just "Urls"
        , id = Just "urls"
        , placeholder = Just "https://blog.com/post-1\nhttps://blog.com/post-2"
        , rows = Just 4
        , message = Just $ case importStatus, preview (_Just <<< _Left) fields.urls.result of
            Success _, Just _ -> "We are importing your links. It can take a few seconds to process all of them"
            _, _ -> "One link per line. Duplicated links will not be imported."
        , disabled = isLoading importStatus
        }
