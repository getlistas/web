module Listasio.Component.HTML.Typed where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Array ((:))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (for_)
import Effect.Aff (Fiber, Milliseconds(..), delay, error, forkAff, killFiber)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Listasio.Component.HTML.Utils (cx)
import Tailwind as T
import Type.Proxy (Proxy(..))

_slot :: Proxy "typed"
_slot = Proxy

type Slot id = forall query. H.Slot query Void id

data TypeStatus
  = Typing Boolean | Deliting | Waiting Int

isWaiting :: TypeStatus -> Boolean
isWaiting (Waiting _) = true
isWaiting _ = false

data Action
  = Initialize
  | Tick
  | Finalize

type State
  = { before :: Array String
    , current :: String
    , after :: Array String
    , status :: TypeStatus
    , display :: String
    , position :: Int
    , tickFiber :: Maybe (Fiber Unit)
    }

type Input
  = {words :: NonEmptyArray String}

component
  :: forall q o m
   . MonadAff m
  => H.Component q Input o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , finalize = Just Finalize
      }
  }
  where
  initialState {words} =
    { before: Array.reverse $ NEA.tail words
    , current: NEA.head words
    , after: []
    , status: Typing false
    , display: ""
    , position: 0
    , tickFiber: Nothing
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Initialize -> do
      {emitter, listener} <- H.liftEffect HS.create
      void $ H.subscribe emitter

      fiber <- H.liftAff $ forkAff $ forever do
        H.liftEffect $ HS.notify listener Tick
        delay $ Milliseconds 60.0

      H.modify_ _ {tickFiber = Just fiber}

    Finalize -> do
      {tickFiber} <- H.get
      for_ tickFiber \fiber ->
        H.liftAff $ killFiber (error "Finalize") fiber


    Tick -> do
      {display, position, status, before, current, after} <- H.get

      case status of
        Waiting 0 -> H.modify_ _ {status = Deliting}

        Waiting n -> H.modify_ _ {status = Waiting (n - 1)}

        Typing false ->  H.modify_ _ {status = Typing true}

        Typing true -> do
          when (display == current) $ H.modify_ _ {status = Waiting 33}

          unless (display == current) do
            let newPosition = position + 1
            H.modify_ _
              { position = newPosition
              , display = String.take newPosition current
              , status = Typing false
              }

        Deliting ->
          case display of
            "" ->
              case NEA.fromArray before of
                -- [a, b] c [d]    ==> [a] b [c, d]
                Just more ->
                  H.modify_ _
                    { before = NEA.init more
                    , current = NEA.last more
                    , after = current : after
                    , position = 0
                    , status = Typing false
                    }

                -- [] a, [b, c, d] ==> [a, b, c] d []
                Nothing -> do
                  let all = NEA.cons' current after -- [a, b, c, d]
                  H.modify_ _
                    { before = NEA.init all
                    , current = NEA.last all
                    , after = []
                    , position = 0
                    , status = Typing false
                    }

            _ -> H.modify_ _ {display = String.take (String.length display - 1) display}

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render {display, status} =
    HH.span
      [ HP.classes
         [ T.typingCursor
         , cx T.idle $ isWaiting status
         ]
      ]
      [ HH.text display ]
