module Listasio.Component.HTML.Dropdown where

import Prelude

import DOM.HTML.Indexed (HTMLbutton)
import Data.Array (difference, length, mapWithIndex, null, (!!))
import Data.Array as A
import Data.Filterable (filter)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Traversable (for_)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Listasio.Component.HTML.Icons as Icons
import Listasio.Component.HTML.Utils (cx, maybeElem)
import Listasio.Form.Validation (class ToText, toText)
import Select as Select
import Select.Setters as Setters
import Tailwind as T
import Type.Proxy (Proxy(..))

type Slot item =
  H.Slot (Select.Query (Query item) ()) (Message item)

_dropdown = Proxy :: Proxy "dropdown"

data Action item
  = Receive (Input item)
  | ClearSelection

data Query item a
  = Clear a
  | Select item a

clear :: forall item. Select.Query (Query item) () Unit
clear = Select.Query $ H.mkTell Clear

select :: forall item. item -> Select.Query (Query item) () Unit
select item = Select.Query $ H.mkTell $ Select item

type State item =
  ( selected :: Maybe item
  , available :: Array item
  , items :: Array item
  , placeholder :: String
  )

type Input item =
  { items :: Array item
  , placeholder :: String
  }

input :: forall item. Input item -> Select.Input (State item)
input {items, placeholder} =
  { inputType: Select.Toggle
  , search: Nothing
  , debounceTime: Nothing
  , getItemCount: length <<< _.items
  , selected: Nothing
  , available: items
  , items
  , placeholder
  }

data Message item
  = Selected item
  | Cleared

spec
  :: forall item m
   . MonadAff m
  => ToText item
  => Eq item
  => Select.Spec (State item) (Query item) (Action item) () (Input item) (Message item) m
spec = Select.defaultSpec
  { render = render
  , handleQuery = handleQuery
  , handleEvent = handleEvent
  , handleAction = handleAction
  , receive = Just <<< Receive
  }
  where
  render st =
    HH.div
      [ HP.classes
          [ cx T.bgGray100 $ st.visibility == Select.On
          , cx T.roundedMd $ st.visibility /= Select.On
          , cx T.roundedTMd $ st.visibility == Select.On
          , T.relative
          ]
      ]
      [ toggle [] st
      , menu st
      ]

  handleQuery :: forall a. Query item a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    Clear a -> do
      handleAction ClearSelection
      pure (Just a)

    Select item a -> do
      H.modify_ \st -> st {selected = Just item, available = difference st.items [ item ]}
      H.raise $ Selected item
      pure (Just a)

  handleAction :: Action item -> H.HalogenM _ _ _ _ _ Unit
  handleAction = case _ of
    Receive {items} -> do
      H.modify_ \st -> st {items = items, available = if A.null st.items then items else st.available}

    ClearSelection -> do
      H.modify_ \st -> st {selected = Nothing, available = st.items}
      H.raise Cleared

  handleEvent = case _ of
    Select.Selected ix -> do
      st <- H.get
      for_ (st.available !! ix) \item -> do
        H.modify_ _
          { selected = Just item
          , available = difference st.items [ item ]
          , visibility = Select.Off
          }
        H.raise (Selected item)

    _ -> pure unit

toggle
  :: forall item ps m st
   . ToText item
  => Array (HH.IProp HTMLbutton (Select.Action (Action item)))
  -> Select.State (placeholder :: String, items :: Array item, selected :: Maybe item | st)
  -> H.ComponentHTML (Select.Action (Action item)) ps m
toggle props st =
  HH.div
    [ HP.classes [ T.relative ] ]
    [ HH.button
        ( Setters.setToggleProps props
            <> [ HP.type_ HP.ButtonButton
               , HP.classes
                   [ T.appearanceNone
                   , T.borderNone
                   , T.wFull
                   , T.py2
                   , T.px4
                   , T.bgGray100
                   , cx T.textGray400 $ isJust st.selected
                   , cx T.textGray300 $ isNothing st.selected
                   , T.placeholderGray400
                   , T.roundedMd
                   , T.textBase
                   , T.focusOutlineNone
                   ]
               ]
        )
        [ HH.text $ fromMaybe st.placeholder (toText <$> st.selected) ]
    , maybeElem (filter (const $ A.length st.items > 1) st.selected) \_ ->
        HH.button
          [ HP.classes
              [ T.absolute
              , T.right2
              , T.top2
              , T.bottom2
              , T.textGray400
              , T.hoverTextKiwi
              , T.cursorPointer
              , T.roundedMd
              , T.focusOutlineNone
              , T.focusRing2
              , T.focusRingKiwi
              , T.focusBorderKiwi
              ]
          , HP.type_ HP.ButtonButton
          , HE.onClick $ const $ Select.Action ClearSelection
          ]
          [ Icons.x
              [ Icons.classes [ T.h4, T.w4 ] ]
          ]
    ]

menu
  :: forall item st act ps m
   . ToText item
  => Select.State ( available :: Array item | st )
  -> H.ComponentHTML (Select.Action act) ps m
menu st =
  HH.div
  [ HP.classes
      [ cx T.pt1 isOn
      , cx T.pb2 isOn
      , cx T.px4 isOn
      , cx T.borderT2 isOn
      , cx T.borderWhite isOn
      , cx T.bgWhite isOn
      , cx T.shadowLg isOn
      , T.absolute
      , T.maxH44
      , T.w11d12
      , T.mxAuto
      , T.right0
      , T.left0
      , T.overflowYAuto
      , T.z10
      ]
  ]
  [ if isOn
      then
        HH.div
          (Setters.setContainerProps [ HP.classes [ T.flex, T.flexCol ] ])
          (mapWithIndex
            (\ix item ->
              HH.span
                ( Setters.setItemProps
                    ix
                    [ HP.classes
                        [ T.textCenter
                        , T.cursorPointer
                        , T.py1
                        , T.px2
                        , T.roundedSm
                        , T.textSm
                        , cx T.textWhite $ Just ix == st.highlightedIndex
                        , cx T.textGray400 $ Just ix /= st.highlightedIndex
                        , cx T.bgDurazno $ Just ix == st.highlightedIndex
                        ]
                    ]
                )
                [ HH.text (toText item) ]
            )
            st.available
          )
      else HH.text ""
  ]
  where
  isOn = st.visibility == Select.On && not (null st.available)
