module Listasio.Component.HTML.Dropdown where

import Prelude

import DOM.HTML.Indexed (HTMLbutton)
import Data.Array (difference, mapWithIndex, length, (!!))
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Symbol (SProxy(..))
import Data.Traversable (for_)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Listasio.Component.HTML.Utils (cx)
import Listasio.Form.Validation (class ToText, toText)
import Select as Select
import Select.Setters as Setters
import Tailwind as T

type Slot item =
  H.Slot (Select.Query Query ()) (Message item)

_dropdown = SProxy :: SProxy "dropdown"

data Query a
  = Clear a

clear :: Select.Query Query () Unit
clear = Select.Query (H.tell Clear)

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
input { items, placeholder } =
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
  :: forall item m i
   . MonadAff m
  => ToText item
  => Eq item
  => Select.Spec (State item) Query Void () i (Message item) m
spec = Select.defaultSpec
  { render = render
  , handleQuery = handleQuery
  , handleEvent = handleEvent
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

  handleQuery :: forall a. Query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    Clear a -> do
      H.modify_ \st -> st { selected = Nothing, available = st.items }
      H.raise Cleared
      pure (Just a)

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
  :: forall item act ps m st
   . ToText item
  => Array (HH.IProp HTMLbutton (Select.Action act))
  -> Select.State (placeholder :: String, selected :: Maybe item | st)
  -> H.ComponentHTML (Select.Action act) ps m
toggle props st =
  HH.div
    []
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
    ]

menu
  :: forall item st act ps m
   . ToText item
  => Select.State (available :: Array item | st)
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
  isOn = st.visibility == Select.On
