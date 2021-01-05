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
import Halogen.HTML.Core as HC
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
          [ cx (HC.ClassName "dropdown is-active") (st.visibility == Select.On) -- TODO ???
          , cx (HC.ClassName "dropdown") (st.visibility /= Select.On)           -- TODO ???
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
  :: forall item act ps m r
   . ToText item
  => Array (HH.IProp HTMLbutton (Select.Action act))
  -> { placeholder :: String, selected :: Maybe item | r }
  -> H.ComponentHTML (Select.Action act) ps m
toggle props st =
  HH.div
    [ HP.classes [ T.my4 ] ]
    [ HH.button
      ( Setters.setToggleProps props
      <> [ HP.classes
             [ T.appearanceNone
             , T.borderNone
             , T.wFull
             , T.py2
             , T.px4
             , T.bgWhite
             , cx T.textGray400 $ isJust st.selected
             , cx T.textGray300 $ isNothing st.selected
             , T.placeholderGray400
             , T.roundedMd
             , T.textBase
             , T.focusOutlineNone
             , T.focusRing2
             , T.focusRingOffset2
             , T.focusRingOffsetGray100
             , T.focusRingKiwi
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
      [ HC.ClassName "dropdown-menu" -- TODO ???
      , cx T.mb2 $ st.visibility == Select.On
      ]
  ]
  [ if st.visibility == Select.Off
      then HH.text ""
      else
        HH.div
          (Setters.setContainerProps [ HP.classes [ T.flex, T.flexCol ] ])
          (mapWithIndex
            (\ix item ->
              HH.span
                ( Setters.setItemProps
                    ix
                    [ HP.classes
                        [ T.cursorPointer
                        , T.my1
                        , T.p2
                        , T.roundedMd
                        , cx T.textWhite $ Just ix == st.highlightedIndex
                        , cx T.textGray400 $ Just ix /= st.highlightedIndex
                        , cx T.bgKiwi $ Just ix == st.highlightedIndex
                        ]
                    ]
                )
                [ HH.text (toText item) ]
            )
            st.available
          )
  ]

