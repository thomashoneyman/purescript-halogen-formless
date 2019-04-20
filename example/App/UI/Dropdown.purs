module Example.App.UI.Dropdown where

import Prelude

import DOM.HTML.Indexed (HTMLbutton)
import Data.Array (difference, mapWithIndex, length, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Traversable (for_)
import Effect.Aff.Class (class MonadAff)
import Example.App.UI.Element (class_)
import Example.App.UI.Element as UI
import Example.App.Validation (class ToText, toText)
import Halogen as H
import Halogen.HTML as HH
import Select as Select
import Select.Setters as Setters

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
  :: forall item m
   . MonadAff m 
  => ToText item
  => Eq item
  => Select.Spec (State item) Query Void () (Message item) m
spec = Select.defaultSpec
  { render = render 
  , handleQuery = handleQuery 
  , handleMessage = handleMessage
  }
  where
  render st =
    HH.div
      [ if st.visibility == Select.On then class_ "dropdown is-active" else class_ "dropdown" ]
      [ toggle [] st, menu st ]

  handleQuery :: forall a. Query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    Clear a -> do
      H.modify_ \st -> st { selected = Nothing, available = st.items }
      H.raise Cleared
      pure (Just a)

  handleMessage = case _ of
    Select.Selected ix -> do
      st <- H.get
      let mbItem = st.available !! ix
      for_ mbItem \item -> do
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
  [ class_ "dropdown-trigger" ]
  [ UI.button
    ( Setters.setToggleProps props )
    [ HH.text $ fromMaybe st.placeholder (toText <$> st.selected) ]
  ]

menu
  :: forall item st act ps m
   . ToText item
  => Select.State (available :: Array item | st)
  -> H.ComponentHTML (Select.Action act) ps m
menu st =
  HH.div
  [ class_ "dropdown-menu" ]
  [ if st.visibility == Select.Off then HH.text "" else
    HH.div
      (Setters.setContainerProps [ class_ "dropdown-content" ])
      (mapWithIndex 
        (\ix item ->
          HH.span
            (Setters.setItemProps ix case Just ix == st.highlightedIndex of
              true -> 
                [ class_ "dropdown-item has-background-link has-text-white-bis" ]
              _ -> 
                [ class_ "dropdown-item" ]
            )
            [ HH.text (toText item) ]
        )
        st.available
      )
  ]

