module Example.App.UI.Typeahead where

import Prelude

import Data.Array (difference, filter, length, (:), (!!))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Example.App.UI.Dropdown as Dropdown
import Example.App.UI.Element (class_)
import Example.App.Validation (class ToText, toText)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select as Select
import Select.Setters as Setters

type Slot f item =
  H.Slot (Select.Query (Query item) ()) (Message f item)

_typeahead = SProxy :: SProxy "typeahead"
_typeaheadSingle = SProxy :: SProxy "typeaheadSingle"
_typeaheadMulti = SProxy :: SProxy "typeaheadMulti"

data Query item a
  = GetAvailableItems (Array item -> a)
  | Clear a

getAvailableItems :: forall item a. (Array item -> a) -> Select.Query (Query item) () a
getAvailableItems = Select.Query <<< GetAvailableItems

clear :: forall item. Select.Query (Query item) () Unit
clear = Select.Query (Clear unit)

data Action item
  = Remove item 

remove :: forall item. item -> Select.Action (Action item)
remove = Select.Action <<< Remove 

type State f item =
  ( items :: Array item
  , available :: Array item
  , selected :: f item
  , placeholder :: String
  )

type Input item =
  { items  :: Array item
  , placeholder :: String
  }

input :: forall f item. Monoid (f item) => Input item -> Select.Input (State f item)
input { items, placeholder } =
  { inputType: Select.Text
  , search: Nothing
  , debounceTime: Nothing
  , getItemCount: length <<< _.items
  , selected: mempty
  , available: items
  , items
  , placeholder
  }

data Message f item
  = SelectionsChanged (f item)

----------
-- Premade

single
  :: ∀ item m
   . MonadAff m
  => ToText item
  => Eq item
  => Semigroup item
  => Select.Spec (State Maybe item) (Query item) (Action item) () (Message Maybe item) m
single = spec' (\i av -> const (av !! i)) (const $ const Nothing) filter' render
  where
  filter' items Nothing = items
  filter' items (Just item) = filter (_ == item) items

  render st = case st.selected of
    Just item ->
      HH.div
        [ if st.visibility == Select.On 
            then class_ "dropdown is-active" 
            else class_ "dropdown is-flex" ]
        [ Dropdown.toggle 
            [ HE.onClick \_ -> Just $ remove item ] 
            st
        , Dropdown.menu st
        ]
    Nothing ->
      HH.div
        [ if st.visibility == Select.On 
            then class_ "dropdown is-flex is-active" 
            else class_ "dropdown is-flex" ]
        [ HH.input
          ( Setters.setInputProps
              [ HP.placeholder st.placeholder
              , HP.value st.search
              , class_ "input"
              ]
          )
        , Dropdown.menu st
        ]

multi
  :: ∀ item m
   . MonadAff m
  => ToText item
  => Eq item
  => Select.Spec (State Array item) (Query item) (Action item) () (Message Array item) m
multi = spec' selectByIndex (filter <<< (/=)) difference render
  where
  selectByIndex ix available selected = case available !! ix of
    Nothing -> selected
    Just item -> item : selected

  render st =
    HH.div_
      [ HH.div
        [ if length st.selected > 0 
            then class_ "panel is-marginless" 
            else class_ "panel is-hidden" 
        ]
        (st.selected <#> \i ->
          HH.div
            [ class_ "panel-block has-background-white"
            , HE.onClick \_ -> Just $ remove i
            ]
            [ HH.text $ toText i ]
        )
      , HH.div
        [ if st.visibility == Select.On 
            then class_ "dropdown is-flex is-active" 
            else class_ "dropdown is-flex" 
        ]
        [ HH.input
            (Setters.setInputProps 
              [ class_ "input"
              , HP.placeholder st.placeholder
              , HP.value st.search 
              ]
            )
        , Dropdown.menu st
        ]
      ]

----------
-- Base component

spec'
  :: ∀ item f m
   . MonadAff m
  => Functor f
  => Monoid (f item)
  => ToText item
  => Eq item
  => (Int -> Array item -> f item -> f item)
  -> (item -> f item -> f item)
  -> (Array item -> f item -> Array item)
  -> (Select.State (State f item) 
       -> H.ComponentHTML (Select.Action (Action item)) () m
     )
  -> Select.Spec (State f item) (Query item) (Action item) () (Message f item) m
spec' select' remove' filter' render' = Select.defaultSpec
  { render = render' 
  , handleMessage = handleMessage 
  , handleQuery = handleQuery
  , handleAction = handleAction
  }
  where
  handleMessage = case _ of
    Select.Searched string -> do
      st <- H.get
      let items = filter (String.contains (String.Pattern string) <<< toText) st.items
      H.modify_ _ { available = filter' items st.selected }

    Select.Selected ix -> do
      st <- H.get
      let selected' = select' ix st.available st.selected
      H.modify_ _ 
        { selected = selected'
        , available = filter' st.available selected'
        , visibility = Select.Off
        }
      H.raise $ SelectionsChanged selected'

    _ -> pure unit

  handleQuery :: forall a. Query item a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    Clear a -> do
      st <- H.modify \st -> st { selected = mempty :: f item, available = st.items, search = "" }
      H.raise (SelectionsChanged st.selected)
      pure (Just a)

    GetAvailableItems f -> do
      st <- H.get
      pure $ Just $ f st.available

  handleAction = case _ of
    Remove item -> do
      st <- H.get
      let selected' = remove' item st.selected
      H.modify_ _
        { selected = selected'
        , available = filter' st.items selected' 
        }
      H.raise (SelectionsChanged selected')
