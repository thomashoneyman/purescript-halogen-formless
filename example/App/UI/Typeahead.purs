module Example.App.UI.Typeahead where

import Prelude

import Data.Array (difference, filter, (:))
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect.Aff.Class (class MonadAff)
import Example.App.UI.Dropdown as Dropdown
import Example.App.UI.Element (css)
import Example.App.Validation (class ToText, toText)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select as Select
import Select.Utils.Setters as Setters

data Query item a
  = HandleSelect (Select.Message (Const Void) item) a
  | Remove item a
  | Clear a

type State f item =
  { items :: Array item
  , selected :: f item
  , placeholder :: String
  }

type Input item =
  { items  :: Array item
  , placeholder :: String
  }

data Message f item
  = SelectionsChanged (f item)

type ChildSlot = Unit
type ChildQuery item = Select.Query (Const Void) item

----------
-- Premade

-- TODO: Add removal events. Will require a second render function, probably.

single
  :: ∀ item m
   . MonadAff m
  => ToText item
  => Eq item
  => Semigroup item
  => H.Component HH.HTML (Query item) (Input item) (Message Maybe item) m
single = component' (const <<< Just) (const $ const Nothing) filter' render
  where
  filter' items Nothing = items
  filter' items (Just item) = filter (_ == item) items
  render st selectState = case st.selected of
    Just item ->
      HH.div
      [ if selectState.visibility == Select.On then css "dropdown is-active" else css "dropdown" ]
      [ Dropdown.toggle st
      , Dropdown.menu selectState
      ]
    Nothing ->
      HH.div
      [ if selectState.visibility == Select.On then css "dropdown is-active" else css "dropdown" ]
      [ HH.input
        ( Setters.setInputProps [ HP.placeholder st.placeholder, HP.value selectState.search ] )
      , Dropdown.menu selectState
      ]


multi
  :: ∀ item m
   . MonadAff m
  => ToText item
  => Eq item
  => H.Component HH.HTML (Query item) (Input item) (Message Array item) m
multi = component' ((:)) (filter <<< (/=)) difference render
  where
  render st selectState =
    HH.div
    [ if selectState.visibility == Select.On then css "dropdown is-active" else css "dropdown" ]
    [ HH.div
      [ css "card" ]
      ( map (HH.text <<< toText) st.selected )
    , HH.input
      ( Setters.setInputProps [ css "input", HP.placeholder st.placeholder, HP.value selectState.search ] )
    , Dropdown.menu selectState
    ]

----------
-- Base component

component'
  :: ∀ item f m
   . MonadAff m
  => Functor f
  => Monoid (f item)
  => ToText item
  => Eq item
  => (item -> f item -> f item)
  -> (item -> f item -> f item)
  -> (Array item -> f item -> Array item)
  -> (State f item -> Select.State item -> Select.ComponentHTML (Const Void) item)
  -> H.Component HH.HTML (Query item) (Input item) (Message f item) m
component' select' remove' filter' render' =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: Input item -> State f item
  initialState { items, placeholder } =
    { items
    , placeholder
    , selected: mempty
    }

  render :: State f item -> H.ParentHTML (Query item) (ChildQuery item) ChildSlot m
  render st =
    HH.slot unit Select.component selectInput (HE.input HandleSelect)

    where

    selectInput =
      { inputType: Select.TextInput
      , items: st.items
      , initialSearch: Nothing
      , debounceTime: Nothing
      , render: render' st
      }

  eval :: Query item ~> H.ParentDSL (State f item) (Query item) (ChildQuery item) ChildSlot (Message f item) m
  eval = case _ of
    Clear next -> do
      st <- H.modify _ { selected = mempty :: f item }
      _ <- H.query unit $ Select.replaceItems st.items
      H.raise (SelectionsChanged st.selected)
      pure next

    Remove item next -> do
      st <- H.modify \st -> st { selected = remove' item st.selected }
      _ <- H.query unit $ Select.replaceItems $ filter' st.items st.selected
      H.raise (SelectionsChanged st.selected)
      pure next

    HandleSelect message next -> case message of
      Select.Searched string -> do
        st <- H.get
        let items = filter (String.contains (String.Pattern string) <<< toText) st.items
        _ <- H.query unit $ Select.replaceItems $ filter' items st.selected
        pure next

      Select.Selected item -> do
        st <- H.modify \st -> st { selected = select' item st.selected }
        _ <- H.query unit $ Select.replaceItems $ filter' st.items st.selected
        _ <- H.query unit $ Select.search ""
        H.raise (SelectionsChanged st.selected)
        pure next

      _ -> pure next
