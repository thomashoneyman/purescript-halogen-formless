module Example.App.UI.Dropdown where

import Prelude

import Data.Array (difference, mapWithIndex)
import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Example.App.UI.Element (css)
import Example.App.Validation (class ToText, toText)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Select as Select
import Select.Utils.Setters as Setters

data Query item a
  = HandleSelect (Select.Message (Const Void) item) a
  | Clear a

type State item =
  { selected :: Maybe item
  , items :: Array item
  , placeholder :: String
  }

type Input item =
  { items :: Array item
  , placeholder :: String
  }

data Message item
  = Selected item
  | Cleared

type ChildSlot = Unit
type ChildQuery item = Select.Query (Const Void) item

component
  :: ∀ item m
   . MonadAff m
  => ToText item
  => Eq item
  => H.Component HH.HTML (Query item) (Input item) (Message item) m
component =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: Input item -> State item
  initialState { items, placeholder } =
    { selected: Nothing
    , items
    , placeholder
    }

  render :: State item -> H.ParentHTML (Query item) (ChildQuery item) ChildSlot m
  render parentState =
    HH.slot unit Select.component selectInput (HE.input HandleSelect)
    where
      selectInput =
        { inputType: Select.Toggle
        , items: parentState.items
        , initialSearch: Nothing
        , debounceTime: Nothing
        , render: dropdown
        }

      dropdown childState =
        HH.div
        [ css "dropdown" ]
        [ toggle parentState
        , menu childState
        ]

  eval :: Query item ~> H.ParentDSL (State item) (Query item) (ChildQuery item) ChildSlot (Message item) m
  eval = case _ of
    Clear next -> do
      st <- H.modify _ { selected = Nothing }
      _ <- H.query unit $ Select.replaceItems st.items
      pure next

    HandleSelect message next -> case message of
      Select.Selected item -> do
        st <- H.get
        _ <- H.query unit $ Select.setVisibility Select.Off
        _ <- H.query unit $ Select.replaceItems $ difference st.items [ item ]
        H.modify_ _ { selected = Just item }
        H.raise (Selected item)
        pure next
      _ -> pure next

toggle
  :: ∀ item r
   . ToText item
  => { placeholder :: String, selected :: Maybe item | r }
  -> Select.ComponentHTML (Const Void) item
toggle parentState =
  HH.div
  [ css "dropdown-trigger" ]
  [ HH.button
    ( Setters.setToggleProps
      [ css "button" ]
    )
    [ HH.span_
      [ HH.text $ fromMaybe parentState.placeholder (toText <$> parentState.selected) ]
    , HH.span
      [ css "icon is-small" ]
      [ HH.i
        [ css "fas fa-angle-down" ]
        []
      ]
    ]
  ]

menu
  :: ∀ item
   . ToText item
  => Select.State item
  -> Select.ComponentHTML (Const Void) item
menu selectState =
  HH.div
  [ css "dropdown-menu" ]
  [ if selectState.visibility == Select.Off then HH.text "" else
    HH.ul
    ( Setters.setContainerProps [ css "dropdown-content" ] )
    ( mapWithIndex (\ix item ->
        HH.li
          ( Setters.setItemProps ix
            $ case Just ix == selectState.highlightedIndex of
                true -> [ css "dropdown-item is-active" ]
                _ -> [ css "dropdown-item" ] )
          [ HH.text (toText item) ]
        )
      selectState.items
    )
  ]
