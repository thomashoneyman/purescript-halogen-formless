module Example.RealWorld.Render.GroupForm where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol)
import Effect.Aff (Aff)
import Example.RealWorld.Data.Group (Admin(..), GroupId(..), _maxBudget, _minBudget)
import Example.RealWorld.Data.Group as G
import Example.RealWorld.Render.Field (FieldConfig, adminToString, renderDropdown)
import Example.RealWorld.Render.Field as Field
import Example.RealWorld.Types (GroupCQ, GroupCS, GroupTASlot(..), Query(..))
import Formless as Formless
import Formless.Spec (InputField, getField)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.Card as Card
import Ocelot.Block.Format as Format
import Ocelot.Block.Input as Input
import Ocelot.Block.Range as Range
import Ocelot.Components.Dropdown as Dropdown
import Ocelot.Components.Typeahead as TA
import Ocelot.Components.Typeahead.Input as TA.Input
import Ocelot.HTML.Properties (css)
import Prim.Row (class Cons)

-- | A convenience synonym for the group Formless state
type FormlessState
  = Formless.State G.GroupForm G.Group Aff

-- | A convenience synonym for the group Formless HTML type
type FormlessHTML
  = Formless.HTML Query GroupCQ GroupCS G.GroupForm G.Group Aff

-- | The form, grouped by sections.
render :: FormlessState -> FormlessHTML
render state =
  HH.div_
    [ Card.card_
      [ Format.subHeading_
        [ HH.text "Name & Admin" ]
      , renderName state
      , renderAdmin state
      , renderSecretKey1 state
      , renderSecretKey2 state
      ]
    , Card.card_
      [ Format.subHeading_
        [ HH.text "Applications & Pixels" ]
      , renderApplications state
      , renderPixels state
      , renderWhiskey state
      ]
    , Card.card_
      [ Format.subHeading_
        [ HH.text "Min & Max Budget" ]
      , renderMinMaxBudget state
      ]
    ]

-----
-- Built fields

renderName :: FormlessState -> FormlessHTML
renderName =
  Field.input
  { label: "Name"
  , placeholder: Just "January Cohort"
  , helpText: "Give the group a name."
  , field: G._name
  } Field.Text

renderSecretKey1 :: FormlessState -> FormlessHTML
renderSecretKey1 =
  Field.input
  { label: "Secret Key"
  , placeholder: Just "au,#OK#F48i$"
  , helpText: "Give the group a secret identifier"
  , field: G._secretKey1
  } Field.Text

renderSecretKey2 :: FormlessState -> FormlessHTML
renderSecretKey2 =
  Field.input
  { label: "Secret Key (Confirm)"
  , placeholder: Just "au,#OK#F48i$"
  , helpText: "Enter the same secret identifier to confirm."
  , field: G._secretKey2
  } Field.Text

renderAdmin :: FormlessState -> FormlessHTML
renderAdmin state =
  HH.div_
    [ Field.formField state
      { label: "Admin (Optional)"
      , placeholder: Nothing
      , helpText: "Choose an admin id to include."
      , field: G._admin
      }
      \admin ->
        HH.slot'
          CP.cp2
          unit
          Dropdown.component
          { selectedItem: Nothing
          , items
          , render: renderDropdown Button.button adminToString "Choose an admin"
          }
          ( HE.input
            ( Formless.Raise
              <<< H.action
              <<< HandleAdminDropdown
            )
          )
    ]
  where
    items =
      [ Admin { id: Nothing }
      , Admin { id: Just $ GroupId 10 }
      , Admin { id: Just $ GroupId 15 }
      , Admin { id: Just $ GroupId 20 }
      , Admin { id: Just $ GroupId 25 }
      , Admin { id: Just $ GroupId 30 }
      , Admin { id: Just $ GroupId 35 }
      ]

renderWhiskey :: FormlessState -> FormlessHTML
renderWhiskey state =
  HH.div_
    [ Field.formField state
      { label: "Whiskey"
      , placeholder: Nothing
      , helpText: "Select one whiskey you'd like in the group."
      , field: G._whiskey
      }
      $ \whiskey ->
        HH.slot'
          CP.cp1
          WhiskeyTypeahead
          TA.component
          ( TA.Input.defSingle
            [ HP.placeholder "Hakushu" ]
            [ "Laphroiag 10"
            , "Lagavulin 12"
            , "Lagavulin 16"
            , "Oban 16"
            , "Kilchoman Blue Label"
            ]
            TA.Input.renderItemString
          )
          ( HE.input
            ( Formless.Raise
             <<< H.action
             <<< HandleGroupTypeahead WhiskeyTypeahead
            )
          )
    ]

renderPixels :: FormlessState -> FormlessHTML
renderPixels =
  multiTypeahead
    PixelsTypeahead
    HandleGroupTypeahead
    { label: "Pixels"
    , placeholder: Just "My unique pixel"
    , helpText: "Select one or more tracking pixels for the group."
    , field: G._name
    }
    [ "My favorite pixel"
    , "Your favorite pixel"
    , "Application main pixel"
    , "A pixel for you is a pixel for me"
    ]

renderApplications :: FormlessState -> FormlessHTML
renderApplications =
 multiTypeahead
   ApplicationsTypeahead
   HandleGroupTypeahead
   { label: "Applications"
   , placeholder: Just "Facebook"
   , helpText: "Select one or more applications for the group."
   , field: G._name
   }
   [ "Facebook", "Google", "Twitter", "Pinterest" ]

renderMinMaxBudget :: FormlessState -> FormlessHTML
renderMinMaxBudget state =
  HH.div
    [ css "flex items-center" ]
    [ HH.label
        [ css "flex-1 block text-center" ]
        [ HH.div
            [ css "py-3" ]
            [ HH.text "Min Budget (Optional)" ]
        , Input.percentage_
            [ Formless.onValueChangeWith _minBudget
            , Formless.onBlurWith _minBudget
            , HP.value minBudget.input
            ]
        ]
    , Range.range
        [ css "px-4 flex-4 self-end my-5"
        , HP.min 0.0
        , HP.max 100.0
        , Formless.onValueChangeWith _minBudget
        , Formless.onBlurWith _minBudget
        , HP.value minBudget.input
        ]
    , HH.label
        [ css "flex-1 block text-center" ]
        [ HH.div
            [ css "py-3" ]
            [ HH.text "Max Budget (Optional)"
            , Input.percentage_
                [ Formless.onValueInputWith _maxBudget
                , Formless.onBlurWith _maxBudget
                , HP.value maxBudget.input
                ]
            ]
        ]
    ]
  where
    minBudget = getField _minBudget state.form
    maxBudget = getField _maxBudget state.form


-----
-- Helper functions

multiTypeahead
  :: ∀ sym e o t0 fields
   . IsSymbol sym
  => Show e
  => Newtype (G.GroupForm InputField) (Record fields)
  => Cons sym (InputField String e o) t0 fields
  => GroupTASlot
  -> (GroupTASlot -> TA.Message Query String -> Unit -> Query Unit)
  -> FieldConfig sym
  -> Array String
  -> FormlessState
  -> FormlessHTML
multiTypeahead slot query config items state =
  HH.div_
    [ Field.formField state config $ \field ->
        HH.slot'
          CP.cp1
          slot
          TA.component
          ( TA.Input.defMulti
            [ HP.placeholder $ fromMaybe "" config.placeholder ]
            items
            TA.Input.renderItemString
          )
          ( HE.input (Formless.Raise <<< H.action <<< query slot) )
    ]

