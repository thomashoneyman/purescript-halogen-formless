module Example.RealWorld.Render.Field where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype)
import Data.String (toLower) as String
import Data.Symbol (class IsSymbol, SProxy)
import Effect.Aff (Aff)
import Example.RealWorld.Data.Group (Admin(..))
import Example.RealWorld.Types (Query)
import Example.Utils (showError)
import Formless as Formless
import Formless.Spec (InputField, getField)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Input as Input
import Ocelot.Components.Dropdown as Dropdown
import Ocelot.Components.Dropdown.Render as DR
import Prim.Row (class Cons)

-----
-- Types

type FieldConfig sym =
  { label :: String
  , helpText :: String
  , placeholder :: Maybe String
  , field :: SProxy sym
  }

-----
-- Common field rendering

data FieldType
  = Currency
  | Percentage
  | Text

input
  :: ∀ form sym e o t0 fields m pq cq cs out
   . IsSymbol sym
  => Show e
  => Newtype (form InputField) (Record fields)
  => Cons sym (InputField e String o) t0 fields
  => FieldConfig sym
  -> FieldType
  -> Formless.State form out m
  -> Formless.HTML pq cq cs form out m
input config ft state =
  HH.div_
    [ formField state config $ \field ->
        case ft of
          Text -> Input.input (props field)
          Currency -> Input.currency_ (props field)
          Percentage -> Input.percentage_ (props field)
    ]
  where
    props field =
      [ HP.placeholder $ fromMaybe "" config.placeholder
      , HP.value field.input
      , Formless.onBlurWith config.field
      , Formless.onValueInputWith config.field
      ]


-- | A utility to help create form fields using an unwrapped
-- | field value from a given symbol.
formField
  :: ∀ form sym i e o t0 fields m pq cq cs out
   . IsSymbol sym
  => Show e
  => Newtype (form InputField) (Record fields)
  => Cons sym (InputField e i o) t0 fields
  => Formless.State form out m
  -> FieldConfig sym
  -> ( { result :: Maybe (Either e o)
       , touched :: Boolean
       , input :: i
       }
       -> Formless.HTML pq cq cs form out m
     )
  -> Formless.HTML pq cq cs form out m
formField state config html =
  HH.div_
    [ FormField.field_
        { label: config.label
        , helpText: Just config.helpText
        , error: showError field
        , inputId: String.toLower config.label
        }
        [ html field ]
    ]
  where
    field = getField config.field state.form


-----
-- Dropdowns

adminToString :: Admin -> String
adminToString (Admin { id }) = maybe "None" show id

renderDropdown
 :: ∀ item
  . Eq item
 => (∀ p i. DR.ButtonFn p i)
 -> (item -> String)
 -> String
 -> Dropdown.State item
 -> H.ParentHTML (Dropdown.Query Query item Aff) (Dropdown.ChildQuery Query item) Dropdown.ChildSlot Aff
renderDropdown btnFn change label = DR.render $ DR.defDropdown btnFn [ ] change label
