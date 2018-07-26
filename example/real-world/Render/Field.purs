module Example.RealWorld.Render.Field where

import Prelude

import Data.Either (Either)
import Data.Lens as Lens
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype)
import Data.String (toLower) as String
import Data.Symbol (class IsSymbol, SProxy)
import Effect.Aff (Aff)
import Example.RealWorld.Data.Group (Admin(..))
import Example.RealWorld.Types (Query)
import Example.Utils (showError)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
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
  => Newtype (form F.InputField) (Record fields)
  => Cons sym (F.InputField e String o) t0 fields
  => FieldConfig sym
  -> FieldType
  -> F.State form out m
  -> F.HTML pq cq cs form out m
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
      , HE.onBlur $ HE.input_ F.Validate
      , HE.onValueInput $ HE.input $ F.Modify <<< F.setInput config.field
      ]


-- | A utility to help create form fields using an unwrapped
-- | field value from a given symbol.
formField
  :: ∀ form sym i e o t0 fields m pq cq cs out
   . IsSymbol sym
  => Show e
  => Newtype (form F.InputField) (Record fields)
  => Cons sym (F.InputField e i o) t0 fields
  => F.State form out m
  -> FieldConfig sym
  -> ( { result :: Maybe (Either e o)
       , touched :: Boolean
       , input :: i
       }
       -> F.HTML pq cq cs form out m
     )
  -> F.HTML pq cq cs form out m
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
    field = Lens.view (F._Field config.field) state.form


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
