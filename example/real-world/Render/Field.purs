module Example.RealWorld.Render.Field where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.String (toLower) as String
import Data.Symbol (class IsSymbol, SProxy)
import Example.Validation.Utils (showError)
import Formless as Formless
import Formless.Spec (InputField)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Input as Input
import Prim.Row (class Cons)
import Record as Record

-----
-- Common field rendering

text
  :: ∀ form sym e o t0 fields m pq cq cs
   . IsSymbol sym
  => Show e
  => Newtype (form InputField) (Record fields)
  => Cons sym (InputField String e o) t0 fields
  => { label :: String, helpText :: String, placeholder :: Maybe String, field :: SProxy sym }
  -> Formless.State form m
  -> Formless.HTML pq cq cs form m
text config state =
  HH.div_
    [ FormField.field_
        { label: config.label
        , helpText: Just config.helpText
        , error: showError field
        , inputId: String.toLower config.label
        }
        [ Input.input
          [ HP.placeholder $ fromMaybe "" config.placeholder
          , HP.value field.input
          , Formless.onBlurWith config.field
          , Formless.onValueInputWith config.field
          ]
        ]
    ]
  where
    field = unwrap $ Record.get config.field $ unwrap state.form

