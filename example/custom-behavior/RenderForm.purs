module Example.CustomBehavior.RenderForm where

import Prelude

import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Example.CustomBehavior.Spec (Form, _color, _name, _object)
import Example.CustomBehavior.Types (FCQ, FCS, Query(..))
import Formless as Formless
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Input as Input
import Ocelot.Components.Dropdown as Dropdown
import Record as Record

-- | Our render function has access to anything in Formless' State type, plus
-- | anything additional in your own state type.
formless
  :: Formless.State Form
  -> Formless.HTML Query FCQ FCS Form Aff
formless state =
  HH.div_
    [ renderName state
    , renderColor state
    , renderObject state
    , Button.buttonPrimary
      [ HE.onClick $ HE.input_ Formless.Submit ]
      [ HH.text "Submit" ]
    ]

----------
-- Helpers

-- | A helper function to render a form text input
renderName :: Formless.State Form -> Formless.HTML Query FCQ FCS Form Aff
renderName state =
  HH.div_
    [ FormField.field_
        { label: "Name"
        , helpText: Just "Write your name."
        , error: join $ map (either Just (const Nothing)) field.result
        , inputId: "name"
        }
        [ Input.input
          [ HP.placeholder "Dale"
          , HP.value field.input
          , Formless.onBlurWith _name
          , Formless.onValueInputWith _name
          ]
        ]
    ]
  where
    field = unwrap $ Record.get _name $ unwrap state.form

renderColor :: Formless.State Form -> Formless.HTML Query FCQ FCS Form Aff
renderColor state =
  HH.div_
    [ FormField.field_
        { label: "Color"
        , helpText: Just "Select a color you like"
        , error: join $ map (either Just (const Nothing)) field.result
        , inputId: "color"
        }
        [ HH.slot
            0
            Dropdown.dropdown
            { selectedItem: Nothing
            , items: [ "Red", "Blue", "Green", "Cyan" ]
            , label: "Pick one"
            , toString: identity
            , disabled: false
            }
            ( HE.input (Formless.Raise <<< H.action <<< HandleColorDropdown) )
        ]
    ]
  where
    field = unwrap $ Record.get _color $ unwrap state.form

renderObject :: Formless.State Form -> Formless.HTML Query FCQ FCS Form Aff
renderObject state =
  HH.div_
    [ FormField.field_
        { label: "Object"
        , helpText: Just "Select an object to match with your color"
        , error: join $ map (either Just (const Nothing)) field.result
        , inputId: "object"
        }
        [ HH.slot
            1
            Dropdown.dropdown
            { selectedItem: Nothing
            , items: [ "Tree", "Fence", "House", "Garbage Pail" ]
            , label: "Pick one"
            , toString: identity
            , disabled: false
            }
            ( HE.input (Formless.Raise <<< H.action <<< HandleObjectDropdown) )
        ]
    ]
  where
    field = unwrap $ Record.get _object $ unwrap state.form
