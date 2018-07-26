module Example.Polyform.RenderForm where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Example.Polyform.Spec (Form, User, _city, _email, _name, _state)
import Example.Polyform.Types (FCQ, FCS, Query)
import Example.Utils (showError)
import Formless as F
import Formless.Events as FE
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Block.Input as Input
import Ocelot.HTML.Properties (css)

-- | Our render function has access to anything in Formless' State type, plus
-- | anything additional in your own state type, if you'd like.
formless
  :: F.State Form User Aff
  -> F.HTML Query FCQ FCS Form User Aff
formless state =
  HH.div_
    [ renderName state
    , renderEmail state
    , renderCity state
    , renderState state
    , Format.p_
      [ HH.text $
          "You can only attempt to submit this form if it is valid "
          <> "and not already being submitted. You can only attempt "
          <> "to reset the form if it has been changed from its initial "
          <> "state."
      ]
    , Button.buttonPrimary
      [ if state.submitting || state.validity /= F.Valid
          then HP.disabled true
          else HE.onClick $ HE.input_ F.Submit
      , css "mr-3"
      ]
      [ HH.text "Submit" ]
    , Button.button
      [ if not state.dirty
          then HP.disabled true
          else HE.onClick $ HE.input_ F.Reset
      ]
      [ HH.text "Reset" ]
    ]

----------
-- Helpers

-- | A helper function to render a form text input
renderName
  :: F.State Form User Aff
  -> F.HTML Query FCQ FCS Form User Aff
renderName state =
  HH.div_
    [ FormField.field_
        { label: "Name"
        , helpText: Just "Write your name."
        , error: showError field
        , inputId: "name"
        }
        [ Input.input
          [ HP.placeholder "Dale"
          , HP.value field.input
          , HE.onDoubleClick $ HE.input_ $ F.AndThen
              (FE.handleBlur _name)
              (FE.modify _name (\i -> i <> i))
          , FE.onBlurWith _name
          , FE.onValueInputWith _name
          ]
        ]
    ]
  where
    field = F.getField _name state.form

renderEmail
  :: F.State Form User Aff
  -> F.HTML Query FCQ FCS Form User Aff
renderEmail state =
  HH.div_
    [ FormField.field_
        { label: "Email"
        , helpText: Just "Enter an email address."
        , error: showError field
        , inputId: "email"
        }
        [ Input.input
          [ HP.placeholder "hello@me.com"
          , HP.value field.input
          , FE.onBlurWith _email
          , FE.onValueInputWith _email
          ]
        ]
    ]
  where
    field = F.getField _email state.form

renderCity
  :: F.State Form User Aff
  -> F.HTML Query FCQ FCS Form User Aff
renderCity state =
  HH.div_
    [ FormField.field_
        { label: "City"
        , helpText: Just "Tell us your favorite city."
        , error: showError field
        , inputId: "city"
        }
        [ Input.input
          [ HP.placeholder "Los Angeles"
          , HP.value field.input
          , FE.onBlurWith _city
          , FE.onValueInputWith _city
          ]
        ]
    ]
  where
    field = F.getField _city state.form

renderState
  :: F.State Form User Aff
  -> F.HTML Query FCQ FCS Form User Aff
renderState state =
  HH.div_
    [ FormField.field_
        { label: "State"
        , helpText: Just "Oh, you thought this would be a literal US state? Well, too bad for you, that's right. Tell us one."
        , error: showError field
        , inputId: "state"
        }
        [ Input.input
          [ HP.value field.input
          , FE.onBlurWith _state
          , FE.onValueInputWith _state
          ]
        ]
    ]
  where
    field = F.getField _state state.form
