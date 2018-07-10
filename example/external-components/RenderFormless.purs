module Example.ExternalComponents.RenderFormless where

import Prelude

import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Example.ExternalComponents.Spec (Form, _email, _name)
import Example.ExternalComponents.Types (FCQ, FCS, Query(..))
import Formless as Formless
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Input as Input
import Ocelot.Components.Typeahead as TA
import Ocelot.Components.Typeahead.Input as TA.Input
import Record as Record

-- | Our render function has access to anything in Formless' State type, plus
-- | anything additional in your own state type.
formless
  :: Formless.State Form
  -> Formless.HTML Query FCQ FCS Form Aff
formless state =
  HH.div_
    [ renderName state
    , renderEmail state
    , Button.buttonPrimary
      [ HE.onClick $ HE.input_ Formless.Submit ]
      [ HH.text "Submit" ]
    ]

----------
-- Helpers

-- | A helper function to render a form text input
renderName :: Formless.State Form -> Formless.HTML Query FCQ FCS Form Aff
renderName state =
  let field = unwrap $ Record.get sym $ unwrap state.form
      sym = _name
      label = "Name"
   in
      HH.div_
        [ if field.touched
            then HH.text "-- changed since form initialization --"
            else HH.text ""
        , FormField.field_
            { label: label
            , helpText: Just "Write your name."
            , error: join $ map (either Just (const Nothing)) field.result
            , inputId: label
            }
            [ Input.input
              [ HP.placeholder "Dale"
              , HP.id_ label
              , HP.value field.input
              , Formless.onBlurWith sym
              , Formless.onValueInputWith sym
              ]
            ]
        ]

renderEmail :: Formless.State Form -> Formless.HTML Query FCQ FCS Form Aff
renderEmail state =
  let field = unwrap $ Record.get sym $ unwrap state.form
      sym = _email
      label = "Email"
   in
      HH.div_
        [ if field.touched
            then HH.text "-- changed since form initialization --"
            else HH.text ""
        , FormField.field_
            { label: label
            , helpText: Just "Write your name."
            , error: join $ map (either Just (const Nothing)) field.result
            , inputId: label
            }
            [ HH.slot
                unit
                TA.component
                ( TA.Input.defSingle
                  [ HP.placeholder "Search email addresses..." ]
                  [ "not@anemail.org"
                  , "snail@utopia.snailutopia"
                  , "blue@jordans@blordans.pordens"
                  , "yea_that_won't_work@email.com"
                  , "standard@email.com"
                  ]
                  TA.Input.renderItemString
                )
                ( HE.input (Formless.Raise <<< H.action <<< HandleTypeahead) )
            ]
        ]
