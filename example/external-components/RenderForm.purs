module Example.ExternalComponents.RenderForm where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Example.ExternalComponents.Spec (Form, User, _email, _language, _name, _whiskey)
import Example.ExternalComponents.Types (FCQ, FCS, Query(..), Slot(..))
import Example.Utils (showError)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Block.Input as Input
import Ocelot.Components.Typeahead as TA
import Ocelot.Components.Typeahead.Input as TA.Input
import Ocelot.HTML.Properties (css)

formless :: F.State Form User Aff -> F.HTML Query FCQ FCS Form User Aff
formless state =
  HH.div_
    [ renderName state
    , renderEmail state
    , renderWhiskey state
    , renderLanguage state
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
          else HE.onClick $ HE.input_ $ F.Raise $ H.action Reset
      ]
      [ HH.text "Reset" ]
    ]

----------
-- Helpers

-- | A helper function to render a form text input
renderName :: F.State Form User Aff -> F.HTML Query FCQ FCS Form User Aff
renderName state =
  HH.div_
    [ FormField.field_
        { label: "Name"
        , helpText: Just "Write your name."
        , error: showError (F.getResult _name state.form)
        , inputId: "name"
        }
        [ Input.input
          [ HP.placeholder "Dale"
          , HP.value (F.getInput _name state.form)
          , HE.onBlur $ HE.input_ F.Validate
          , HE.onValueInput $ HE.input $ F.Modify <<< F.setInput _name
          ]
        ]
    ]

renderEmail
  :: F.State Form User Aff
  -> F.HTML Query FCQ FCS Form User Aff
renderEmail state =
  HH.div_
    [ FormField.field_
        { label: "Email"
        , helpText: Just "Select an email address"
        , error: showError (F.getResult _email state.form)
        , inputId: "email"
        }
        [ HH.slot
            EmailTypeahead
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
            ( HE.input $ F.Raise <<< H.action <<< HandleTypeahead EmailTypeahead )
        ]
    ]

renderWhiskey :: F.State Form User Aff -> F.HTML Query FCQ FCS Form User Aff
renderWhiskey state =
  HH.div_
    [ FormField.field_
        { label: "Whiskey"
        , helpText: Just "Select a favorite whiskey"
        , error: showError (F.getResult _whiskey state.form)
        , inputId: "whiskey"
        }
        [ HH.slot
            WhiskeyTypeahead
            TA.component
            ( TA.Input.defSingle
              [ HP.placeholder "Search whiskies..." ]
              [ "Lagavulin 16"
              , "Kilchoman Blue Label"
              , "Laphroaig"
              , "Ardbeg"
              ]
              TA.Input.renderItemString
            )
            ( HE.input $ F.Raise <<< H.action <<< HandleTypeahead WhiskeyTypeahead )
        ]
    ]

renderLanguage :: F.State Form User Aff -> F.HTML Query FCQ FCS Form User Aff
renderLanguage state =
  HH.div_
    [ FormField.field_
        { label: "Language"
        , helpText: Just "Select a favorite language"
        , error: showError (F.getResult _language state.form)
        , inputId: "language"
        }
        [ HH.slot
            LanguageTypeahead
            TA.component
            ( TA.Input.defSingle
              [ HP.placeholder "Search lanugages..." ]
              [ "Rust"
              , "Python"
              , "Haskell"
              , "PureScript"
              , "PHP"
              , "JavaScript"
              , "C"
              , "C++"
              , "C#"
              , "C--"
              , "Ruby"
              , "APL"
              ]
              TA.Input.renderItemString
            )
            ( HE.input $ F.Raise <<< H.action <<< HandleTypeahead LanguageTypeahead )
        ]
    ]
