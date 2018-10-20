module Example.ExternalComponents.RenderForm where

import Prelude

import Effect.Aff (Aff)
import Example.App.UI.Element as UI
import Example.App.UI.Typeahead as TA
import Example.ExternalComponents.Spec (UserForm, prx)
import Example.ExternalComponents.Types (Query(..), Slot(..))
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

formless :: F.State UserForm Aff -> F.HTML Query (TA.Query String) Slot UserForm Aff
formless state =
  UI.formContent_
    [ UI.formlessField
        UI.input
        { label: "Name"
        , help: "Write your name"
        , placeholder: "Dale"
        , sym: prx.name
        } state
    , email state
    , whiskey state
    , language state
    , UI.p_ $
        "You can only attempt to submit this form if it is valid "
        <> "and not already being submitted. You can only attempt "
        <> "to reset the form if it has been changed from its initial "
        <> "state."
    , HH.br_
    , UI.grouped_
      [ UI.buttonPrimary
        [ if state.submitting || state.validity /= F.Valid
            then HP.disabled true
            else HE.onClick $ HE.input_ F.submit
        ]
        [ HH.text "Submit" ]
      , UI.button
        [ if not state.dirty
            then HP.disabled true
            else HE.onClick $ HE.input_ $ F.Raise $ H.action Reset
        ]
        [ HH.text "Reset" ]
      ]
    ]

----------
-- Helpers

email :: F.State UserForm Aff -> F.HTML Query (TA.Query String) Slot UserForm Aff
email state =
  UI.field
  { label: "Email"
  , help: UI.resultToHelp "Choose an email address -- carefully." (F.getResult prx.email state.form)
  }
  [ HH.slot Email TA.single
    { placeholder: "me@you.com"
    , items:
        [ "not@anemail.org"
        , "snail@utopia.snailutopia"
        , "blue@jordans@blordans.pordens"
        , "yea_that_won't_work@email.com"
        , "standard@email.com"
        ]
    }
    ( HE.input $ F.Raise <<< H.action <<< Typeahead Email )
  ]

whiskey :: F.State UserForm Aff -> F.HTML Query (TA.Query String) Slot UserForm Aff
whiskey state =
  UI.field
  { label: "Whiskey"
  , help: UI.resultToHelp "Select a favorite whiskey" (F.getResult prx.whiskey state.form)
  }
  [ HH.slot Whiskey TA.single
      { placeholder: "Lagavulin 12"
      , items:
          [ "Lagavulin 16"
          , "Kilchoman Blue Label"
          , "Laphroaig"
          , "Ardbeg"
          ]
      }
      ( HE.input $ F.Raise <<< H.action <<< Typeahead Whiskey )
  ]

language :: F.State UserForm Aff -> F.HTML Query (TA.Query String) Slot UserForm Aff
language state =
  UI.field
  { label: "Language"
  , help: UI.resultToHelp "Choose your favorite programming language." (F.getResult prx.language state.form)
  }
  [ HH.slot Language TA.single
      { placeholder: "Haskell"
      , items:
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
      }
      ( HE.input $ F.Raise <<< H.action <<< Typeahead Language )
  ]
