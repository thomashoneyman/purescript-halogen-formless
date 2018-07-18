module Example.RealWorld.Render.OptionsForm where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Example.RealWorld.Data.Options (Metric(..), Speed(..), _enable, _metric, _speed)
import Example.RealWorld.Data.Options as OP
import Example.RealWorld.Render.Field (renderDropdown)
import Example.RealWorld.Render.Field as Field
import Example.RealWorld.Types (OptionsCQ, OptionsCS, Query(..))
import Example.Utils (showError)
import Formless as Formless
import Formless.Spec (getField, getInput)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button (button) as Button
import Ocelot.Block.Card as Card
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Block.Radio as Radio
import Ocelot.Block.Toggle as Toggle
import Ocelot.Components.Dropdown as Dropdown
import Ocelot.HTML.Properties (css)

-- | A convenience synonym for the group Formless state
type FormlessState
  = Formless.State OP.OptionsForm OP.Options Aff

-- | A convenience synonym for the group Formless HTML type
type FormlessHTML
  = Formless.HTML Query OptionsCQ OptionsCS OP.OptionsForm OP.Options Aff

-- | The form, grouped by sections.
render :: FormlessState -> FormlessHTML
render state =
  HH.div_
    [ Card.card_
        [ Format.subHeading_
          [ HH.text "Overview" ]
        , renderEnabled state
        ]
    , HH.div
        [ if (getInput _enable state.form) then css "" else css "hidden" ]
        [ renderMetrics state
        , renderOthers state
        ]
    ]

-----
-- Form parts

renderMetrics :: FormlessState -> FormlessHTML
renderMetrics state =
  Card.card_
    [ Format.subHeading_
      [ HH.text "Metrics" ]
    , renderMetric state
    , renderMetricField $ getInput _metric state.form
    ]
  where
    renderMetricField = case _ of
      Just ViewCost -> renderViewCost state
      Just ClickCost -> renderClickCost state
      Just InstallCost -> renderInstallCost state
      Nothing -> HH.div_ []

renderOthers :: FormlessState -> FormlessHTML
renderOthers state =
  Card.card_
    [ Format.subHeading_
      [ HH.text "Other Settings" ]
    ,  renderSize state
    , renderDimensions state
    , renderSpeed state
    ]


-----
-- Fields

renderEnabled :: FormlessState -> FormlessHTML
renderEnabled state =
  FormField.field_
    { label: "Enable"
    , helpText: Just "Do you want to enable this set of options?"
    , error: showError enable
    , inputId: "enable"
    }
    [ Toggle.toggle
      [ HP.checked enable.input
      , Formless.onChangeWith _enable (not enable.input)
      , Formless.onBlurWith _enable
      ]
    ]
  where
    enable = getField _enable state.form

renderMetric :: FormlessState -> FormlessHTML
renderMetric state =
  HH.div_
    [ Field.formField state
      { label: "Metric"
      , placeholder: Nothing
      , helpText: "Choose a metric to optimize for."
      , field: OP._metric
      }
      \metric ->
        HH.slot
          unit
          Dropdown.component
          { selectedItem: Nothing
          , items: [ ViewCost, ClickCost, InstallCost ]
          , render: renderDropdown Button.button show "Choose a metric"
          }
          ( HE.input
            ( Formless.Raise
              <<< H.action
              <<< HandleMetricDropdown
            )
          )
    ]

renderViewCost :: FormlessState -> FormlessHTML
renderViewCost =
  Field.input
  { label: "View Cost"
  , placeholder: Just "100"
  , helpText: "Enter a dollar amount for view costs."
  , field: OP._viewCost
  } Field.Currency

renderClickCost :: FormlessState -> FormlessHTML
renderClickCost =
  Field.input
  { label: "Click Cost"
  , placeholder: Just "1"
  , helpText: "Enter a dollar amount for click costs."
  , field: OP._clickCost
  } Field.Currency

renderInstallCost :: FormlessState -> FormlessHTML
renderInstallCost =
  Field.input
  { label: "App Install Cost"
  , placeholder: Just "10"
  , helpText: "Enter a dollar amount for installation costs."
  , field: OP._installCost
  } Field.Currency

renderSize :: FormlessState -> FormlessHTML
renderSize =
  Field.input
  { label: "Size"
  , placeholder: Just "1.123"
  , helpText: "Enter a size for the campaign."
  , field: OP._size
  } Field.Text

renderDimensions :: FormlessState -> FormlessHTML
renderDimensions =
  Field.input
  { label: "Dimensions"
  , placeholder: Just "5.2"
  , helpText: "Enter a set of dimensions for the campaign."
  , field: OP._dimensions
  } Field.Text

renderSpeed :: FormlessState -> FormlessHTML
renderSpeed state =
  FormField.fieldset_
  { label: "Speed"
  , inputId: "speedy-mcgee"
  , helpText: Just "How fast do you want to go?"
  , error: showError speed
  }
  [ HH.div_
    [ Radio.radio_
      [ HP.name "speed"
      , HP.checked $ speed.input == Low
      , Formless.onClickWith _speed Low
      ]
      [ HH.text $ show Low ]
    , Radio.radio_
      [ HP.name "speed"
      , HP.checked $ speed.input == Medium
      , Formless.onClickWith _speed Medium
      ]
      [ HH.text $ show Medium ]
    , Radio.radio_
      [ HP.name "speed"
      , HP.checked $ speed.input == Fast
      , Formless.onClickWith _speed Fast
      ]
      [ HH.text $ show Fast ]
    ]
  ]
  where
    speed = getField _speed state.form
