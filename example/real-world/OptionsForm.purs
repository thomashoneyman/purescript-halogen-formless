module Example.RealWorld.OptionsForm where

import Prelude

import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Newtype (class Newtype, over)
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import Example.App.UI.Dropdown as DD
import Example.App.UI.Element (class_)
import Example.App.UI.Element as UI
import Example.App.Validation (class ToText, FieldError)
import Example.App.Validation as V
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select as Select
import Type.Proxy (Proxy(..))

-- Supporting types

newtype Dollars = Dollars Int
derive instance newtypeDollars :: Newtype Dollars _
derive newtype instance eqDollars :: Eq Dollars
derive newtype instance showDollars :: Show Dollars

-- Depending on the user's choice of metric, different fields ought to display
data Metric = ViewCost | ClickCost | InstallCost
derive instance genericMetric :: Generic Metric _
derive instance eqMetric :: Eq Metric
derive instance ordMetric :: Ord Metric

instance showMetric :: Show Metric where
  show = genericShow

instance toTextMetric :: ToText Metric where
  toText ViewCost = "View Cost"
  toText ClickCost = "Click Cost"
  toText InstallCost = "Install Cost"

-- This data type will be used in radio buttons. To generate a default value
-- with `F.mkInputFields` we'll need an instance of the `Initial` type class
data Speed = Low | Medium | Fast
derive instance genericSpeed :: Generic Speed _
derive instance eqSpeed :: Eq Speed
derive instance ordSpeed :: Ord Speed

instance showSpeed :: Show Speed where
  show = genericShow

instance initialSpeed :: F.Initial Speed where
  initial = Low

-- This is the data type used throughout our fake application. In this case, it's
-- the same type the form and the underlying row, so we'll use `F.OutputType`.
newtype Options = Options
  { enable :: Boolean
  , metric :: Metric
  , viewCost :: Maybe Dollars
  , clickCost :: Maybe Dollars
  , installCost :: Maybe Dollars
  , size :: Number
  , dimensions :: Number
  , speed :: Speed
  }
derive instance newtypeOptions :: Newtype Options _
derive newtype instance eqOptions :: Eq Options
derive newtype instance showOptions :: Show Options

-- Form types

newtype OptionsForm (r :: Row Type -> Type) f = OptionsForm (r (OptionsRow f))
derive instance newtypeOptionsForm :: Newtype (OptionsForm r f) _

type OptionsRow :: (Type -> Type -> Type -> Type) -> Row Type
type OptionsRow f =
  ( enable :: f Void Boolean Boolean
  , metric :: f FieldError (Maybe Metric) Metric
  , viewCost :: f FieldError String (Maybe Dollars)
  , clickCost :: f FieldError String (Maybe Dollars)
  , installCost :: f FieldError String (Maybe Dollars)
  , size :: f FieldError String Number
  , dimensions :: f FieldError String Number
  , speed :: f Void Speed Speed
  )

-- Form component types

type Slot =
  H.Slot (F.Query OptionsForm (Const Void) ChildSlots) Message

_optionsForm = Proxy :: Proxy "optionsForm"

-- We'll maintain a flag so we can check if the enabled state has changed
type State =
  (prevEnabled :: Boolean)

data Action
  = HandleDropdown (DD.Message Metric)

type Message =
  { errors :: Int
  , dirty :: Boolean
  }

type ChildSlots =
  (dropdown :: DD.Slot Metric Unit)

-- Form spec

prx :: F.SProxies OptionsForm
prx = F.mkSProxies (Proxy :: Proxy OptionsForm)

component :: F.Component OptionsForm (Const Void) ChildSlots Unit Message Aff
component = F.component (const input) $ F.defaultSpec
  { render = render
  , handleAction = handleAction
  , handleEvent = handleEvent
  }
  where
  input :: F.Input OptionsForm State Aff
  input =
    { prevEnabled: false
    , initialInputs: Just defaultInputFields
    , validators: OptionsForm
        { enable: F.noValidation
        , metric: V.exists
        , viewCost: validateMetric ViewCost
        , clickCost: validateMetric ClickCost
        , installCost: validateMetric InstallCost
        , size: Int.toNumber <$> V.strIsInt
        , dimensions: Int.toNumber <$> V.strIsInt
        , speed: F.noValidation
        }
    }
    where
    validateMetric metric = F.Validation \form i ->
      if F.getInput prx.metric form == Just metric then map (map (Just <<< Dollars)) $ F.runValidation V.strIsInt form i
      else pure (pure Nothing)

  defaultInputFields :: OptionsForm Record F.InputField
  defaultInputFields = F.wrapInputFields
    { enable: false
    , metric: Just ViewCost
    , viewCost: "10"
    , clickCost: ""
    , installCost: ""
    , size: "21"
    , dimensions: "3005"
    , speed: Low
    }

  -- available for both handleEvent and handleAction
  eval act = F.handleAction handleAction handleEvent act

  handleEvent = case _ of
    F.Changed form -> do
      st <- H.get
      let enabled = F.getInput prx.enable form.form
      H.raise { errors: form.errors, dirty: form.dirty }
      H.modify_ _ { prevEnabled = enabled }
      when (st.prevEnabled /= enabled) case enabled of
        true -> do
          let
            initial = F.mkInputFields
            new = over OptionsForm (_ { enable = F.InputField true }) initial
          eval $ F.loadForm new
        _ ->
          eval $ F.loadForm defaultInputFields
    _ -> pure unit

  handleAction = case _ of
    HandleDropdown msg -> case msg of
      DD.Selected new -> eval $ F.setValidate prx.metric (Just new)
      DD.Cleared -> eval $ F.setValidate prx.metric Nothing

  render st@{ form } = UI.formContent_
    [ renderEnabled
    , HH.div
        [ class_ ("is-hidden" # guard (not $ F.getInput prx.enable form)) ]
        [ renderMetric
        , case F.getInput prx.metric form of
            Just ViewCost -> renderViewCost
            Just ClickCost -> renderClickCost
            Just InstallCost -> renderInstallCost
            Nothing -> HH.div_ []
        , renderSize
        , renderDimensions
        , renderSpeed
        ]
    ]
    where
    renderEnabled = UI.field
      { label: "Enable"
      , help: Right "Do you want to enable this set of options?"
      }
      [ HH.label
          [ class_ "checkbox" ]
          [ HH.input
              [ class_ "checkbox"
              , HP.type_ InputCheckbox
              , HP.checked $ F.getInput prx.enable form
              , HE.onChange \_ -> F.modify prx.enable not
              ]
          , HH.text " Enable extra options"
          ]
      ]

    renderMetric = UI.field
      { label: "Metric"
      , help: F.getResult prx.metric form # UI.resultToHelp
          "Choose a metric to optimize for."
      }
      [ HH.slot DD._dropdown unit (Select.component DD.input DD.spec) ddInput handler ]
      where
      handler = F.injAction <<< HandleDropdown
      ddInput =
        { placeholder: "Choose a metric"
        , items: [ ViewCost, ClickCost, InstallCost ]
        }

    renderViewCost = st # UI.formlessField UI.input
      { label: "View Cost"
      , placeholder: "100"
      , help: "Enter a dollar amount for view costs."
      , sym: prx.viewCost
      }

    renderClickCost = st # UI.formlessField UI.input
      { label: "Click Cost"
      , placeholder: "1"
      , help: "Enter a dollar amount you're willing to pay for a click."
      , sym: prx.clickCost
      }

    renderInstallCost = st # UI.formlessField UI.input
      { label: "Install Cost"
      , placeholder: "10"
      , help: "Enter a dollar amount you're willing to pay for an app install."
      , sym: prx.installCost
      }

    renderSize = st # UI.formlessField UI.input
      { label: "Size"
      , placeholder: "10.233"
      , help: "Enter a total campaign size."
      , sym: prx.size
      }

    renderDimensions = st # UI.formlessField UI.input
      { label: "Dimensions"
      , placeholder: "1.027"
      , help: "Enter a total campaign dimension set ratio buzzword."
      , sym: prx.dimensions
      }

    renderSpeed = UI.field
      { label: "Speed", help: Right "How fast do you want to go?" }
      [ speedInput Low, speedInput Medium, speedInput Fast ]
      where
      speed = F.getField prx.speed form
      speedInput speed' =
        HH.label
          [ class_ "radio" ]
          [ HH.input
              [ HP.name "speed"
              , class_ "radio"
              , HP.type_ InputRadio
              , HP.checked $ speed.input == speed'
              , HE.onClick \_ -> F.set prx.speed speed'
              ]
          , HH.text (" " <> show speed')
          ]
