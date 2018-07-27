module Example.Polyform.Component where

import Prelude

import Data.Lens as Lens
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Effect.Console as Console
import Example.Utils as V
import Formless as F
import Formless.Spec.Transform (class MakeLenses, mkLensesFromFormSpec)
import Formless.Validation.Polyform (applyOnInputFields)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Block.Input as Input
import Ocelot.HTML.Properties (css)
import Polyform.Validation as Validation
import Prim.RowList as RL
import Record (delete)
import Type.Row (RProxy(..))

data Query a = HandleFormless (F.Message' Form User) a

type State = Unit

type ChildQuery = F.Query' Form User Aff
type ChildSlot = Unit

component :: H.Component HH.HTML Query Unit Void Aff
component =
  H.parentComponent
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    }
  where

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void Aff
  eval = case _ of
    HandleFormless m a -> case m of
      F.Emit _ -> pure a
      F.Submitted user -> a <$ do
        H.liftEffect $ Console.log $ show (user :: User)
      F.Changed fstate -> a <$ do
        H.liftEffect $ Console.log $ show $ delete (SProxy :: SProxy "form") fstate

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
  render st =
    HH.div
    [ css "flex-1 container p-12" ]
    [ Format.heading_
      [ HH.text "Formless" ]
    , Format.subHeading_
      [ HH.text "A form using the composable validation toolkit Polyform." ]
    , Format.p_
      [ HH.text $
          "In Formless, you can use whatever validation library you prefer. The component provides "
          <> "helpers for working with "
      , HH.code_ [ HH.text "purescript-polyform" ]
      , HH.text "or the canonical "
      , HH.code_ [ HH.text "purescript-validation" ]
      , HH.text $
          " library, but you're not obligated to use either. This form demonstrates using Polyform "
          <> "as the underlying validation library, whereas the other examples use "
      , HH.code_ [ HH.text "purescript-validation" ]
      , HH.text "."
      ]
    , Format.p_
      [ HH.text $
        "Try watching the console output as you fill out the form, and notice how you can only reset the "
        <> "form if it is in a dirty state, and can only submit the form if it is valid."
      ]
    , HH.slot
        unit
        F.component
        { formSpec: F.mkFormSpecFromRow $ RProxy :: RProxy (FormRow F.InputType)
        , validator
        , submitter: pure <<< F.unwrapOutput
        , render: renderFormless
        }
        (HE.input HandleFormless)
    ]


----------
-- Spec

type User = Record (FormRow F.OutputType)

newtype Form f = Form (Record (FormRow f))
derive instance newtypeForm :: Newtype (Form f) _

type FormRow f =
  ( name  :: f V.Errs String V.Name
  , email :: f V.Errs String V.Email
  , city  :: f V.Errs String String
  , state :: f V.Errs String String
  )

_name = SProxy :: SProxy "name"
_email = SProxy :: SProxy "email"
_city = SProxy :: SProxy "city"
_state = SProxy :: SProxy "state"

validator :: ∀ m. MonadEffect m => Form F.InputField -> m (Form F.InputField)
validator = applyOnInputFields
  { name: V.Name <$> (V.minLength 5 *> V.maxLength 10)
  , email: V.emailFormat >>> V.emailIsUsed
  , city: V.minLength 0
  , state: Validation.hoistFnV pure
  }

formSpec :: Form F.FormSpec
formSpec = F.mkFormSpecFromRow $ RProxy :: RProxy (FormRow F.InputType)

lenses
  :: ∀ row xs row'
   . RL.RowToList row xs
  => MakeLenses Form xs row'
  => Newtype (Form F.FormSpec) (Record row)
  => Record row'
lenses = mkLensesFromFormSpec formSpec

----------
-- Render

-- | Our render function has access to anything in Formless' State type, plus
-- | anything additional in your own state type, if you'd like.
renderFormless :: F.State Form User Aff -> F.HTML' Form User Aff
renderFormless state =
  HH.div_
    [ renderName
    , renderEmail
    , renderCity
    , renderState
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
          else HE.onClick $ HE.input_ F.ResetAll
      ]
      [ HH.text "Reset" ]
    ]

  where

    renderName =
      HH.div_
        [ FormField.field_
            { label: "Name"
            , helpText: Just "Write your name."
            , error: V.showError (F.getResult _name state.form)
            , inputId: "name"
            }
            [ Input.input
              [ HP.placeholder "Dale"
              , HP.value $ _.input $ (Lens.view (Lens.cloneLens lenses.name.field) state.form)
              , HE.onBlur $ HE.input_ F.Validate
              , HE.onValueInput $ HE.input $ F.Modify <<< F.setInput _name
              ]
            ]
        ]

    renderEmail =
      HH.div_
        [ FormField.field_
            { label: "Email"
            , helpText: Just "Enter an email address."
            , error: V.showError (F.getResult _email state.form)
            , inputId: "email"
            }
            [ Input.input
              [ HP.placeholder "hello@me.com"
              , HP.value (F.getInput _email state.form)
              , HE.onBlur $ HE.input_ F.Validate
              , HE.onValueInput $ HE.input $ F.Modify <<< F.setInput _email
              ]
            ]
        ]

    renderCity =
      HH.div_
        [ FormField.field_
            { label: "City"
            , helpText: Just "Tell us your favorite city."
            , error: V.showError (F.getResult _city state.form)
            , inputId: "city"
            }
            [ Input.input
              [ HP.placeholder "Los Angeles"
              , HP.value (F.getInput _city state.form)
              , HE.onBlur $ HE.input_ F.Validate
              , HE.onValueInput $ HE.input $ F.Modify <<< F.setInput _city
              ]
            ]
        ]

    renderState =
      HH.div_
        [ FormField.field_
            { label: "State"
            , helpText: Just "Oh, you thought this would be a literal US state? Well, too bad for you, that's right. Tell us one."
            , error: V.showError (F.getResult _state state.form)
            , inputId: "state"
            }
            [ Input.input
              [ HP.value (F.getInput _state state.form)
              , HE.onBlur $ HE.input_ F.Validate
              , HE.onValueInput $ HE.input $ F.Modify <<< F.setInput _state
              ]
            ]
        ]
