module Example.Polyform.Component where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Effect.Console as Console
import Example.App.UI.Element as UI
import Example.App.Validation as V
import Formless as F
import Formless.Validation.Polyform (applyOnFormInputs)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Polyform.Validation as Validation
import Record (delete)

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
    UI.section_
    [ UI.h1_ [ HH.text "Formless" ]
    , UI.h2_ [ HH.text "A form using the composable validation toolkit Polyform." ]
    , HH.p_
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
    , HH.br_
    , UI.p_ $
        "Try watching the console output as you fill out the form, and notice how you can only reset the "
        <> "form if it is in a dirty state, and can only submit the form if it is valid."
    , HH.br_
    , HH.slot
        unit
        F.component
        { formSpec: F.mkFormSpecFromProxy _form
        , validator
        , submitter: pure <<< F.unwrapOutput
        , render: renderFormless
        }
        (HE.input HandleFormless)
    ]


----------
-- Formless

-- We can recover both our user type and our form from the same row.
type User = Record (FormRow F.OutputType)

newtype Form r f = Form (r (FormRow f))
derive instance newtypeForm :: Newtype (Form r f) _

-- This proxy will let us generate all the SProxies for our form as
-- well as our entire initial form.
_form = F.FormProxy :: F.FormProxy Form

-- This is a record of symbol proxies, which we can now pass to the
-- various Formless functions that require them. See the render function
-- below as an example in practice.
proxies :: F.SProxies Form
proxies = F.mkSProxies _form

type FormRow f =
  ( name  :: f V.Errs String V.Name
  , email :: f V.Errs String V.Email
  , city  :: f V.Errs String String
  , state :: f V.Errs String String
  )

validator :: ∀ m. MonadEffect m => Form Record F.FormInput -> m (Form Record F.FormInput)
validator = applyOnFormInputs
  { name: V.Name <$> (V.minLength 5 *> V.maxLength 10)
  , email: V.emailFormat >>> V.emailIsUsed
  , city: V.minLength 0
  , state: Validation.hoistFnV pure
  }

renderFormless :: F.State Form User Aff -> F.HTML' Form User Aff
renderFormless state =
  UI.formContent_
  [ UI.formlessField
      UI.input
      { label: "Name"
      , help: "Write your name"
      , placeholder: "Dale"
      , sym: proxies.name
      } state
  , UI.formlessField
      UI.input
      { label: "Email Address"
      , help: "Write your email"
      , placeholder: "me@you.com"
      , sym: proxies.email
      } state
  , UI.formlessField
      UI.input
      { label: "City"
      , help: "Write your favorite city"
      , placeholder: "Los Angeles"
      , sym: proxies.city
      } state
  , UI.formlessField
      UI.input
      { label: "State"
      , help: "Write your favorite state of mind"
      , placeholder: ""
      , sym: proxies.state
      } state
    , HH.br_
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
            else HE.onClick $ HE.input_ F.Submit
        ]
        [ HH.text "Submit" ]
      , UI.button
        [ if not state.dirty
            then HP.disabled true
            else HE.onClick $ HE.input_ F.ResetAll
        ]
        [ HH.text "Reset" ]
      ]
    ]

