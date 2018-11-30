module Example.Async.Component where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Console (logShow)
import Example.App.UI.Element as UI
import Example.App.Validation as V
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Query a = Formless (F.Message' Form) a

type ChildQuery = F.Query' Form Aff
type ChildSlot = Unit

component :: H.Component HH.HTML Query Unit Void Aff
component = H.parentComponent
  { initialState: const unit
  , render
  , eval
  , receiver: const Nothing
  }
  where

  render :: Unit -> H.ParentHTML Query ChildQuery ChildSlot Aff
  render _ =
    UI.section_
    [ UI.h1_ [ HH.text "Formless" ]
    , UI.h2_ [ HH.text "A form with debounced async fields." ]
    , UI.p_ $
        "If you have fields with expensive validation, you can debounce modifications to the field "
        <> "with the async versions of setValidate and modifyValidate query functions. The result "
        <> "type of the form field lets you know whether the field has not been validated, is "
        <> "currently validating, or has produced an error or result."
    , HH.br_
    , HH.slot unit F.component { initialInputs, validators, render: renderForm } (HE.input Formless)
    ]

  eval :: Query ~> H.ParentDSL Unit Query ChildQuery ChildSlot Void Aff
  eval (Formless (F.Submitted formOutputs) a) = a <$ do
    let result = F.unwrapOutputFields formOutputs
    H.liftEffect $ logShow result

  -- In this example we can ignore other outputs, but see the other examples for more
  -- in-depth usage.
  eval (Formless _ a) = pure a

----------
-- FORM SPEC
-----------

newtype Form r f = Form (r (FormRow f))
derive instance newtypeForm :: Newtype (Form r f) _

type FormRow f =
  ( name    :: f V.FieldError String String
  , email   :: f V.FieldError String V.Email
  , balance :: f V.FieldError String Int
  )

-- | You'll usually want symbol proxies for convenience
prx :: F.SProxies Form
prx = F.mkSProxies $ F.FormProxy :: F.FormProxy Form

-- | You can generate your initial inputs
initialInputs :: Form Record F.InputField
initialInputs = F.mkInputFields $ F.FormProxy :: F.FormProxy Form

validators :: ∀ m. MonadAff m => Form Record (F.Validation Form m)
validators = Form
  { name: V.minLength 5
  , email: V.emailFormat >>> V.emailIsUsed
  , balance: V.strIsInt >>> V.enoughMoney
  }

----------
-- RENDER
----------

renderForm :: F.State Form Aff -> F.HTML' Form Aff
renderForm { form } =
 UI.formContent_
  [ UI.input
      { label: "Name"
      , help: UI.resultToHelp "Write your name" $ F.getResult prx.name form
      , placeholder: "Frank Ocean"
      }
      [ HP.value $ F.getInput prx.name form
      , HE.onValueInput $ HE.input $ F.setValidate prx.name
      ]
  , UI.input
      { label: "Email"
      , help: UI.resultToHelp "Provide your email address" $ F.getResult prx.email form
      , placeholder: "john@hamm.com"
      }
      [ HP.value $ F.getInput prx.email form
      , HE.onValueInput $ HE.input $ F.asyncSetValidate (Milliseconds 300.0) prx.email
      ]
  , UI.input
      { label: "Donation"
      , help: UI.resultToHelp "How many dollas do you want to spend?" $ F.getResult prx.balance form
      , placeholder: "1000"
      }
      [ HP.value $ F.getInput prx.balance form
      , HE.onValueInput $ HE.input $ F.asyncSetValidate (Milliseconds 500.0) prx.balance
      ]
  , UI.buttonPrimary
      [ HE.onClick $ HE.input_ F.submit ]
      [ HH.text "Submit" ]
  ]