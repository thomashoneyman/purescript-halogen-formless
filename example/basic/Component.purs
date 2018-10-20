module Example.Basic.Component where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Console (logShow)
import Example.App.UI.Element as UI
import Example.App.Validation as V
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Query a = Formless (F.Message' ContactForm) a

type ChildQuery = F.Query' ContactForm Aff
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
  render st =
    UI.section_
    [ UI.h1_ [ HH.text "Formless" ]
    , UI.h2_ [ HH.text "A basic contact form." ]
    , UI.p_ $
        "You can create a full Halogen contact form like this in less than 100 lines of code with "
        <> "Formless, most of which is simply Halogen boilerplate. The actual form spec and wiring "
        <> "consists of less than 20 lines of code."
    , HH.br_
    , HH.slot unit F.component { initialInputs, validators, render: renderFormless } (HE.input Formless)
    ]

  eval :: Query ~> H.ParentDSL Unit Query ChildQuery ChildSlot Void Aff
  eval (Formless (F.Submitted formOutputs) a) = a <$ do
    -- To unwrap the OutputField newtypes on each field and the overall ContactForm newtype,
    -- use the unwrapOutputFields helper.
    let contact :: Contact
        contact = F.unwrapOutputFields formOutputs
    H.liftEffect $ logShow contact

  -- In this example we can ignore other outputs, but see the other examples for more
  -- in-depth usage.
  eval (Formless _ a) = pure a

-----
-- Formless

type Contact = { name :: String, text :: String }

newtype ContactForm r f = ContactForm (r
  ( name :: f V.FieldError String String
  , text :: f Void String String
  ))
derive instance newtypeContactForm :: Newtype (ContactForm r f) _

initialInputs :: ContactForm Record F.InputField
initialInputs = F.wrapInputFields { name: "", text: "" }

validators :: ContactForm Record (F.Validation ContactForm Aff)
validators = ContactForm { name: V.minLength 5, text: F.hoistFn_ identity }

renderFormless :: F.State ContactForm Aff -> F.HTML' ContactForm Aff
renderFormless state =
 UI.formContent_
 [ UI.input
     { label: "Name"
     , help: UI.resultToHelp "Write your name" $ F.getResult _name state.form
     , placeholder: "Dale"
     }
     [ HP.value $ F.getInput _name state.form
     , HE.onValueInput $ HE.input $ F.setValidate _name
     ]
 , UI.textarea
     { label: "Message"
     , help: Right "Write us a message"
     , placeholder: "We prefer nice messages, but have at it."
     }
     [ HP.value $ F.getInput _text state.form
     , HE.onValueInput $ HE.input $ F.set _text
     ]
   , UI.buttonPrimary
     [ HE.onClick $ HE.input_ F.submit ]
     [ HH.text "Submit" ]
   ]
  where
    _name = SProxy :: SProxy "name"
    _text = SProxy :: SProxy "text"
