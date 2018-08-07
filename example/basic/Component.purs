module Example.Basic.Component where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import Example.App.UI.Element as UI
import Example.App.Validation as V
import Formless as F
import Formless.Validation.Polyform (toEither)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Query a = HandleFormless (F.Message' Form Contact Aff) a

type ChildQuery = F.Query' Form Contact Aff
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
    , HH.slot unit F.component
        { inputs
        , validators
        , submitter: pure <<< F.unwrapRecord
        , render: renderFormless
        }
        (const Nothing)
    ]

  eval :: Query ~> H.ParentDSL Unit Query ChildQuery ChildSlot Void Aff
  eval (HandleFormless (F.Submitted contact) a) = a <$ do
    H.liftEffect $ log $ show (contact :: Contact)
  eval (HandleFormless _ a) = pure a

-----
-- Formless

type Contact =
  { name :: String
  , text :: String
  }

type Form f =
  ( name :: f V.Errs String String
  , text :: f Unit String String
  )

inputs :: Form F.InputField
inputs = F.mkInputFields
  { name: ""
  , text: ""
  }

validators :: F.PublicState Form Aff -> Form Record (F.Validator Aff)
validators _ = F.wrapRecord
  { name: toEither $ V.minLength 5
  , text: toEither $ V.notRequired
  }

renderFormless :: F.State Form Contact Aff -> F.HTML' Form Contact Aff
renderFormless state =
 UI.formContent_
 [ UI.input
     { label: "Name"
     , help: UI.resultToHelp "Write your name" $ F.getResult _name state.form
     , placeholder: "Dale"
     }
     [ HP.value $ F.getInput _name state.form
     , HE.onValueInput $ HE.input $ F.modifyValidate _name
     ]
 , UI.textarea
     { label: "Message"
     , help: Right "Write us a message"
     , placeholder: "We prefer nice messages, but have at it."
     }
     [ HP.value $ F.getInput _text state.form
     , HE.onValueInput $ HE.input $ F.modify _text
     ]
   , UI.buttonPrimary
     [ HE.onClick $ HE.input_ F.Submit ]
     [ HH.text "Submit" ]
   ]
  where
    _name = SProxy :: SProxy "name"
    _text = SProxy :: SProxy "text"
