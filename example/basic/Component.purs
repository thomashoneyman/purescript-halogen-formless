module Example.Basic.Component where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Example.Utils (Errs, minLength, notRequired, showError)
import Formless as F
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

data Query a = DoNothing a

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
    HH.div
    [ css "flex-1 container p-12" ]
    [ Format.heading_
      [ HH.text "Formless" ]
    , Format.subHeading_
      [ HH.text "A basic contact form." ]
    , HH.slot
        unit
        F.component
        { formSpec: F.mkFormSpec { name: "", text: "" }
        , validator
        , submitter: pure <<< F.unwrapOutput
        , render: renderFormless
        }
        (const Nothing)
    ]

  eval :: Query ~> H.ParentDSL Unit Query ChildQuery ChildSlot Void Aff
  eval (DoNothing a) = pure a


-----
-- Formless

type Contact =
  { name :: String
  , text :: String
  }

newtype Form f = Form
  { name :: f Errs String String
  , text :: f Unit String String
  }
derive instance newtypeForm :: Newtype (Form f) _

validator :: ∀ m. Monad m => Form F.InputField -> m (Form F.InputField)
validator = applyOnInputFields $ identity
  { name: minLength 5
  , text: notRequired
  }

renderFormless :: F.State Form Contact Aff -> F.HTML' Form Contact Aff
renderFormless state =
 HH.div_
   [ FormField.field_
     { label: "Name"
     , helpText: Just "Write your name."
     , error: showError (F.getResult _name state.form)
     , inputId: "name"
     }
     [ Input.input
       [ HP.value (F.getInput _name state.form)
       , HE.onBlur $ HE.input_ F.Validate
       , HE.onValueInput $ HE.input $ F.Modify <<< F.setInput _name
       ]
     ]
   , FormField.field_
     { label: "Message"
     , helpText: Just "Write us a message!"
     , error: Nothing
     , inputId: "message"
     }
     [ Input.textarea
       [ HP.value (F.getInput _text state.form)
       , HE.onBlur $ HE.input_ F.Validate
       , HE.onValueInput $ HE.input $ F.Modify <<< F.setInput _text
       ]
     ]
   , Button.buttonPrimary
     [ HE.onClick $ HE.input_ F.Submit ]
     [ HH.text "Submit" ]
   ]
  where
    _name = SProxy :: SProxy "name"
    _text = SProxy :: SProxy "text"
