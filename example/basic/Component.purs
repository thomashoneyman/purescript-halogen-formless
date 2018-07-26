module Example.Basic.Component where

import Prelude

import Data.Const (Const)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Example.Utils (FieldError, validateNonEmpty, showError)
import Formless as F
import Formless.Events as FE
import Formless.Validation.Semigroup (onInputField)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Block.Input as Input
import Ocelot.HTML.Properties (css)

-----
-- Form spec

-- | The type that you care about receiving from the form
type Contact =
  { name :: String
  , text :: String
  }

-- | The same type represented as a form: the possible input,
-- | error, and output types for each field.
newtype Form f = Form
  { name :: f (NonEmptyList FieldError) String String
  , text :: f Void String String
  }
derive instance newtypeForm :: Newtype (Form f) _

-- | You'll generally want to make symbol proxies for convenience
-- | for each field.
_name = SProxy :: SProxy "name"
_text = SProxy :: SProxy "text"


-- | The initial values for the form, which you must provide. If
-- | this seems tedious, it is! For a much less boilerplate-heavy
-- | version, see the external-components or real-world examples.
formSpec :: Form F.FormSpec
formSpec = F.mkFormSpec
  { name: ""
  , text: ""
  }

-- | Your form validation that you'd like run on any touched
-- | fields in the form. It can be monadic, so you can do things like
-- | server validation.
validator :: ∀ m. Monad m => Form F.InputField -> m (Form F.InputField)
validator (Form form) = pure $ Form
  { name: validateNonEmpty `onInputField` form.name
  , text: pure `onInputField` form.text
  }

-- | When the form is run, it will produce your Form type, with
-- | only the output fields. Since our Contact type is the same
-- | shape as the Form type, we can just unwrap the newtypes. This can
-- | be monadic, like hitting a server for extra information.
submitter :: ∀ m. Monad m => Form F.OutputField -> m Contact
submitter = pure <<< F.unwrapOutput

-----
-- Component types

data Query a
  = DoNothing a

type ChildQuery = F.Query Query (Const Void) Unit Form Contact Aff
type ChildSlot = Unit

-----
-- Minimal component

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
      [ HH.text "F" ]
    , Format.subHeading_
      [ HH.text "A basic contact form." ]
    , HH.slot
        unit
        F.component
        { formSpec
        , validator
        , submitter
        , render: formless
        }
        (const Nothing)
    ]

  eval :: Query ~> H.ParentDSL Unit Query ChildQuery ChildSlot Void Aff
  eval (DoNothing a) = pure a


-----
-- F

formless
  :: F.State Form Contact Aff
  -> F.HTML Query (Const Void) Unit Form Contact Aff
formless state =
 HH.div_
   [ FormField.field_
     { label: "Name"
     , helpText: Just "Write your name."
     , error: showError name
     , inputId: "name"
     }
     [ Input.input
       [ HP.value name.input
       , FE.onBlurWith _name
       , FE.onValueInputWith _name
       ]
     ]
   , FormField.field_
     { label: "Message"
     , helpText: Just "Write us a message!"
     , error: Nothing -- Errors are impossible.
     , inputId: "message"
     }
     [ Input.textarea
       [ HP.value text.input
       , FE.onBlurWith _text
       , FE.onValueInputWith _text
       ]
     ]
   , Button.buttonPrimary
     [ HE.onClick $ HE.input_ F.Submit ]
     [ HH.text "Submit" ]
   ]
  where
    name = F.getField _name state.form
    text = F.getField _text state.form
