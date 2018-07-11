module Example.Basic.Component where

import Prelude

import Data.Const (Const)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Formless as Formless
import Formless.Spec (FormSpec(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Button as Button
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Format as Format
import Ocelot.Block.Input as Input
import Ocelot.HTML.Properties (css)
import Record as Record

-----
-- Form spec

newtype Form f = Form
  { name :: f String String String
  , text :: f String Void String
  }
derive instance newtypeForm :: Newtype (Form f) _

formSpec :: Form FormSpec
formSpec = Form
  { name: FormSpec ""
  , text: FormSpec ""
  }

_name = SProxy :: SProxy "name"
_text = SProxy :: SProxy "text"

-----
-- Component types

data Query a
  = DoNothing a

type ChildQuery = Formless.Query Query (Const Void) Unit Form Aff
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
      [ HH.text "Formless" ]
    , Format.subHeading_
      [ HH.text "A basic contact form." ]
    , HH.slot
        unit
        Formless.component
        { formSpec
        , render: formless
        }
        (const Nothing)
    ]

  eval :: Query ~> H.ParentDSL Unit Query ChildQuery ChildSlot Void Aff
  eval (DoNothing a) = pure a


-----
-- Formless

formless
  :: Formless.State Form
  -> Formless.HTML Query (Const Void) Unit Form Aff
formless state =
 HH.div_
   [ FormField.field_
     { label: "Name"
     , helpText: Just $ "Write your name." <> (if name.touched then " (touched)" else "")
     , error: join $ map (either Just (const Nothing)) name.result
     , inputId: "name"
     }
     [ Input.input
       [ HP.value name.input
       , Formless.onBlurWith _name
       , Formless.onValueInputWith _name
       ]
     ]
   , FormField.field_
     { label: "Message"
     , helpText: Just $ "Write us a message!" <> (if text.touched then " (touched)" else "")
     , error: Nothing -- Errors are impossible.
     , inputId: "message"
     }
     [ Input.textarea
       [ HP.value text.input
       , Formless.onBlurWith _text
       , Formless.onValueInputWith _text
       ]
     ]
   , Button.buttonPrimary
     [ HE.onClick $ HE.input_ Formless.Submit ]
     [ HH.text "Submit" ]
   ]
  where
    name = unwrap $ Record.get _name $ unwrap state.form
    text = unwrap $ Record.get _text $ unwrap state.form
