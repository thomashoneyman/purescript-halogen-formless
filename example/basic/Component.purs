module Example.Basic.Component where

import Prelude

import Data.Const (Const)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.String (null)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Formless as Formless
import Formless.Spec (FormSpec(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.Format (heading_, mutedClasses, p, subHeading_) as Format
import Ocelot.Block.Input (input, textarea) as Input
import Ocelot.HTML.Properties (css)
import Record (get) as Record

-----
-- Form spec

newtype Form f = Form
  { name :: f String String String
  , text :: f String Void String
  }
derive instance newtypeForm :: Newtype (Form f) _

formSpec :: Form FormSpec
formSpec = Form
  { name: FormSpec
    { input: ""
    , validator: \s -> if null s then Left "Too short" else Right s
    }
  , text: FormSpec
    { input: ""
    , validator: pure
    }
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
   [ Input.input
     [ HP.value name.input
     , Formless.onBlurWith _name
     , Formless.onValueInputWith _name
     ]
   , muted
       (show name.touched)
       (maybe "not run" (either show show) name.result)
   , Input.textarea
     [ HP.value text.input
     , Formless.onBlurWith _text
     , Formless.onValueInputWith _text
     ]
   , muted
       (show text.touched)
       (maybe "not run" (either (const "void") show) text.result)
   ]
  where
    name = unwrap $ Record.get _name $ unwrap state.form
    text = unwrap $ Record.get _text $ unwrap state.form
    muted str0 str1 =
      Format.p
        [ HP.classes Format.mutedClasses ]
        [ HH.text $ "Touched: " <> str0
        , HH.text " | "
        , HH.text $ "Validated: " <> str1
        ]
