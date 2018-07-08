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
    HH.div_
      [ HH.h2_
        [ HH.text "Contact us."
        , HH.br_
        ]
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
   [ HH.input
     [ HP.attr (HH.AttrName "style") "border: 1px solid #c2c6cc;"
     , HP.value name.input
     , Formless.onBlurWith _name
     , Formless.onValueInputWith _name
     ]
   , HH.br_
   , HH.text $ "Touched: " <> show name.touched
   , HH.br_
   , HH.text $ "Result: " <> maybe "not run" (either show show) name.result
   , HH.br_
   , HH.br_
   , HH.textarea
     [ HP.attr (HH.AttrName "style") "border: 1px solid #c2c6cc;"
     , HP.value text.input
     , Formless.onBlurWith _text
     , Formless.onValueInputWith _text
     ]
   , HH.br_
   , HH.text $ "Touched: " <> show text.touched
   , HH.br_
   , HH.text $ "Result: " <> maybe "not run" (either (const "void") show) text.result
   ]
  where
    name = unwrap $ Record.get _name $ unwrap state.form
    text = unwrap $ Record.get _text $ unwrap state.form
