module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Debug.Trace (spy)
import Effect (Effect)
import Example.Validation.Semigroup (InvalidPrimitive, validateMinimumLength)
import Formless.Spec (FormSpec(..), InputField(..))
import Formless.Validation (onInputField)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  suite "pure form validation" do
    test "validates" do
      let _ = spy "input form" inForm
          _ = spy "intended output form" $ outForm
          _ = spy "output form" $ validateForm outForm
      Assert.equal 0 0

-----
-- Types

newtype Form f = Form
  { name :: f String (NonEmptyList InvalidPrimitive) String
  , text :: f String Void String
  }
derive instance newtypeForm :: Newtype (Form f) _

formSpec :: Form FormSpec
formSpec = Form
  { name: FormSpec ""
  , text: FormSpec ""
  }

validateForm
  :: Form InputField
  -> Form InputField
validateForm (Form form) = Form
  { name: (flip validateMinimumLength 10) `onInputField` form.name
  , text: pure `onInputField` form.text
  }

-----
-- Example Forms

inForm :: Form InputField
inForm = Form
  { name: InputField
    { input: "Jordan"
    , touched: true
    , result: Nothing
    }
  , text: InputField
    { input: ""
    , touched: false
    , result: Nothing
    }
  }

outForm :: Form InputField
outForm = Form
  { name: InputField
    { input: "Jordan"
    , touched: true
    , result: Just $ Right "Jordan"
    }
  , text: InputField
    { input: ""
    , touched: false
    , result: Nothing
    }
  }


