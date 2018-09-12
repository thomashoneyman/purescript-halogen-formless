module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj)
import Effect (Effect)
import Effect.Aff (Aff)
import Formless.Spec (FormField(..), InputField(..), U(..))
import Formless.Transform.Record (unsafeRunValidationVariant, unsafeSetInputVariant)
import Formless.Validation (Validation, hoistFn_)
import Test.Unit (suite, test)
import Test.Unit.Assert (equal')
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  suite "variant manipulation" do
   test "can set input variant" do
     let res = unsafeSetInputVariant testInputV testFormR
     equal' "Setting inputs not equal" res testFormInputRes

   test "can run validation" do
     res <- unsafeRunValidationVariant testValidationV testValidationR testFormR
     equal' "Validation not equal" res testFormValidRes


----------
-- Unsafe Internals

newtype Form r f = Form (r ( foo :: f Void String String ))
derive instance newtypeForm :: Newtype (Form r f) _
derive newtype instance eqFormField :: Eq (Form Record FormField)

testFormR :: Form Record FormField
testFormR = Form { foo: FormField { input: "goodbye", touched: true, result: Nothing } }

-----
-- Set Inputs

testInputV :: Form Variant InputField
testInputV = Form $ inj (SProxy :: SProxy "foo") (InputField "hello")

testFormInputRes :: Form Record FormField
testFormInputRes = Form
  { foo: FormField { input: "hello", touched: true, result: Nothing } }

-----
-- Validation

testValidationR :: Form Record (Validation Form Aff)
testValidationR = Form { foo: hoistFn_ identity }

testValidationV :: Form Variant U
testValidationV = Form $ inj (SProxy :: SProxy "foo") U

testFormValidRes :: Form Record FormField
testFormValidRes = Form
  { foo: FormField
    { input: "goodbye"
    , touched: true
    , result: Just (Right "goodbye")
    }
  }
