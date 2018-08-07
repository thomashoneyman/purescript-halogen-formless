module Test.Main where

import Prelude

import Effect (Effect)
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  suite "pure form validation" do
    test "validates" do
      pure unit
