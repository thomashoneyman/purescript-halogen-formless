module Example.RealWorld.Spec.OptionsForm where

import Prelude

import Example.RealWorld.Data.Options (OptionsForm(..), OptionsRow)
import Formless.Spec as FSpec
import Type.Row (RProxy(..))

-- | mkFormSpecFromRow can produce a valid input form spec from your row
-- | without you having to type anything.
optionsFormSpec :: OptionsForm FSpec.FormSpec
optionsFormSpec =
  FSpec.mkFormSpecFromRow $ RProxy :: RProxy (OptionsRow FSpec.Input)

-- | You should provide your own validation. This example uses the PureScript
-- | standard, `purescript-validation`.
optionsFormValidation
  :: OptionsForm FSpec.InputField
  -> OptionsForm FSpec.InputField
optionsFormValidation (OptionsForm form) = OptionsForm form
