module Example.RealWorld.Spec.GroupForm where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Example.RealWorld.Data.Group (GroupForm(..), GroupFormRow)
import Example.Validation.Semigroup as V
import Formless.Spec as FSpec
import Formless.Validation (onInputField)
import Type.Row (RProxy(..))

-- | mkFormSpecFromRow can produce a valid input form spec from your row
-- | without you having to type anything.
groupFormSpec :: GroupForm FSpec.FormSpec
groupFormSpec =
  FSpec.mkFormSpecFromRow $ RProxy :: RProxy (GroupFormRow FSpec.Input)

groupFormValidation
  :: GroupForm FSpec.InputField
  -> GroupForm FSpec.InputField
groupFormValidation (GroupForm form) = GroupForm
  { name: V.validateNonEmpty `onInputField` form.name
  , secretKey1: (\i ->
      V.validateNonEmpty i
      *> V.validateEqual (_.input $ unwrap form.secretKey2) i
      ) `onInputField` form.secretKey1
  , secretKey2: (\i ->
      V.validateNonEmpty i
      *> V.validateEqual (_.input $ unwrap form.secretKey1) i
      ) `onInputField` form.secretKey2
  , admin: V.validateMaybe `onInputField` form.admin
  , applications: V.validateNonEmptyF `onInputField` form.applications
  , pixels: V.validateNonEmptyF `onInputField` form.pixels
  , maxBudget: const (pure $ Just 100) `onInputField` form.maxBudget
  , minBudget: V.validateInt `onInputField` form.minBudget
  , whiskey: V.validateMaybe `onInputField` form.whiskey
  }
