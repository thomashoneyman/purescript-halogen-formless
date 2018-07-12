module Example.RealWorld.Spec.GroupForm where

import Prelude

import Example.RealWorld.Data.Group (GroupForm(..), GroupFormRow)
import Formless.Spec as FSpec
import Type.Row (RProxy(..))

-- | mkFormSpecFromRow can produce a valid input form spec from your row
-- | without you having to type anything.
groupFormSpec :: GroupForm FSpec.FormSpec
groupFormSpec = FSpec.mkFormSpecFromRow $ RProxy :: RProxy (GroupFormRow FSpec.Input)

-- | You should provide your own validation. This example uses the PureScript
-- | standard, `purescript-validation`.
groupFormValidation :: GroupForm FSpec.InputField -> GroupForm FSpec.InputField
groupFormValidation (GroupForm form) = GroupForm form
