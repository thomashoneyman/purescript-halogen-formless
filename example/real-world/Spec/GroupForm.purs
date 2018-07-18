module Example.RealWorld.Spec.GroupForm where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Example.RealWorld.Data.Group
  (Group(..), GroupForm(..), GroupFormRow, GroupId(..))
import Example.Validation.Semigroup as V
import Formless.Spec (OutputField, unwrapOutput)
import Formless.Spec as FSpec
import Formless.Validation (onInputField)
import Record as Record
import Type.Row (RProxy(..))

-- | mkFormSpecFromRow can produce a valid input form spec from your row
-- | without you having to type anything.
groupFormSpec :: GroupForm FSpec.FormSpec
groupFormSpec =
  FSpec.mkFormSpecFromRow $ RProxy :: RProxy (GroupFormRow FSpec.Input)

-- | We can use simple record manipulations to change the group form result
-- | into our output type
groupFormSubmit :: ∀ m. Monad m => GroupForm OutputField -> m Group
groupFormSubmit form = do
  -- This could be a server call or something else that is necessary
  -- to collect the information to complete your output type.
  groupId <- pure (GroupId 10)
  pure $ Group
    <<< Record.delete (SProxy :: SProxy "secretKey2")
    <<< Record.rename (SProxy :: SProxy "secretKey1") (SProxy :: SProxy "secretKey")
    <<< Record.insert (SProxy :: SProxy "id") groupId
    <<< Record.insert (SProxy :: SProxy "options") Nothing
    $ unwrapOutput form

-- | We'll provide a fairly involved validation function to verify the fields are
-- | correct. This includes things like dependent validation.
groupFormValidate
  :: ∀ m
   . Monad m
  => GroupForm FSpec.InputField
  -> m (GroupForm FSpec.InputField)
groupFormValidate (GroupForm form) = pure $
  GroupForm
    { name: V.validateNonEmpty `onInputField` form.name
    , secretKey1:
        (\i ->
          V.validateNonEmpty i
          *> V.validateEqual (_.input $ unwrap form.secretKey2) i
        )
        `onInputField` form.secretKey1
    , secretKey2:
        (\i ->
          V.validateNonEmpty i
          *> V.validateEqual (_.input $ unwrap form.secretKey1) i
        )
        `onInputField` form.secretKey2
    , admin: V.validateMaybe `onInputField` form.admin
    , applications: V.validateNonEmptyF `onInputField` form.applications
    , pixels: V.validateNonEmptyF `onInputField` form.pixels
    , maxBudget: const (pure $ Just 100) `onInputField` form.maxBudget
    , minBudget: V.validateInt `onInputField` form.minBudget
    , whiskey: V.validateMaybe `onInputField` form.whiskey
    }

