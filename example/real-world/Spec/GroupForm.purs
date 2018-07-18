module Example.RealWorld.Spec.GroupForm where

import Prelude

import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Validation.Semigroup (V)
import Example.RealWorld.Data.Group
  (Admin, Group(..), GroupForm, GroupFormRow, GroupId(..), _secretKey1, _secretKey2)
import Example.Validation.Semigroup (InvalidPrimitive)
import Example.Validation.Semigroup as V
import Formless.Spec (OutputField, getInput, unwrapOutput)
import Formless.Spec as FSpec
import Formless.Validation.Semigroup (applyOnInputFields)
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
groupFormValidate form = pure $ applyOnInputFields
  { name: V.validateNonEmpty
  , secretKey1:
      (\i ->
        V.validateNonEmpty i
        *> V.validateEqual (getInput _secretKey2 form) i
      )
  , secretKey2:
      (\i ->
        V.validateNonEmpty i
        *> V.validateEqual (getInput _secretKey1 form) i
      )
  , admin: validateAdmin
  , applications: validateStrings
  , pixels: validateStrings
  , maxBudget: validateBudget
  , minBudget: V.validateInt
  , whiskey: \(i :: Maybe String) -> V.validateMaybe i
  }
  form


validateAdmin :: Maybe Admin -> V (NonEmptyList InvalidPrimitive) Admin
validateAdmin = V.validateMaybe

validateStrings :: Array String -> V (NonEmptyList InvalidPrimitive) (Array String)
validateStrings = V.validateNonEmptyF

validateBudget :: String -> V (NonEmptyList InvalidPrimitive) (Maybe Int)
validateBudget = const (pure $ Just 100)
