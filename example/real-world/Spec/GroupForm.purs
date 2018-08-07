module Example.RealWorld.Spec.GroupForm where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Symbol (SProxy(..))
import Example.App.Validation as V
import Example.RealWorld.Data.Group (Group(..), GroupForm, GroupId(..), prx)
import Formless as F
import Formless.Validation.Semigroup (toEitherPure)
import Record as Record

groupFormSubmit :: ∀ m. Monad m => GroupForm Record F.OutputField -> m Group
groupFormSubmit form = do
  -- This could be a server call or something else that is necessary
  -- to collect the information to complete your output type.
  groupId <- pure (GroupId 10)
  pure $ Group
    <<< Record.delete (SProxy :: SProxy "secretKey2")
    <<< Record.rename (SProxy :: SProxy "secretKey1") (SProxy :: SProxy "secretKey")
    <<< Record.insert (SProxy :: SProxy "id") groupId
    <<< Record.insert (SProxy :: SProxy "options") Nothing
    <<< F.unwrapRecord
    $ unwrap form

groupInputs :: GroupForm Record F.InputField
groupInputs = F.mkInputFields $ F.FormProxy :: F.FormProxy GroupForm

groupValidators :: ∀ m. Monad m => F.PublicState GroupForm m -> GroupForm Record (F.Validator m)
groupValidators { form } = wrap $ F.wrapRecord
  { name: toEitherPure V.validateNonEmpty
    -- Despite being a field-level validation, you can use other fields in the form because the
    -- public state is provided as an argument.
  , secretKey1: toEitherPure $ V.validateEqual (F.getInput prx.secretKey2 form)
  , secretKey2: toEitherPure $ V.validateEqual (F.getInput prx.secretKey1 form)
  , admin: toEitherPure V.validateMaybe
  , applications: toEitherPure V.validateNonEmptyArray
  , pixels: toEitherPure V.validateNonEmptyArray
  , whiskey: toEitherPure V.validateMaybe
  }
