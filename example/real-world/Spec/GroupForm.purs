module Example.RealWorld.Spec.GroupForm where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Example.App.Validation as V
import Example.RealWorld.Data.Group (Group(..), GroupForm(..), GroupId(..), prx)
import Formless as F
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

groupValidators :: ∀ m. Monad m => GroupForm Record (F.Validation GroupForm m)
groupValidators = GroupForm
  { name: V.nonEmptyStr
    -- Despite being a field-level validation, you can use other fields in the form because the
    -- public state is provided as an argument.
  , secretKey1: V.nonEmptyStr >>> V.minLength 5 >>> equalsSK2
  , secretKey2: V.nonEmptyStr >>> V.minLength 5 >>> equalsSK1
  , admin: V.exists
  , applications: V.nonEmptyArray
  , pixels: V.nonEmptyArray
  , whiskey: V.exists
  }
  where
    -- A custom validator relying on the form state
    equalsSK1 :: F.Validation GroupForm m V.FieldError String String
    equalsSK1 = F.hoistFnE \form str1 ->
      let str0 = F.getInput prx.secretKey1 form
       in if str0 == str1
            then Right str1
            else Left $ V.NotEqual str0 str1

    equalsSK2 :: F.Validation GroupForm m V.FieldError String String
    equalsSK2 = F.hoistFnE \form str1 ->
      let str0 = F.getInput prx.secretKey2 form
       in if str0 == str1
            then Right str1
            else Left $ V.NotEqual str0 str1
