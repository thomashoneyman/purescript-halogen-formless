module Example.RealWorld.Spec.GroupForm where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Validation.Semigroup (V)
import Example.RealWorld.Data.Group (Group(..), GroupForm, GroupId(..), proxies)
import Example.App.Validation as V
import Formless as F
import Formless.Validation.Semigroup (applyOnInputFields)
import Record as Record

groupFormSpec :: GroupForm F.FormSpec
groupFormSpec = F.mkFormSpecFromProxy $ F.FormProxy :: F.FormProxy GroupForm

groupFormSubmit :: ∀ m. Monad m => GroupForm F.OutputField -> m Group
groupFormSubmit form = do
  -- This could be a server call or something else that is necessary
  -- to collect the information to complete your output type.
  groupId <- pure (GroupId 10)
  pure $ Group
    <<< Record.delete (SProxy :: SProxy "secretKey2")
    <<< Record.rename (SProxy :: SProxy "secretKey1") (SProxy :: SProxy "secretKey")
    <<< Record.insert (SProxy :: SProxy "id") groupId
    <<< Record.insert (SProxy :: SProxy "options") Nothing
    $ F.unwrapOutput form

-- | We'll provide a fairly involved validation function to verify fields are correct
groupFormValidate :: ∀ m. Monad m => GroupForm F.InputField -> m (GroupForm F.InputField)
groupFormValidate form = pure $ applyOnInputFields
  ( identity
    { name: V.validateNonEmpty
    , secretKey1:
        \i -> V.validateNonEmpty i *> V.validateEqual (F.getInput proxies.secretKey2 form) i
    , secretKey2:
        \i -> V.validateNonEmpty i *> V.validateEqual (F.getInput proxies.secretKey1 form) i
    , admin: V.validateMaybe
    , applications: V.validateNonEmptyArray
    , pixels: V.validateNonEmptyArray
    , whiskey: V.validateMaybe
    }
  )
  form

  where

  validateBudget :: String -> V V.Errs (Maybe Int)
  validateBudget = const (pure $ Just 100)
