module Example.RealWorld.Spec.GroupForm where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Example.App.Validation as V
import Example.RealWorld.Data.Group (Group(..), GroupForm(..), GroupId(..))
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
    $ F.unwrapOutput form


groupFormSpec :: ∀ m. Monad m => GroupForm Record (F.FormSpec m)
groupFormSpec = GroupForm
  { name: F.FormSpec
    { input: ""
    , validator: toEitherPure V.validateNonEmpty
    }
  , secretKey1: F.FormSpec
    { input: ""
    , validator: toEitherPure V.validateNonEmpty
    }
  , secretKey2: F.FormSpec
    { input: ""
    , validator: toEitherPure V.validateNonEmpty
    }
  , admin: F.FormSpec
    { input: Nothing
    , validator: toEitherPure V.validateMaybe
    }
  , applications: F.FormSpec
    { input: []
    , validator: toEitherPure V.validateNonEmptyArray
    }
  , pixels: F.FormSpec
    { input: []
    , validator: toEitherPure V.validateNonEmptyArray
    }
  , whiskey: F.FormSpec
    { input: Nothing
    , validator: toEitherPure V.validateMaybe
    }
  }

-- | We'll provide a fairly involved validation function to verify fields are correct
-- | TODO:
-- | Only fields that you want to validate are necessary. The others will not run.
--  groupFormValidate :: ∀ m. Monad m => GroupForm Record (F.FormField m) -> m (GroupForm Record (F.FormField m))
--  groupFormValidate form = pure $ GroupForm
--    { name: toEitherPure $ (\(i :: String) -> pure i) `onFormField` form'.name
--    , secretKey1: toEitherPure $ V.validateEqual (F.getInput prx.secretKey2 form) `onFormField` form'.secretKey1
--    , secretKey2: toEitherPure $ V.validateEqual (F.getInput prx.secretKey2 form) `onFormField` form'.secretKey2
--    , admin: toEitherPure $ pure `onFormField` form'.admin
--    , applications: toEitherPure $ pure `onFormField` form'.applications
--    , pixels: toEitherPure $ pure `onFormField` form'.pixels
--    , whiskey: toEitherPure $ pure `onFormField` form'.whiskey
--    }
--    where
--      form' = unwrap form
