module Formless.UseForm where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Foldable (all, for_, traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Time.Duration (Milliseconds)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Formless.Data.FormFieldResult (FormFieldResult(..), fromEither)
import Halogen.Hooks (Hook, HookM, UseState, useState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Extra.Hooks.UseDebouncer (UseDebouncer, useDebouncer)

newtype UseForm hooks =
  UseForm (UseState Unit hooks)

derive instance newtypeUseForm :: Newtype (UseForm hooks) _

type FormFields slots output m rows =
  { touched :: Boolean
  , validate :: HookM slots output m Unit
  , reset :: HookM slots output m Unit
  | rows
  }

type FormReturn slots output m =
  { allTouched :: Boolean
  , validateAll :: HookM slots output m Unit
  , resetAll :: HookM slots output m Unit
  }

useForm
  :: forall slots output m rows
   . Array (FormFields slots output m rows)
  -> Hook slots output m (UseState Int) _
useForm fields =
  Hooks.do
    submitAttempts /\ tSubmitAttempts <- useState 0
    allTouched /\ tAllTouched <- useState false
    dirty /\ tDirty <- useState false
    valid /\ tValid <- useState Inva
    submitting /\ tSubmitting <- useState false

    Hooks.pure
      -- state
      { allTouched
      , errors: -- foldl (\acc next -> acc + (if isError next then 1 else 0) 0 fields
      , dirty: -- any (_.dirty) fields
      , valid: -- all (isSuccess <<< _.valid) fields
      , submitAttempts
      , submitting

      -- actions
      , resyncState -- recalculate state values to ensure they are accurate
                    -- maybe this should be done via useEvent?
      , validateAll: traverse_ (_.validate) fields
      , resetAll: traverse_ (_.reset) fields
      , submit
      }
  where
    resyncState = do
      Hooks.put tAllTouched (all (_.touched) fields)

    submit = do
      -- maybe this should build up a record of things...?
      -- otherwise, how will end user know which value
      -- refers to which?
      -- If so, field needs to story an SProxy so we can
      -- know which label to store its value as
