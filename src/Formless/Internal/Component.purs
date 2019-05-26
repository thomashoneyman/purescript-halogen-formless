module Formless.Internal.Component where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Formless.Internal.Transform as Internal
import Formless.Types.Component (HalogenM, InternalState(..), PublicState, State, ValidStatus(..))
import Formless.Types.Form (FormField, OutputField)
import Formless.Validation (Validation)
import Halogen as H
import Prim.Row as Row
import Prim.RowList as RL
import Record.Builder as Builder

-- Remove internal fields and user-supplied fields to return the public state
getPublicState
  :: forall form st m
   . Row.Lacks "internal" st
  => State form st m
  -> PublicState form st
getPublicState = Builder.build (Builder.delete (SProxy :: SProxy "internal"))

preSubmit
  :: forall form st act ps msg m fs fxs os vs
   . MonadAff m
  => RL.RowToList fs fxs
  => Internal.AllTouched fxs fs
  => Internal.SetFormFieldsTouched fxs fs fs
  => Internal.ValidateAll vs fxs fs fs m
  => Internal.FormFieldToMaybeOutput fxs fs os
  => Internal.ValidateAll vs fxs fs fs m
  => Newtype (form Record FormField) { | fs }
  => Newtype (form Record OutputField) { | os }
  => Newtype (form Record (Validation form m)) { | vs }
  => HalogenM form st act ps msg m Unit
preSubmit = do
  init <- H.modify \st -> st
    { submitAttempts = st.submitAttempts + 1
    , submitting = true
    }

  -- For performance purposes, avoid running this if possible
  let internal = unwrap init.internal
  when (not internal.allTouched) do
    H.modify_ _
      { form = Internal.setFormFieldsTouched init.form
      , internal = over InternalState (_ { allTouched = true }) init.internal
      }

submit
  :: forall form st act ps msg m fs fxs os vs
   . MonadAff m
  => RL.RowToList fs fxs
  => Internal.AllTouched fxs fs
  => Internal.SetFormFieldsTouched fxs fs fs
  => Internal.ValidateAll vs fxs fs fs m
  => Internal.FormFieldToMaybeOutput fxs fs os
  => Internal.ValidateAll vs fxs fs fs m
  => Newtype (form Record FormField) { | fs }
  => Newtype (form Record OutputField) { | os }
  => Newtype (form Record (Validation form m)) { | vs }
  => HalogenM form st act ps msg m (Maybe (form Record OutputField))
submit = do
  -- For performance purposes, only attempt to submit if the form is valid
  validated <- H.get
  H.modify_ _ { submitting = false }

  pure case validated.validity of
    Valid -> Internal.formFieldsToMaybeOutputFields validated.form
    _ -> Nothing
