module Formless.Component where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (store)
import Control.Monad.Free (liftF)
import Data.Coyoneda (liftCoyoneda)
import Data.Eq (class EqRecord)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse_)
import Data.Variant (Variant)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Formless.Data.FormFieldResult (FormFieldResult(..))
import Formless.Internal.Debounce (debounceForm)
import Formless.Internal.Transform as Internal
import Formless.Types.Component (Component, DSL, Input, InternalState(..), Message(..), PublicState, Query(..), State, StateStore, ValidStatus(..))
import Formless.Types.Form (FormField, InputField, InputFunction, OutputField, U)
import Formless.Validation (Validation)
import Halogen as H
import Halogen.HTML.Events as HE
import Prim.RowList as RL
import Record as Record
import Renderless.State (getState, modifyState, modifyState_, modifyStore_)
import Unsafe.Coerce (unsafeCoerce)

-- | The Formless component
component
  :: ∀ pq cq cs form m is ixs ivs fs fxs us vs os ifs ivfs
   . Ord cs
  => MonadAff m
  => RL.RowToList is ixs
  => RL.RowToList fs fxs
  => EqRecord ixs is
  => Internal.InputFieldsToFormFields ixs is fs
  => Internal.FormFieldsToInputFields fxs fs is
  => Internal.CountErrors fxs fs
  => Internal.AllTouched fxs fs
  => Internal.SetFormFieldsTouched fxs fs fs
  => Internal.ReplaceFormFieldInputs is fxs fs fs
  => Internal.ModifyAll ifs fxs fs fs
  => Internal.ValidateAll vs fxs fs fs m
  => Internal.FormFieldToMaybeOutput fxs fs os
  => Newtype (form Record InputField) { | is }
  => Newtype (form Record InputFunction) { | ifs }
  => Newtype (form Record FormField) { | fs }
  => Newtype (form Record OutputField) { | os }
  => Newtype (form Record (Validation form m)) { | vs }
  => Newtype (form Variant InputField) (Variant ivs)
  => Newtype (form Variant InputFunction) (Variant ivfs)
  => Newtype (form Variant U) (Variant us)
  => Component pq cq cs form m
component =
  H.lifecycleParentComponent
    { initialState
    , render: extract
    , eval
    , receiver: HE.input Receive
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }
  where

  initialState :: Input pq cq cs form m -> StateStore pq cq cs form m
  initialState { initialInputs, validators, render } = store render $
    { validity: Incomplete
    , dirty: false
    , errors: 0
    , submitAttempts: 0
    , submitting: false
    , form: Internal.inputFieldsToFormFields initialInputs
    , internal: InternalState
        { allTouched: false
        , initialInputs
        , validators
        , debounceRef: Nothing
        , validationRef: Nothing
        }
    }

  eval :: Query pq cq cs form m ~> DSL pq cq cs form m
  eval = case _ of
    Initialize a -> do
      dr <- H.liftEffect $ Ref.new Nothing
      vr <- H.liftEffect $ Ref.new Nothing
      modifyState_ \st -> st
        { internal = over InternalState 
            (_ 
              { debounceRef = Just dr
              , validationRef = Just vr
              }
            )
            st.internal 
        }
      pure a

    Modify variant a -> do
      modifyState_ \st -> st
        { form = Internal.unsafeModifyInputVariant identity variant st.form }
      eval $ SyncFormData a

    Validate variant a -> do
      st <- getState
      form <- H.lift
        $ Internal.unsafeRunValidationVariant variant (unwrap st.internal).validators st.form
      modifyState_ _ { form = form }
      eval $ SyncFormData a

    -- Provided as a separate query to minimize state updates / re-renders
    ModifyValidate milliseconds variant a -> do
      let
        modifyWith
          :: (forall e o. FormFieldResult e o -> FormFieldResult e o)
          -> DSL pq cq cs form m (form Record FormField)
        modifyWith f = do
          s <- modifyState \st -> st { form = Internal.unsafeModifyInputVariant f variant st.form }
          pure s.form

        validate = do
          st <- getState
          let vs = (unwrap st.internal).validators
          form <- H.lift $ Internal.unsafeRunValidationVariant (unsafeCoerce variant) vs st.form
          modifyState_ _ { form = form }
          pure form

      case milliseconds of
        Nothing -> do
          _ <- modifyWith identity
          _ <- validate
          eval (SyncFormData a)
        Just ms -> do
          debounceForm
            ms
            (modifyWith identity)
            (modifyWith (const Validating) *> validate)
            (eval $ SyncFormData a)
          pure a

    Reset variant a -> do
      modifyState_ \st -> st
        { form = Internal.unsafeModifyInputVariant identity variant st.form
        , internal = over InternalState (_ { allTouched = false }) st.internal
        }
      eval $ SyncFormData a

    SetAll formInputs a -> do
      new <- modifyState \st -> st
        { form = Internal.replaceFormFieldInputs formInputs st.form }
      H.raise $ Changed $ getPublicState new
      eval $ SyncFormData a

    ModifyAll formInputs a -> do
      new <- modifyState \st -> st
        { form = Internal.modifyAll formInputs st.form }
      H.raise $ Changed $ getPublicState new
      eval $ SyncFormData a

    ValidateAll a -> do
      st <- getState
      form <- H.lift $ Internal.validateAll (unwrap st.internal).validators st.form
      modifyState_ _ { form = form }
      eval $ SyncFormData a

    -- A query to sync the overall state of the form after an individual field change
    -- or overall validation.
    SyncFormData a -> do
      st <- getState

      let errors = Internal.countErrors st.form
          dirty = not $ eq
            (unwrap (Internal.formFieldsToInputFields st.form))
            (unwrap (unwrap st.internal).initialInputs)

      -- Need to verify the validity status of the form.
      newState <- case (unwrap st.internal).allTouched of
        true -> modifyState _
          { validity = if not (st.errors == 0) then Invalid else Valid
          , errors = errors
          , dirty = dirty
          }

        -- If not all fields are touched, then we need to quickly sync the form state
        -- to verify this is actually the case.
        _ -> case Internal.allTouched st.form of

          -- The sync revealed all fields really have been touched
          true -> modifyState _
            { validity = if not (st.errors == 0) then Invalid else Valid
            , internal = over InternalState (_ { allTouched = true }) st.internal
            , errors = errors
            , dirty = dirty
            }

          -- The sync revealed that not all fields have been touched
          _ -> modifyState _ { validity = Incomplete, errors = errors, dirty = dirty }

      H.raise $ Changed $ getPublicState newState
      pure a

    -- Submit, also raising a message to the user
    Submit a -> do
      mbForm <- runSubmit
      traverse_ (H.raise <<< Submitted) mbForm
      pure a

    -- Submit, not raising a message
    SubmitReply reply -> do
      mbForm <- runSubmit
      pure $ reply mbForm

    -- | Should completely reset the form to its initial state
    ResetAll a -> do
      new <- modifyState \st -> st
        { validity = Incomplete
        , dirty = false
        , errors = 0
        , submitAttempts = 0
        , submitting = false
        , form = Internal.replaceFormFieldInputs (unwrap st.internal).initialInputs st.form
        , internal = over InternalState (_ { allTouched = false }) st.internal
        }
      H.raise $ Changed $ getPublicState new
      pure a

    GetState reply -> do
      st <- getState
      pure $ reply $ getPublicState st

    Send cs cq -> H.HalogenM $ liftF $ H.ChildQuery cs $ liftCoyoneda cq

    Raise query a -> do
      H.raise (Emit query)
      pure a

    LoadForm formInputs a -> do
      st <- getState
      new <- modifyState _
        { validity = Incomplete
        , dirty = false
        , errors = 0
        , submitAttempts = 0
        , submitting = false
        , form = Internal.replaceFormFieldInputs formInputs st.form
        , internal = over
            InternalState
            (_
              { allTouched = false
              , initialInputs = formInputs
              }
            )
            st.internal
        }

      H.raise $ Changed $ getPublicState new
      pure a

    Receive { render, validators } a -> do
      let applyOver = over InternalState (_ { validators = validators })
      modifyStore_ render (\st -> st { internal = applyOver st.internal })
      pure a

    AndThen q1 q2 a -> do
      void (eval q1)
      void (eval q2)
      pure a

  -- Remove internal fields and return the public state
  getPublicState :: State form m -> PublicState form
  getPublicState = Record.delete (SProxy :: SProxy "internal")

  -- Run submission without raising messages or replies
  runSubmit :: DSL pq cq cs form m (Maybe (form Record OutputField))
  runSubmit = do
    init <- modifyState \st -> st
      { submitAttempts = st.submitAttempts + 1
      , submitting = true
      }

    -- For performance purposes, avoid running this if possible
    let internal = unwrap init.internal
    when (not internal.allTouched) do
      modifyState_ _
       { form = Internal.setFormFieldsTouched init.form
       , internal = over InternalState (_ { allTouched = true }) init.internal
       }

    -- Necessary to validate after fields are touched, but before parsing
    _ <- eval $ ValidateAll unit

    -- For performance purposes, only attempt to submit if the form is valid
    validated <- getState
    modifyState_ \st -> st { submitting = false }
    pure $
      if validated.validity == Valid
        then Internal.formFieldsToMaybeOutputFields validated.form
        else Nothing
