module Formless.Component where

import Prelude

import Control.Monad.Free (liftF)
import Data.Eq (class EqRecord)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse_)
import Data.Variant (Variant)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Formless.Data.FormFieldResult (FormFieldResult)
import Formless.Internal.Transform as Internal
import Formless.Types.Component (Component, HalogenM, ComponentHTML, Input, InternalState(..), Message(..), PublicState, Query(..), Action(..), State, ValidStatus(..))
import Formless.Types.Form (FormField, InputField, InputFunction, OutputField, U)
import Formless.Validation (Validation)
import Halogen as H
import Prim.Row as Row
import Prim.RowList as RL
import Record.Builder as Builder
import Unsafe.Coerce (unsafeCoerce)

-- | The Formless component
component
  :: forall form st query ps msg m is ixs ivs fs fxs us vs os ifs ivfs
   . MonadAff m
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
  => Newtype (form Record InputField) {| is }
  => Newtype (form Record InputFunction) {| ifs }
  => Newtype (form Record FormField) {| fs }
  => Newtype (form Record OutputField) {| os }
  => Newtype (form Record (Validation form m)) {| vs }
  => Newtype (form Variant InputField) (Variant ivs)
  => Newtype (form Variant InputFunction) (Variant ivfs)
  => Newtype (form Variant U) (Variant us)
  => Row.Lacks "validators" st
  => Row.Lacks "initialInputs" st
  => Row.Lacks "validity" st
  => Row.Lacks "dirty" st
  => Row.Lacks "errors" st
  => Row.Lacks "submitAttempts" st
  => Row.Lacks "submitting" st
  => Row.Lacks "form" st
  => Row.Lacks "internal" st
  => (State form st m -> ComponentHTML form query ps m)
  -> (forall a. query a -> HalogenM form st query ps msg m (Maybe a))
  -> (Message form msg -> HalogenM form st query ps msg m Unit)
  -> Component form st query ps msg m
component render handleExtraQuery handleMessage = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery handleExtraQuery handleMessage
      , handleAction = handleAction handleExtraQuery handleMessage
      , initialize = Just Initialize
      , receive = Just <<< Receive <<< _.validators
      }
  }
  where
  -- It's necessary to build from the original input because we have no idea
  -- what additional fields may have been provided by the user. This also incurs
  -- constraints on the component to ensure that the user ha 
  initialState :: Input form st m -> State form st m
  initialState input = Builder.build buildFull input
    where
    initialForm = Internal.inputFieldsToFormFields input.initialInputs
    internalState = InternalState
      { allTouched: false
      , initialInputs: input.initialInputs
      , validators: input.validators
      , debounceRef: Nothing
      , validationRef: Nothing
      }
    buildFull = 
      Builder.delete (SProxy :: _ "validators")
        >>> Builder.delete (SProxy :: _ "initialInputs")
        >>> Builder.insert (SProxy :: _ "validity") Incomplete
        >>> Builder.insert (SProxy :: _ "dirty") false
        >>> Builder.insert (SProxy :: _ "errors") 0
        >>> Builder.insert (SProxy :: _ "submitAttempts") 0
        >>> Builder.insert (SProxy :: _ "submitting") false
        >>> Builder.insert (SProxy :: _ "form") initialForm
	>>> Builder.insert (SProxy :: _ "internal") internalState

handleAction
  :: forall form st query ps msg m is ixs ivs fs fxs us vs os ifs ivfs
   . MonadAff m
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
  => Newtype (form Record InputField) {| is }
  => Newtype (form Record InputFunction) {| ifs }
  => Newtype (form Record FormField) {| fs }
  => Newtype (form Record OutputField) {| os }
  => Newtype (form Record (Validation form m)) {| vs }
  => Newtype (form Variant InputField) (Variant ivs)
  => Newtype (form Variant InputFunction) (Variant ivfs)
  => Newtype (form Variant U) (Variant us)
  => (forall a. query a -> HalogenM form st query ps msg m (Maybe a))
  -> (Message form msg -> HalogenM form st query ps msg m Unit)
  -> Action form query ps m
  -> HalogenM form st query ps msg m Unit
handleAction handleExtraQuery handleMessage = case _ of
  Initialize -> do
    dr <- H.liftEffect $ Ref.new Nothing
    vr <- H.liftEffect $ Ref.new Nothing
    let setFields rec = rec { debounceRef = Just dr, validationRef = Just vr }
    H.modify_ \st -> st { internal = over InternalState setFields st.internal }

  Receive validators -> do
    let applyOver = over InternalState (_ { validators = validators })
    H.modify_ \st -> st { internal = applyOver st.internal }

  -- An action to sync the overall state of the form after an individual field change
  -- or overall validation.
  SyncFormData -> do
    st <- H.get
    let 
      errors = Internal.countErrors st.form
      dirty = not $ eq
        (unwrap (Internal.formFieldsToInputFields st.form))
        (unwrap (unwrap st.internal).initialInputs)

    -- Need to verify the validity status of the form.
    newState <- case (unwrap st.internal).allTouched of
      true -> H.modify _
        { validity = if not (st.errors == 0) then Invalid else Valid
        , errors = errors
        , dirty = dirty
        }

      -- If not all fields are touched, then we need to quickly sync the form state
      -- to verify this is actually the case.
      _ -> case Internal.allTouched st.form of

        -- The sync revealed all fields really have been touched
        true -> H.modify _
          { validity = if not (st.errors == 0) then Invalid else Valid
          , internal = over InternalState (_ { allTouched = true }) st.internal
          , errors = errors
          , dirty = dirty
          }

        -- The sync revealed that not all fields have been touched
        _ -> H.modify _ { validity = Incomplete, errors = errors, dirty = dirty }

    raise' $ Changed $ getPublicState newState

  AsAction query ->
    void $ handleQuery handleExtraQuery handleMessage query

  where
  -- messages should be run by the user-provided handler first, and only
  -- afterwards raised to a parent component
  raise' msg = do
    void $ H.fork $ handleMessage msg
    raise' msg

handleQuery 
  :: forall form st query ps msg m a is ixs ivs fs fxs us vs os ifs ivfs
   . MonadAff m
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
  => Newtype (form Record InputField) {| is }
  => Newtype (form Record InputFunction) {| ifs }
  => Newtype (form Record FormField) {| fs }
  => Newtype (form Record OutputField) {| os }
  => Newtype (form Record (Validation form m)) {| vs }
  => Newtype (form Variant InputField) (Variant ivs)
  => Newtype (form Variant InputFunction) (Variant ivfs)
  => Newtype (form Variant U) (Variant us)
  => (forall b. query b -> HalogenM form st query ps msg m (Maybe b))
  -> (Message form msg -> HalogenM form st query ps msg m Unit)
  -> Query form query ps a 
  -> HalogenM form st query ps msg m (Maybe a)
handleQuery handleExtraQuery handleMessage = case _ of
  Modify variant a -> Just a <$ do
    H.modify_ \st -> st
      { form = Internal.unsafeModifyInputVariant identity variant st.form }
    handleAction handleExtraQuery handleMessage SyncFormData

  Validate variant a -> Just a <$ do
    st <- H.get
    let validators = (unwrap st.internal).validators
    form <- H.lift do
      Internal.unsafeRunValidationVariant variant validators st.form
    H.modify_ _ { form = form }
    handleAction handleExtraQuery handleMessage SyncFormData

  -- Provided as a separate query to minimize state updates / re-renders
  ModifyValidate milliseconds variant a -> Just a <$ do
    let
      modifyWith
        :: (forall e o. FormFieldResult e o -> FormFieldResult e o)
        -> HalogenM form st query ps msg m (form Record FormField)
      modifyWith f = do
        st <- H.modify \s -> s
          { form = Internal.unsafeModifyInputVariant f variant s.form }
        pure st.form

      validate = do
        st <- H.get
        let vs = (unwrap st.internal).validators
        form <- H.lift do 
          Internal.unsafeRunValidationVariant (unsafeCoerce variant) vs st.form
        H.modify_ _ { form = form }
        pure form

    case milliseconds of
      Nothing -> do
        void $ modifyWith identity
        void validate
        handleAction handleExtraQuery handleMessage SyncFormData
      Just ms -> do
        -- TODO debounceForm ms (modifyWith identity) (modifyWith (const Validating) *> validate)
	pure unit

  Reset variant a -> Just a <$ do
    H.modify_ \st -> st
      { form = Internal.unsafeModifyInputVariant identity variant st.form
      , internal = over InternalState (_ { allTouched = false }) st.internal
      }
    handleAction handleExtraQuery handleMessage SyncFormData

  SetAll formInputs a -> Just a <$ do
    new <- H.modify \st -> st
      { form = Internal.replaceFormFieldInputs formInputs st.form }
    raise' $ Changed $ getPublicState new
    handleAction handleExtraQuery handleMessage SyncFormData

  ModifyAll formInputs a -> Just a <$ do
    new <- H.modify \st -> st
      { form = Internal.modifyAll formInputs st.form }
    raise' $ Changed $ getPublicState new
    handleAction handleExtraQuery handleMessage SyncFormData

  ValidateAll a -> Just a <$ do
    st <- H.get
    form <- H.lift $ Internal.validateAll (unwrap st.internal).validators st.form
    H.modify_ _ { form = form }
    handleAction handleExtraQuery handleMessage SyncFormData

  -- Submit, also raising a message to the user
  Submit a -> Just a <$ do
    mbForm <- runSubmit
    traverse_ (raise' <<< Submitted) mbForm

  -- Submit, not raising a message
  SubmitReply reply -> do
    mbForm <- runSubmit
    pure $ Just $ reply mbForm

  -- Load a new set of form inputs
  LoadForm formInputs a -> Just a <$ do
    let setFields rec = rec { allTouched = false, initialInputs = formInputs }
    st <- H.get
    new <- H.modify _
      { validity = Incomplete
      , dirty = false
      , errors = 0
      , submitAttempts = 0
      , submitting = false
      , form = Internal.replaceFormFieldInputs formInputs st.form
      , internal = over InternalState setFields st.internal
      }
    raise' $ Changed $ getPublicState new

  -- | Should completely reset the form to its initial state
  ResetAll a -> Just a <$ do
    new <- H.modify \st -> st
      { validity = Incomplete
      , dirty = false
      , errors = 0
      , submitAttempts = 0
      , submitting = false
      , form =
	  Internal.replaceFormFieldInputs (unwrap st.internal).initialInputs st.form
      , internal = 
          over InternalState (_ { allTouched = false }) st.internal
      }
    raise' $ Changed $ getPublicState new

  GetState reply -> do
    st <- H.get
    pure $ Just $ reply $ getPublicState st

  SendQuery box -> 
    H.HalogenM $ liftF $ H.ChildQuery box

  Embed query ->
    handleExtraQuery query

  AndThen q1 q2 a -> Just a <$ do
    void $ handleQuery handleExtraQuery handleMessage q1
    void $ handleQuery handleExtraQuery handleMessage q2
  

  where
  -- messages should be run by the user-provided handler first, and only
  -- afterwards raised to a parent component
  raise' msg = do
    void $ H.fork $ handleMessage msg
    H.raise msg

  -- Run submission without raising messages or replies
  runSubmit :: HalogenM form st query ps msg m (Maybe (form Record OutputField))
  runSubmit = do
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

    -- Necessary to validate after fields are touched, but before parsing
    _ <- handleQuery handleExtraQuery handleMessage $ ValidateAll unit

    -- For performance purposes, only attempt to submit if the form is valid
    validated <- H.get
    H.modify_ _ { submitting = false }
    pure case validated.validity of
      Valid -> Internal.formFieldsToMaybeOutputFields validated.form
      _ -> Nothing

-- Remove internal fields and user-supplied fields to return the public state
getPublicState :: forall form st m. State form st m -> PublicState form
getPublicState st = 
  { validity: st.validity
  , dirty: st.dirty
  , submitting: st.submitting
  , errors: st.errors
  , submitAttempts: st.submitAttempts
  , form: st.form
  }

