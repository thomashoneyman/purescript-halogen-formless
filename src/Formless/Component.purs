module Formless.Component where

import Prelude

import Control.Monad.Free (liftF)
import Data.Eq (class EqRecord)
import Data.Functor.Variant as VF
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, match, inj, expand)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Formless.Action as FA
import Formless.Data.FormFieldResult (FormFieldResult(..))
import Formless.Internal.Transform as Internal
import Formless.Internal.Debounce (debounceForm)
import Formless.Types.Component (Action, Component, HalogenM, Input, InternalState(..), Message(..), PublicAction, PublicState, QueryF(..), Query, Spec, State, ValidStatus(..), InitialInputs(..))
import Formless.Types.Form (FormField, InputField, InputFunction, OutputField, U, FormProxy(..))
import Formless.Transform.Row (mkInputFields, class MakeInputFieldsFromRow)
import Formless.Validation (Validation)
import Halogen as H
import Halogen.HTML as HH
import Prim.Row as Row
import Prim.RowList as RL
import Record.Builder as Builder
import Unsafe.Coerce (unsafeCoerce)

-- | The default spec, which can be overridden by whatever functions you need 
-- | to extend the component. For example:
-- |
-- | ```purescript
-- | mySpec = F.defaultSpec { render = myRender }
-- | ```
defaultSpec :: forall form st query act ps msg m. Spec form st query act ps msg m
defaultSpec = 
  { render: const (HH.text mempty)
  , handleAction: const (pure unit)
  , handleQuery: const (pure Nothing)
  , handleMessage: const (pure unit)
  , receive: const Nothing
  , initialize: Nothing
  , finalize: Nothing
  }

-- | The Formless component, which takes a `spec` and provides a running form
-- | component from it.
component
  :: forall form st query act ps msg m is ixs ivs fs fxs us vs os ifs ivfs
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
  => MakeInputFieldsFromRow ixs is is
  => Newtype (form Record InputField) { | is }
  => Newtype (form Record InputFunction) { | ifs }
  => Newtype (form Record FormField) { | fs }
  => Newtype (form Record OutputField) { | os }
  => Newtype (form Record (Validation form m)) { | vs }
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
  => Spec form st query act ps msg m
  -> Component form st query ps msg m
component spec = H.mkComponent
  { initialState
  , render: getPublicState >>> spec.render
  , eval: H.mkEval $ H.defaultEval
      { handleQuery = \q -> handleQuery spec.handleQuery spec.handleMessage q
      , handleAction = \act -> handleAction spec.handleAction spec.handleMessage act
      , initialize = Just (inj (SProxy :: _ "initialize") spec.initialize)
      , receive = map (map FA.injAction) spec.receive
      , finalize = map FA.injAction spec.finalize
      }
  }
  where
  -- It's necessary to build from the original input because we have no idea
  -- what additional fields may have been provided by the user.
  initialState :: Input form st m -> State form st m
  initialState input = Builder.build pipeline input
    where
    initialInputs = case input.initialInputs of
      Defaults -> mkInputFields (FormProxy :: FormProxy form)
      Custom inputs -> inputs
    initialForm = Internal.inputFieldsToFormFields initialInputs
    internalState = InternalState
      { allTouched: false
      , initialInputs
      , validators: input.validators
      , debounceRef: Nothing
      , validationRef: Nothing
      }
    pipeline = 
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
  :: forall form st act ps msg m is ixs ivs fs fxs us vs os ifs ivfs
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
  => Newtype (form Record InputField) { | is }
  => Newtype (form Record InputFunction) { | ifs }
  => Newtype (form Record FormField) { | fs }
  => Newtype (form Record OutputField) { | os }
  => Newtype (form Record (Validation form m)) { | vs }
  => Newtype (form Variant InputField) (Variant ivs)
  => Newtype (form Variant InputFunction) (Variant ivfs)
  => Newtype (form Variant U) (Variant us)
  => Row.Lacks "internal" st
  => (act -> HalogenM form st act ps msg m Unit)
  -> (Message form st -> HalogenM form st act ps msg m Unit)
  -> Action form act
  -> HalogenM form st act ps msg m Unit
handleAction handleAction' handleMessage action = flip match action 
  { initialize: \mbAction -> do
      dr <- H.liftEffect $ Ref.new Nothing
      vr <- H.liftEffect $ Ref.new Nothing
      let setFields rec = rec { debounceRef = Just dr, validationRef = Just vr }
      H.modify_ \st -> st { internal = over InternalState setFields st.internal }
      traverse_ handleAction' mbAction

  , syncFormData: \_ -> do
      st <- H.get
      let 
        errors = Internal.countErrors st.form
        dirty = not $ eq
          (unwrap (Internal.formFieldsToInputFields st.form))
          (unwrap (unwrap st.internal).initialInputs)

      -- Need to verify the validity status of the form.
      newState <- case (unwrap st.internal).allTouched of
        true -> H.modify _
          { validity = if errors == 0 then Valid else Invalid
          , errors = errors
          , dirty = dirty
          }

        -- If not all fields are touched, then we need to quickly sync the form state
        -- to verify this is actually the case.
        _ -> case Internal.allTouched st.form of

          -- The sync revealed all fields really have been touched
          true -> H.modify _
            { validity = if errors == 0 then Valid else Invalid
            , internal = over InternalState (_ { allTouched = true }) st.internal
            , errors = errors
            , dirty = dirty
            }

          -- The sync revealed that not all fields have been touched
          _ -> H.modify _ { validity = Incomplete, errors = errors, dirty = dirty }

      handleMessage $ Changed $ getPublicState newState

  , userAction: \act -> 
      handleAction' act

  , modify: \variant ->  do
      H.modify_ \st -> st
        { form = Internal.unsafeModifyInputVariant identity variant st.form }
      handleAction handleAction' handleMessage sync

  , validate: \variant -> do
      st <- H.get
      let validators = (unwrap st.internal).validators
      form <- H.lift do
        Internal.unsafeRunValidationVariant variant validators st.form
      H.modify_ _ { form = form }
      handleAction handleAction' handleMessage sync

  , modifyValidate: \(Tuple milliseconds variant) -> do
      let
        modifyWith
          :: (forall e o. FormFieldResult e o -> FormFieldResult e o)
          -> HalogenM form st act ps msg m (form Record FormField)
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
        Nothing -> 
          modifyWith identity *> validate *> handleAction handleAction' handleMessage sync
        Just ms ->
          debounceForm
            ms
            (modifyWith identity)
            (modifyWith (const Validating) *> validate)
            (handleAction handleAction' handleMessage sync)

  , reset: \variant -> do
      H.modify_ \st -> st
        { form = Internal.unsafeModifyInputVariant identity variant st.form
        , internal = over InternalState (_ { allTouched = false }) st.internal
        }
      handleAction handleAction' handleMessage sync

  , setAll: \(Tuple formInputs shouldValidate) -> do
      new <- H.modify \st -> st
        { form = Internal.replaceFormFieldInputs formInputs st.form }
      handleMessage $ Changed $ getPublicState new
      case shouldValidate of
        true -> handleAction handleAction' handleMessage FA.validateAll
        _ -> handleAction handleAction' handleMessage sync

  , modifyAll: \(Tuple formInputs shouldValidate) -> do
      new <- H.modify \st -> st
        { form = Internal.modifyAll formInputs st.form }
      handleMessage $ Changed $ getPublicState new
      case shouldValidate of
        true -> handleAction handleAction' handleMessage FA.validateAll
        _ -> handleAction handleAction' handleMessage sync

  , validateAll: \_ -> do
      st <- H.get
      form <- H.lift $ Internal.validateAll (unwrap st.internal).validators st.form
      H.modify_ _ { form = form }
      handleAction handleAction' handleMessage sync

  , resetAll: \_ -> do
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
      handleMessage $ Changed $ getPublicState new

  , submit: \_ -> do
      _ <- preSubmit 
      _ <- handleAction handleAction' handleMessage FA.validateAll 
      submit >>= traverse_ (Submitted >>> handleMessage)

  , loadForm: \formInputs -> do
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
      handleMessage $ Changed $ getPublicState new
  }
  where
  sync :: Action form act 
  sync = inj (SProxy :: SProxy "syncFormData") unit

handleQuery 
  :: forall form st query act ps msg m a is ixs ivs fs fxs us vs os ifs ivfs
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
  => Newtype (form Record InputField) { | is }
  => Newtype (form Record InputFunction) { | ifs }
  => Newtype (form Record FormField) { | fs }
  => Newtype (form Record OutputField) { | os }
  => Newtype (form Record (Validation form m)) { | vs }
  => Newtype (form Variant InputField) (Variant ivs)
  => Newtype (form Variant InputFunction) (Variant ivfs)
  => Newtype (form Variant U) (Variant us)
  => Row.Lacks "internal" st
  => (forall b. query b -> HalogenM form st act ps msg m (Maybe b))
  -> (Message form st -> HalogenM form st act ps msg m Unit)
  -> Query form query ps a 
  -> HalogenM form st act ps msg m (Maybe a)
handleQuery handleQuery' handleMessage = VF.match
  { query: case _ of
      SubmitReply reply -> do
        _ <- preSubmit 
        _ <- handleAction (const (pure unit)) handleMessage FA.validateAll 
        mbForm <- submit 
        pure $ Just $ reply mbForm

      SendQuery box -> 
        H.HalogenM $ liftF $ H.ChildQuery box

      AsQuery (act :: Variant (PublicAction form)) a -> Just a <$ 
        handleAction 
          (const (pure unit)) 
          handleMessage
          ((expand act) :: Action form act)

  , userQuery: \q -> handleQuery' q
  }


-- INTERNAL

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
