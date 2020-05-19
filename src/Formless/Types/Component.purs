module Formless.Types.Component where

import Prelude

import Data.Eq (class EqRecord)
import Data.Foldable (traverse_)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant)
import Effect.Aff (Fiber, Milliseconds)
import Effect.Aff.AVar (AVar)
import Formless.Class.Initial (class Initial)
import Formless.Data.FormFieldResult (FormFieldResult)
import Formless.Internal.Transform (class ModifyAll, class ReplaceFormFieldInputs)
import Formless.Internal.Transform as IT
import Formless.Transform.Row (class MakeInputFieldsFromRow, mkInputFields)
import Formless.Types.Form (FormField, FormProxy(..), InputField, InputFunction, OutputField, U)
import Formless.Validation (Validation)
import Halogen as H
import Halogen.Hooks (Hook, HookM, UseRef, UseState, useRef, useState)
import Halogen.Hooks as Hooks
import Halogen.Query.HalogenM (ForkId)
import Prim.RowList as RL
import Unsafe.Coerce (unsafeCoerce)

type PublicAction form =
  ( modify :: form Variant InputFunction
  , validate :: form Variant U
  , modifyValidate :: Tuple (Maybe Milliseconds) (form Variant InputFunction)
  , reset :: form Variant InputFunction
  , setAll :: Tuple (form Record InputField) Boolean
  , modifyAll :: Tuple (form Record InputFunction) Boolean
  , validateAll :: Unit
  , resetAll :: Unit
  , submit :: Unit
  , loadForm :: form Record InputField
  )

-- | A type to represent a running debouncer
type Debouncer =
  { var :: AVar Unit
  , fiber :: Fiber Unit
  , forkId :: ForkId
  }

-- | A type to represent validation status
data ValidStatus
  = Invalid
  | Incomplete
  | Valid

derive instance genericValidStatus :: Generic ValidStatus _
derive instance eqValidStatus :: Eq ValidStatus
derive instance ordValidStatus :: Ord ValidStatus

instance showValidStatus :: Show ValidStatus where
  show = genericShow

-- | The component tries to require as few messages to be handled as possible. You
-- | can always use the *Reply variants of queries to perform actions and receive
-- | a result out the other end, or extend these messages.
data FormlessEvent form
  = Submitted (form Record OutputField)
  | Changed (FormlessState form)

-- | A convenience export of formless as a symbol for use when mounting Formless
-- | as a child component
-- |
-- | ```purescript
-- | type ChildSlots = (formless :: F.Slot' Form FormResult)
-- | HH.slot F._formless unit (F.component spec) input handler
-- | ```
_formless = SProxy :: SProxy "formless"

-- | The hook's input type. If you provide `Nothing` as your `initialInputs`,
-- | then the form will fill in values based on the `Initial` type class for the
-- | field's input type. Otherwise, the form will contain the values you provide.
-- |
-- | Validators can be created using the Formless.Validation module.
type FormlessInput form m =
  { initialInputs :: Maybe (form Record InputField)
  , validators :: form Record (Validation form m)
  , pushChange :: FormlessState form -> HookM m Unit
  , pushSubmitted :: form Record OutputField -> HookM m Unit
  }

type FormlessState form =
  { validity :: ValidStatus
  , dirty :: Boolean
  , submitting :: Boolean
  , errors :: Int
  , submitAttempts :: Int
  , form :: form Record FormField
  }

type InternalState form =
  { allTouched :: Boolean
  -- , validationRef - use ref; outer Maybe only used for initial Ref value
  --                   due to NOT using `unsafePerformEffect $ liftEffect Ref.new`
  , initialInputs :: form Record InputField
  -- validators is in-scope via FormlessInput
  -- debounceRef can be reimplemented via useDebouncer
  }

newtype UseFormless form hooks = UseFormless
  (UseRef (Maybe H.ForkId)
  (UseState (InternalState form)
  (UseState (FormlessState form)
  hooks)))

derive instance newtypeUseFormless :: Newtype (UseFormless form hooks) _

useFormless
  :: forall form m inputFields formFields inputFieldsRowList variantInputFunction variantUnit validationFields
   . Monad m
  => Newtype (form Record InputField) { | inputFields }
  => Newtype (form Record FormField) { | formFields }
  => Newtype (form Variant InputFunction) (Variant variantInputFunction)
  => Newtype (form Variant U) (Variant variantUnit)
  => Newtype (form Record (Validation form m)) { | validationFields }

  => MakeInputFieldsFromRow inputFieldsRowList inputFields inputFields
  => IT.InputFieldsToFormFields inputFieldsRowList inputFields formFields
  => RL.RowToList inputFields inputFieldsRowList
  => RL.RowToList formFields inputFieldsRowList
  => FormlessInput form m
  -> Hook m (UseFormless form) Unit
useFormless inputRec =
  let
    providedInitialInputs :: form Record InputField
    providedInitialInputs = case inputRec.initialInputs of
      Nothing -> mkInputFields (FormProxy :: FormProxy form)
      Just inputs -> inputs

    initialForm :: form Record FormField
    initialForm = IT.inputFieldsToFormFields providedInitialInputs

  in Hooks.wrap Hooks.do
    public /\ publicId <- useState
      { validity: Incomplete
      , dirty: false
      , errors: 0
      , submitAttempts: 0
      , submitting: false
      , form: initialForm
      }
    internal /\ internalId <- useState
      { allTouched: false
      , initialInputs: providedInitialInputs
      }
    _ /\ validationRef <- useRef Nothing

    let
      syncFormData
        :: Newtype (form Record FormField) { | formFields }
        => Newtype (form Record InputField) { | inputFields }
        => RL.RowToList formFields inputFieldsRowList
        => IT.FormFieldsToInputFields inputFieldsRowList formFields inputFields
        => IT.CountErrors inputFieldsRowList formFields
        => EqRecord inputFieldsRowList inputFields
        => IT.AllTouched inputFieldsRowList formFields
        => HookM m Unit
      syncFormData = do
        st' <- Hooks.get publicId
        internal' <- Hooks.get internalId
        let
          errors = IT.countErrors st'.form
          dirty = not $ eq
            (unwrap (IT.formFieldsToInputFields st'.form))
            (unwrap internal.initialInputs)

        -- Need to verify the validity status of the form.
        newState <- case internal'.allTouched of
          true -> Hooks.modify publicId \rec -> rec
            { validity = if errors == 0 then Valid else Invalid
            , errors = errors
            , dirty = dirty
            }

          -- If not all fields are touched, then we need to quickly sync the form state
          -- to verify this is actually the case.
          _ -> case IT.allTouched st'.form of

            -- The sync revealed all fields really have been touched
            true -> do
              Hooks.modify_ internalId (_ { allTouched = true })
              Hooks.modify publicId \rec -> rec
                { validity = if errors == 0 then Valid else Invalid
                , errors = errors
                , dirty = dirty
                }

            -- The sync revealed that not all fields have been touched
            _ -> do
              Hooks.modify publicId \rec -> rec
                { validity = Incomplete
                , errors = errors
                , dirty = dirty
                }

        inputRec.pushChange newState

      modify
        :: Newtype (form Variant InputFunction) (Variant variantInputFunction)
        => Newtype (form Record FormField) { | formFields }
        => Newtype (form Record InputField) { | inputFields }
        => RL.RowToList formFields inputFieldsRowList
        => IT.FormFieldsToInputFields inputFieldsRowList formFields inputFields
        => IT.CountErrors inputFieldsRowList formFields
        => EqRecord inputFieldsRowList inputFields
        => IT.AllTouched inputFieldsRowList formFields
        => form Variant InputFunction
        -> HookM m Unit
      modify variant = do
        Hooks.modify_ publicId \st -> st
          { form = IT.unsafeModifyInputVariant identity variant st.form }
        syncFormData

      validate
        :: Newtype (form Variant U) (Variant variantUnit)
        => Newtype (form Record FormField) { | formFields }
        => Newtype (form Record (Validation form m)) { | validationFields }
        => Newtype (form Record InputField) { | inputFields }
        => RL.RowToList formFields inputFieldsRowList
        => IT.FormFieldsToInputFields inputFieldsRowList formFields inputFields
        => IT.CountErrors inputFieldsRowList formFields
        => EqRecord inputFieldsRowList inputFields
        => IT.AllTouched inputFieldsRowList formFields
        => form Variant U
        -> HookM m Unit
      validate variant = do
        st <- Hooks.get publicId
        formProcessor <- H.lift do
          IT.unsafeRunValidationVariant variant inputRec.validators st.form
        st' <- Hooks.get publicId
        Hooks.modify_ publicId (_ { form = formProcessor st'.form })
        syncFormData

      modifyValidate
        :: Newtype (form Variant InputFunction) (Variant variantInputFunction)
        => Newtype (form Variant U) (Variant variantUnit)
        => Newtype (form Record (Validation form m)) { | validationFields }
        => Newtype (form Record FormField) { | formFields }
        => Newtype (form Record InputField) { | inputFields }
        => RL.RowToList formFields inputFieldsRowList
        => IT.FormFieldsToInputFields inputFieldsRowList formFields inputFields
        => IT.CountErrors inputFieldsRowList formFields
        => EqRecord inputFieldsRowList inputFields
        => IT.AllTouched inputFieldsRowList formFields
        => Tuple (Maybe Milliseconds) (form Variant InputFunction)
        -> HookM m Unit
      modifyValidate (Tuple milliseconds variant) = do
        let
          modifyWith
            :: (forall e o. FormFieldResult e o -> FormFieldResult e o)
            -> HookM m (form Record FormField)
          modifyWith f = do
            st <- Hooks.modify publicId \s -> s
              { form = IT.unsafeModifyInputVariant f variant s.form }
            pure st.form

          runValidate = do
            st <- Hooks.get publicId
            let vs = inputRec.validators
            formProcessor <- H.lift do
              IT.unsafeRunValidationVariant (unsafeCoerce variant) vs st.form
            st' <- Hooks.get publicId
            let newForm = formProcessor st'.form
            Hooks.modify_ publicId (_ { form = newForm })
            pure newForm

        case milliseconds of
          Nothing ->
            modifyWith identity *> runValidate *> syncFormData
          Just ms ->
            syncFormData
            -- TODO: use debouncer hook to handle this
            -- debounceForm
            --   ms
            --   (modifyWith identity)
            --   (modifyWith (const Validating) *> validate)
            --   (syncFormData)

      reset
        :: forall i
         . Initial i
        => Newtype (form Variant InputFunction) (Variant variantInputFunction)
        => Newtype (form Record FormField) { | formFields }
        => Newtype (form Record InputField) { | inputFields }
        => RL.RowToList formFields inputFieldsRowList
        => IT.FormFieldsToInputFields inputFieldsRowList formFields inputFields
        => IT.CountErrors inputFieldsRowList formFields
        => EqRecord inputFieldsRowList inputFields
        => IT.AllTouched inputFieldsRowList formFields
        => form Variant InputFunction
        -> HookM m Unit
      reset variant = do
        Hooks.modify_ publicId \st -> st
          { form = IT.unsafeModifyInputVariant identity variant st.form }
        Hooks.modify_ internalId (_ { allTouched = false })
        syncFormData

      setAll
        :: ReplaceFormFieldInputs inputFields inputFieldsRowList formFields formFields
        => Newtype (form Record InputField) { | inputFields }
        => IT.ValidateAll validationFields inputFieldsRowList formFields formFields m
        => Newtype (form Record (Validation form m)) { | validationFields }
        => Newtype (form Record FormField) { | formFields }
        => Newtype (form Record InputField) { | inputFields }
        => RL.RowToList formFields inputFieldsRowList
        => IT.FormFieldsToInputFields inputFieldsRowList formFields inputFields
        => IT.CountErrors inputFieldsRowList formFields
        => EqRecord inputFieldsRowList inputFields
        => IT.AllTouched inputFieldsRowList formFields
        => Tuple (form Record InputField) Boolean
        -> HookM m Unit
      setAll (Tuple formInputs shouldValidate) = do
        new <- Hooks.modify publicId \st -> st
          { form = IT.replaceFormFieldInputs formInputs st.form }
        inputRec.pushChange new
        case shouldValidate of
          true -> validateAll
          _ -> syncFormData

      modifyAll
        :: forall inputFunctionFields
         . ModifyAll inputFunctionFields inputFieldsRowList formFields formFields
        => Newtype (form Record InputFunction) { | inputFunctionFields }
        => IT.ValidateAll validationFields inputFieldsRowList formFields formFields m
        => Newtype (form Record (Validation form m)) { | validationFields }
        => Newtype (form Record FormField) { | formFields }
        => Newtype (form Record InputField) { | inputFields }
        => RL.RowToList formFields inputFieldsRowList
        => IT.FormFieldsToInputFields inputFieldsRowList formFields inputFields
        => IT.CountErrors inputFieldsRowList formFields
        => EqRecord inputFieldsRowList inputFields
        => IT.AllTouched inputFieldsRowList formFields
        => Tuple (form Record InputFunction) Boolean
        -> HookM m Unit
      modifyAll (Tuple formInputs shouldValidate) = do
          new <- Hooks.modify publicId \st -> st
            { form = IT.modifyAll formInputs st.form }
          inputRec.pushChange new
          case shouldValidate of
            true -> validateAll
            _ -> syncFormData

      validateAll
        :: IT.ValidateAll validationFields inputFieldsRowList formFields formFields m
        => Newtype (form Record (Validation form m)) { | validationFields }
        => Newtype (form Record FormField) { | formFields }
        => Newtype (form Record InputField) { | inputFields }
        => RL.RowToList formFields inputFieldsRowList
        => IT.FormFieldsToInputFields inputFieldsRowList formFields inputFields
        => IT.CountErrors inputFieldsRowList formFields
        => EqRecord inputFieldsRowList inputFields
        => IT.AllTouched inputFieldsRowList formFields
        => HookM m Unit
      validateAll = do
        st <- Hooks.get publicId
        form <- H.lift $ IT.validateAll inputRec.validators st.form
        Hooks.modify_ publicId (_ { form = form })
        syncFormData

      resetAll
        :: ReplaceFormFieldInputs inputFields inputFieldsRowList formFields formFields
        => Newtype (form Record InputField) { | inputFields }
        => Newtype (form Record FormField) { | formFields }
        => HookM m Unit
      resetAll = do
        internal' <- Hooks.get internalId
        new <- Hooks.modify publicId \st -> st
          { validity = Incomplete
          , dirty = false
          , errors = 0
          , submitAttempts = 0
          , submitting = false
          , form = IT.replaceFormFieldInputs internal'.initialInputs st.form
          }
        Hooks.modify_ internalId (_ { allTouched = false })
        inputRec.pushChange new

      submit
        :: forall outputFields
         . IT.AllTouched inputFieldsRowList formFields
        => IT.SetFormFieldsTouched inputFieldsRowList formFields formFields
        => IT.ValidateAll validationFields inputFieldsRowList formFields formFields m
        => IT.FormFieldToMaybeOutput inputFieldsRowList formFields outputFields
        => Newtype (form Record OutputField) { | outputFields }
        => Newtype (form Record (Validation form m)) { | validationFields }
        => Newtype (form Record FormField) { | formFields }
        => Newtype (form Record InputField) { | inputFields }
        => RL.RowToList formFields inputFieldsRowList
        => IT.FormFieldsToInputFields inputFieldsRowList formFields inputFields
        => IT.CountErrors inputFieldsRowList formFields
        => EqRecord inputFieldsRowList inputFields
        => IT.AllTouched inputFieldsRowList formFields
        => HookM m Unit
      submit = do
        -- preSubmit
        init <- Hooks.modify publicId \st -> st
          { submitAttempts = st.submitAttempts + 1
          , submitting = true
          }

        -- For performance purposes, avoid running this if possible
        internal' <- Hooks.get internalId
        when (not internal'.allTouched) do
          Hooks.modify_ publicId
            (_ { form = IT.setFormFieldsTouched init.form })
          Hooks.modify_ internalId (_ { allTouched = true })

        -- validateAll
        _ <- validateAll

        -- submit
        st' <- Hooks.get publicId
        Hooks.modify_ publicId _ { submitting = false }

        traverse_ inputRec.pushSubmitted case st'.validity of
          Valid -> IT.formFieldsToMaybeOutputFields st'.form
          _ -> Nothing

      loadForm
        :: forall inputFields'
         . Newtype (form Record InputField) { | inputFields' }
        => ReplaceFormFieldInputs inputFields inputFieldsRowList formFields formFields
        => form Record InputField
        -> HookM m Unit
      loadForm formInputs = do
        new <- Hooks.modify publicId \st -> st
          { validity = Incomplete
          , dirty = false
          , errors = 0
          , submitAttempts = 0
          , submitting = false
          , form = IT.replaceFormFieldInputs formInputs st.form
          }
        Hooks.put internalId { allTouched: false, initialInputs: formInputs }
        inputRec.pushChange new

    Hooks.pure unit
