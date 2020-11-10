module Halogen.Hooks.Formless
  ( useForm
  , useFormState
  , useFormInputs
  , UseForm
  , FormState
  , UseFormResult
  , buildForm
  , FormInput(..)
  , FormInputProps
  , WithInput
  , WithValue
  , UseFormInput
  , class BuildForm
  , BuildFormInput
  , UseBuildForm
  , buildFormStep
  , BuildFormFor
  , initialFormState
  , InitialFormState
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple.Nested ((/\), type (/\))
import Halogen as H
import Halogen.Hooks (Hook, HookM, StateId)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Hook (HProxy)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Row as Row
import Prim.RowList (class RowToList, kind RowList)
import Prim.RowList as RowList
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.RowList (RLProxy(..))
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

type FormInputProps m i =
  { onChange :: i -> HookM m Unit
  , value :: Maybe i
  , touched :: Boolean
  }

newtype FormInput m h ro i o =
  FormInput (FormInputProps m i -> Hook m h { | WithValue o ro })

type WithInput slots m r = (input :: H.ComponentHTML (HookM m Unit) slots m | r)
type WithValue o r = (value :: Maybe o | r)

type BuildFormInputProps m form =
  { form :: { | form }
  , modifyForm :: ({ | form } -> { | form }) -> HookM m Unit
  }

newtype BuildFormInput (closed :: # Type) form m h fields value =
  BuildFormInput
    (BuildFormInputProps m form
      -> Hook m h { fields :: { | fields }, value :: Maybe { | value }})

type UseFormInput' h1 = h1 Hooks.<> Hooks.Pure

foreign import data UseFormInput :: Hooks.HookType -> Hooks.HookType

instance newtypeUseFormInput
  :: Hooks.HookEquals x (UseFormInput' h1)
  => Hooks.HookNewtype (UseFormInput h1) x

buildFormInput
  :: forall sym m h ro i o form' form closed fields value
   . IsSymbol sym
  => Row.Cons sym { state :: Maybe i, touched :: Boolean } form' form
  => Row.Cons sym { state :: Maybe i, touched :: Boolean } () closed
  => Row.Cons sym { | WithValue o ro } () fields
  => Row.Cons sym o () value
  => Row.Lacks sym ()
  => SProxy sym
  -> FormInput m h ro i o
  -> BuildFormInput closed form m (UseFormInput h) fields value
buildFormInput sym (FormInput formInput) =
  BuildFormInput \{ form, modifyForm } -> Hooks.wrap Hooks.do
    let
      { state, touched } = Record.get sym form
      onChange = modifyForm <<< Record.set sym <<< { state: _, touched: true } <<< Just

    result <- formInput { onChange, value: state, touched }

    let
      fields = Record.insert sym result {}
      value = flip (Record.insert sym) {} <$> result.value

    Hooks.pure { fields, value }

type UseMergedFormInputs' h1 h2 =
  h1
    Hooks.<> h2
    Hooks.<> Hooks.Pure

foreign import data UseMergedFormInputs :: Hooks.HookType -> Hooks.HookType -> Hooks.HookType

instance newtypeUseMergedFormInputs
  :: Hooks.HookEquals x (UseMergedFormInputs' h1 h2)
  => Hooks.HookNewtype (UseMergedFormInputs h1 h2) x

mergeFormInputs
  :: forall form m h1 h2 fields1 fields2 fields3 value1 value2 value3 closed1 closed2 closed3
   . Row.Union fields1 fields2 fields3
  => Row.Union value1 value2 value3
  => Row.Union closed1 closed2 closed3
  => BuildFormInput closed1 form m h1 fields1 value1
  -> BuildFormInput closed2 form m h2 fields2 value2
  -> BuildFormInput closed3 form m (UseMergedFormInputs h1 h2) fields3 value3
mergeFormInputs (BuildFormInput step1) (BuildFormInput step2) =
  BuildFormInput \props -> Hooks.wrap Hooks.do
    result1 <- step1 props
    result2 <- step2 props
    let fields = Record.union result1.fields result2.fields
    let value = Record.union <$> result1.value <*> result2.value
    Hooks.pure { fields, value }

type UseForm' form m h =
  Hooks.UseMemo (({ | form } -> { | form }) -> HookM m Unit)
    Hooks.<> h
    Hooks.<> Hooks.Pure

foreign import data UseForm :: # Type -> (Type -> Type) -> Hooks.HookType -> Hooks.HookType

instance newtypeUseForm
  :: Hooks.HookEquals x (UseForm' form m h)
  => Hooks.HookNewtype (UseForm form m h) x

type UseFormResult form m fields value =
  { dirty :: Boolean
  , fields :: { | fields }
  , form :: { | form }
  , modifyForm :: ({ | form } -> { | form }) -> HookM m Unit
  , value :: Maybe { | value }
  }

type FormState form =
  { dirty :: Boolean
  , form :: form
  }

type UseFormState form =
  Hooks.UseState (FormState form)

useFormState
  :: forall form m
   . (Unit -> form)
  -> Hooks.Hook m (UseFormState form) ((FormState form) /\ StateId (FormState form))
useFormState initialForm = Hooks.useState { dirty: false, form: initialForm unit }

useFormInputs
  :: forall m h form fields value
   . BuildFormInput form form m h fields value
  -> (FormState { | form } /\ StateId (FormState { | form }))
  -> Hooks.Hook m (UseForm form m h) (UseFormResult form m fields value)
useFormInputs (BuildFormInput step) ({ form, dirty } /\ sid) = Hooks.wrap Hooks.do
  modifyForm <- Hooks.captures {} Hooks.useMemo \_ fn ->
    Hooks.modify_ sid \st -> { form: fn st.form, dirty: true }

  { fields, value } <- step
    { form
    , modifyForm
    }

  Hooks.pure { dirty, fields, form, modifyForm, value }

type UseFormWithState form m h =
  UseFormState { | form }
    Hooks.<> UseForm form m h

useForm
  :: forall m h form fields value
   . (Unit -> { | form })
  -> BuildFormInput form form m h fields value
  -> Hooks.Hook m (UseFormWithState form m h) (UseFormResult form m fields value)
useForm k = Hooks.bind (useFormState k) <<< useFormInputs

type FormInputState (h :: Hooks.HookType) (ro :: # Type) i o = { state :: Maybe i, touched :: Boolean }
type FormInputValue (h :: Hooks.HookType) (ro :: # Type) i o = o
type FormInputField (h :: Hooks.HookType) (ro :: # Type) i o = { | WithValue o ro }
type FormInputHooks (h :: Hooks.HookType) (ro :: # Type) i o = HProxy h

foreign import data UseBuildForm :: # Type -> Hooks.HookType

buildForm
  :: forall m r r' sym h2 ro i o tail form' form closed2 closed3 h3 fields2 fields3 value2 value3 hform hform'
   . RowList.RowToList r (RowList.Cons sym (FormInput m h2 ro i o) tail)
  => Row.Cons sym (FormInput m h2 ro i o) r' r
  => IsSymbol sym
  => BuildForm r tail
      (BuildFormInput closed2 form m (UseFormInput h2) fields2 value2)
      (BuildFormInput closed3 form m h3 fields3 value3)
      hform'
  => Row.Cons sym { state :: Maybe i, touched :: Boolean } form' form
  => Row.Cons sym { state :: Maybe i, touched :: Boolean } () closed2
  => Row.Cons sym { | WithValue o ro } () fields2
  => Row.Cons sym o () value2
  => Row.Cons sym (HProxy h2) hform' hform
  => Row.Lacks sym ()
  => { | r }
  -> BuildFormInput closed3 form m (UseBuildForm hform) fields3 value3
buildForm inputs = do
  let
    input :: FormInput m h2 ro i o
    input = Record.get (SProxy :: _ sym) inputs

    builder :: BuildFormInput closed2 form m (UseFormInput h2) fields2 value2
    builder = buildFormInput (SProxy :: _ sym) input

  (unsafeCoerce
    :: BuildFormInput closed3 form m h3 fields3 value3
    -> BuildFormInput closed3 form m (UseBuildForm hform) fields3 value3) $
    buildFormStep (BuildFormFor inputs :: BuildFormFor r tail) builder

newtype BuildFormFor (r :: # Type) (rl :: RowList) = BuildFormFor { | r }

class BuildForm (r :: # Type) (rl :: RowList) builder1 builder2 (hform :: # Type) | r rl builder1 -> builder2 hform where
  buildFormStep :: BuildFormFor r rl -> builder1 -> builder2

instance buildFormNil :: BuildForm r RowList.Nil builder1 builder1 () where
  buildFormStep _ = identity

instance buildFormCons ::
  ( BuildForm r tail
      (BuildFormInput closed2 form m (UseMergedFormInputs h1 (UseFormInput h2)) fields2 value2)
      (BuildFormInput closed3 form m h3 fields3 value3)
      hform'
  , Row.Union fields1 fields' fields2
  , Row.Union value1 value' value2
  , Row.Union closed1 closed' closed2
  , IsSymbol sym
  , Row.Cons sym { state :: Maybe i, touched :: Boolean } form' form
  , Row.Cons sym { state :: Maybe i, touched :: Boolean } () closed'
  , Row.Cons sym { | WithValue o ro } () fields'
  , Row.Cons sym o () value'
  , Row.Lacks sym ()
  , Row.Cons sym (FormInput m h2 ro i o) r' r
  , Row.Cons sym (HProxy h2) hform' hform
  ) =>
  BuildForm r
    (RowList.Cons sym (FormInput m h2 ro i o) tail)
    (BuildFormInput closed1 form m h1 fields1 value1)
    (BuildFormInput closed3 form m h3 fields3 value3)
    hform
  where
  buildFormStep (BuildFormFor inputs) builder = do
    let
      input :: FormInput m h2 ro i o
      input = Record.get (SProxy :: _ sym) inputs

      builder' :: BuildFormInput closed2 form m (UseMergedFormInputs h1 (UseFormInput h2)) fields2 value2
      builder' = mergeFormInputs builder (buildFormInput (SProxy :: _ sym) input)

    buildFormStep (BuildFormFor inputs :: BuildFormFor r tail) builder'

data InitialFormState = InitialFormState

instance foldingInitialFormState ::
  ( TypeEquals { state :: Maybe a, touched :: Boolean } st
  , Row.Cons sym { state :: Maybe a, touched :: Boolean } rb rc
  , Row.Lacks sym rb
  , IsSymbol sym
  ) =>
  FoldingWithIndex InitialFormState (SProxy sym) (Builder { | ra } { | rb }) (Proxy st) (Builder { | ra } { | rc }) where
  foldingWithIndex _ sym builder _ =
    builder >>> Builder.insert sym { state: Nothing, touched: false }

-- | Build a default form state where all fields are initialized to Nothing.
initialFormState
  :: forall r rl
   . RowToList r rl
  => HFoldlWithIndex InitialFormState (Builder {} {}) (RLProxy rl) (Builder {} { | r })
  => { | r }
initialFormState =
  Builder.build (hfoldlWithIndex InitialFormState (identity :: Builder {} {}) (RLProxy :: RLProxy rl)) {}
