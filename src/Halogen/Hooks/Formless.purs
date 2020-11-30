module Halogen.Hooks.Formless
  ( useForm
  , useFormWithState
  , UseFormWithState
  , UseFormState
  , UseForm
  , FormState
  , UseFormResult
  , FormFieldState'
  , ToFormState
  , ToFormOutput
  , ToFormValue
  , ToFormField
  , ToFormHooks
  , buildForm
  , FormField(..)
  , FormFieldProps
  , WithInput
  , WithValue
  , UseFormField
  , class BuildForm
  , BuildFormField
  , UseBuildForm
  , buildFormStep
  , BuildFormFor
  , initialFormState
  , InitialFormState
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Halogen as H
import Halogen.Hooks (HookM)
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
import Type.Equality as TE
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

type ToFormState (m :: Type -> Type) (h :: Hooks.HookType) (ro :: # Type) i o = FormFieldState' i
type ToFormOutput (m :: Type -> Type) (h :: Hooks.HookType) (ro :: # Type) i o = o
type ToFormValue (m :: Type -> Type) (h :: Hooks.HookType) (ro :: # Type) i o = { | WithValue o ro }
type ToFormHooks (m :: Type -> Type) (h :: Hooks.HookType) (ro :: # Type) i o = HProxy h
type ToFormField (m :: Type -> Type) (h :: Hooks.HookType) (ro :: # Type) i o = FormField m h ro i o

type FormFieldState' i =
  { touched :: Boolean
  , value :: Maybe i
  }

type FormFieldProps m i =
  { onChange :: i -> HookM m Unit
  , touched :: Boolean
  , value :: Maybe i
  }

newtype FormField m h ro i o =
  FormField (FormFieldProps m i -> Hooks.Hook m h { | WithValue o ro })

type WithInput m r = (input :: H.ComponentHTML (HookM m Unit) () m | r)
type WithValue o r = (value :: Maybe o | r)

type BuildFormFieldProps form m =
  { form :: { | form }
  , modifyForm :: ({ | form } -> { | form }) -> HookM m Unit
  }

newtype BuildFormField (closed :: # Type) form m h fields value =
  BuildFormField
    (BuildFormFieldProps form m
      -> Hooks.Hook m h { fields :: { | fields }, value :: Maybe { | value }})

type UseFormField' h1 = h1 Hooks.<> Hooks.Pure

foreign import data UseFormField :: Hooks.HookType -> Hooks.HookType

instance newtypeUseFormField
  :: Hooks.HookEquals x (UseFormField' h1)
  => Hooks.HookNewtype (UseFormField h1) x

buildFormField
  :: forall sym m h ro i o form' form closed fields value
   . IsSymbol sym
  => Row.Cons sym (FormFieldState' i) form' form
  => Row.Cons sym (FormFieldState' i) () closed
  => Row.Cons sym { | WithValue o ro } () fields
  => Row.Cons sym o () value
  => Row.Lacks sym ()
  => SProxy sym
  -> FormField m h ro i o
  -> BuildFormField closed form m (UseFormField h) fields value
buildFormField sym (FormField formInput) =
  BuildFormField \{ form, modifyForm } -> Hooks.wrap Hooks.do
    let { touched, value } = Record.get sym form
    let onChange = modifyForm <<< Record.set sym <<< { touched: true, value: _ } <<< Just
    result <- formInput { onChange, touched, value }
    let fields = Record.insert sym result {}
    let value = flip (Record.insert sym) {} <$> result.value
    Hooks.pure { fields, value }

type UseMergedFormFields' h1 h2 =
  h1
    Hooks.<> h2
    Hooks.<> Hooks.Pure

foreign import data UseMergedFormFields :: Hooks.HookType -> Hooks.HookType -> Hooks.HookType

instance newtypeUseMergedFormFields
  :: Hooks.HookEquals x (UseMergedFormFields' h1 h2)
  => Hooks.HookNewtype (UseMergedFormFields h1 h2) x

mergeFormFields
  :: forall form m h1 h2 fields1 fields2 fields3 value1 value2 value3 closed1 closed2 closed3
   . Row.Union fields1 fields2 fields3
  => Row.Union value1 value2 value3
  => Row.Union closed1 closed2 closed3
  => BuildFormField closed1 form m h1 fields1 value1
  -> BuildFormField closed2 form m h2 fields2 value2
  -> BuildFormField closed3 form m (UseMergedFormFields h1 h2) fields3 value3
mergeFormFields (BuildFormField step1) (BuildFormField step2) =
  BuildFormField \props -> Hooks.wrap Hooks.do
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
  -> Hooks.Hook m (UseFormState form) (Tuple (FormState form) ((FormState form -> FormState form) -> HookM m Unit))
useFormState initialForm = map (map Hooks.modify_) (Hooks.useState { dirty: false, form: initialForm unit })

useForm
  :: forall m h form fields value
   . BuildFormField form form m h fields value
  -> Tuple (FormState { | form }) ((FormState { | form } -> FormState { | form }) -> HookM m Unit)
  -> Hooks.Hook m (UseForm form m h) (UseFormResult form m fields value)
useForm (BuildFormField step) ({ form, dirty } /\ modifyState) = Hooks.wrap Hooks.do
  modifyForm <- Hooks.captures {} Hooks.useMemo \_ fn ->
    modifyState \st -> { form: fn st.form, dirty: true }

  { fields, value } <- step
    { form
    , modifyForm
    }

  Hooks.pure { dirty, fields, form, modifyForm, value }

type UseFormWithState form m h =
  UseFormState { | form }
    Hooks.<> UseForm form m h

useFormWithState
  :: forall m h form fields value
   . (Unit -> { | form })
  -> BuildFormField form form m h fields value
  -> Hooks.Hook m (UseFormWithState form m h) (UseFormResult form m fields value)
useFormWithState k = Hooks.bind (useFormState k) <<< useForm

foreign import data UseBuildForm :: # Type -> Hooks.HookType

buildForm
  :: forall r r' sym m h2 ro i o tail form' form closed2 closed3 h3 fields2 fields3 value2 value3 hform hform'
   . RowList.RowToList r (RowList.Cons sym (FormField m h2 ro i o) tail)
  => Row.Cons sym (FormField m h2 ro i o) r' r
  => IsSymbol sym
  => BuildForm r tail
      (BuildFormField closed2 form m (UseFormField h2) fields2 value2)
      (BuildFormField closed3 form m h3 fields3 value3)
      hform'
  => Row.Cons sym (FormFieldState' i) form' form
  => Row.Cons sym (FormFieldState' i) () closed2
  => Row.Cons sym { | WithValue o ro } () fields2
  => Row.Cons sym o () value2
  => Row.Cons sym (HProxy h2) hform' hform
  => Row.Lacks sym ()
  => { | r }
  -> BuildFormField closed3 form m (UseBuildForm hform) fields3 value3
buildForm inputs = do
  let
    input :: FormField m h2 ro i o
    input = Record.get (SProxy :: _ sym) inputs

    builder :: BuildFormField closed2 form m (UseFormField h2) fields2 value2
    builder = buildFormField (SProxy :: _ sym) input

  (unsafeCoerce
    :: BuildFormField closed3 form m h3 fields3 value3
    -> BuildFormField closed3 form m (UseBuildForm hform) fields3 value3) $
    buildFormStep (BuildFormFor inputs :: BuildFormFor r tail) builder

newtype BuildFormFor (r :: # Type) (rl :: RowList) = BuildFormFor { | r }

class BuildForm (r :: # Type) (rl :: RowList) builder1 builder2 (hform :: # Type) | r rl builder1 -> builder2 hform where
  buildFormStep :: BuildFormFor r rl -> builder1 -> builder2

instance buildFormNil :: BuildForm r RowList.Nil builder1 builder1 () where
  buildFormStep _ = identity

instance buildFormCons ::
  ( BuildForm r tail
      (BuildFormField closed2 form m (UseMergedFormFields h1 (UseFormField h2)) fields2 value2)
      (BuildFormField closed3 form m h3 fields3 value3)
      hform'
  , Row.Union fields1 fields' fields2
  , Row.Union value1 value' value2
  , Row.Union closed1 closed' closed2
  , IsSymbol sym
  , Row.Cons sym (FormFieldState' i) form' form
  , Row.Cons sym (FormFieldState' i) () closed'
  , Row.Cons sym { | WithValue o ro } () fields'
  , Row.Cons sym o () value'
  , Row.Lacks sym ()
  , Row.Cons sym (FormField m h2 ro i o) r' r
  , Row.Cons sym (HProxy h2) hform' hform
  ) =>
  BuildForm r
    (RowList.Cons sym (FormField m h2 ro i o) tail)
    (BuildFormField closed1 form m h1 fields1 value1)
    (BuildFormField closed3 form m h3 fields3 value3)
    hform
  where
  buildFormStep (BuildFormFor inputs) builder = do
    let
      input :: FormField m h2 ro i o
      input = Record.get (SProxy :: _ sym) inputs

      builder' :: BuildFormField closed2 form m (UseMergedFormFields h1 (UseFormField h2)) fields2 value2
      builder' = mergeFormFields builder (buildFormField (SProxy :: _ sym) input)

    buildFormStep (BuildFormFor inputs :: BuildFormFor r tail) builder'

type UseFormRows m hooks form field value =
  Hooks.Hook m
    (UseForm form m (UseBuildForm hooks))
    (UseFormResult form m field value)

data InitialFormState = InitialFormState

instance foldingInitialFormState ::
  ( TE.TypeEquals (FormFieldState' a) formInputState
  , Row.Cons sym formInputState rb rc
  , Row.Lacks sym rb
  , IsSymbol sym
  ) =>
  FoldingWithIndex InitialFormState (SProxy sym) (Builder { | ra } { | rb }) (Proxy formInputState) (Builder { | ra } { | rc }) where
  foldingWithIndex _ sym builder _ =
    builder >>> Builder.insert sym (TE.to { touched: false, value: Nothing })

-- | Build a default form state where all fields are initialized to Nothing.
initialFormState
  :: forall r rl
   . RowToList r rl
  => HFoldlWithIndex InitialFormState (Builder {} {}) (RLProxy rl) (Builder {} { | r })
  => { | r }
initialFormState =
  Builder.build (hfoldlWithIndex InitialFormState (identity :: Builder {} {}) (RLProxy :: RLProxy rl)) {}
