module Halogen.Hooks.Formless
  ( useForm
  , UseForm
  , useFormState
  , UseFormState
  , useFormFields
  , UseFormFields
  , FormState
  , FormInterface
  , buildForm
  , FormField(..)
  , FormFieldInput
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
import Data.Tuple.Nested ((/\), type (/\))
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
import Type.Proxy (Proxy, Proxy2)
import Unsafe.Coerce (unsafeCoerce)

-- | Input provided to a `FormField`, which can be used to implement the field.
-- |
-- | - `onChange`: A callback function which should be called when the value of
-- |   the field changes.
-- | - `value`: The current value of this form field in the form state, where
-- |   `Nothing` means the field has not been touched by the user yet.
-- | - `reset`: Set the field back to `Nothing`
type FormFieldInput m i =
  { onChange :: i -> HookM m Unit
  , value :: Maybe i
  , reset :: HookM m Unit
  }

-- | The basic building block of forms, where a Formless form is a record of
-- | `FormField`. A `FormField` is a function from `FormFieldInput` to a Hook
-- | which returns at least an output value (but may also return other, user-
-- | provided fields).
-- |
-- | - `m`: The monad used for for this form field. The monad type must match
-- |   among all form fields in a particular form.
-- | - `h`: The Hook type used for this form field.
-- | - `ro`: A row of any additional fields you wish to include in this form field
-- |   (for example, a rendered form control or error message, a callback function
-- |   to focus the field, and so on).
-- | - `i`: The input type for this form field.
-- | - `o`: The output type for this form field. A form is only considered valid
-- |   when output types are present for all fields in the form.
data FormField m h ro i o = FormField (Proxy2 m) (FormFieldInput m i -> Hooks.Hook m h { value :: Maybe o | ro })

-- | The interface returned by the `useForm` and `useFormFields` Hooks.
-- |
-- | - `touched`: Whether the user has interacted with any field in the form.
-- | - `fields`: The result value of each `FormField` in the form, which you can
-- |   then use to render or otherwise control your form.
-- | - `form`: The underlying form state for each field in the form.
-- | - `modifyForm`: A callback function which can be used to imperatively modify
-- |   the underlying form state.
-- | - `value`: If all fields have produced an output value, then this contains
-- |   a record of each field's output value.
type FormInterface form m fields value =
  { touched :: Boolean
  , fields :: { | fields }
  , form :: { | form }
  , modifyForm :: ({ | form } -> { | form }) -> HookM m Unit
  , value :: Maybe { | value }
  }

type UseForm' form m h =
  UseFormState { | form }
    Hooks.<> UseFormFields form m h

-- | The Hook type for `useForm`, which accepts the form row, monad type, and
-- | form fields Hook type (typically UseBuildForm) as arguments.
foreign import data UseForm :: # Type -> (Type -> Type) -> Hooks.HookType -> Hooks.HookType

instance newtypeUseForm
  :: Hooks.HookEquals x (UseForm' form m h)
  => Hooks.HookNewtype (UseForm form m h) x

-- | A Hook for managing forms with Formless. Combines `useFormState` and
-- | `useFormFields` to manage form state and form fields.
-- |
-- | This Hook requires a function to produce the initial form state and function
-- | to build the set of `FormField`.
-- |
-- | As demonstrated below, typically, you'll provide `\_ -> initialFormState`
-- | for the initial form state and `buildForm { <your form fields> }` to build
-- | the set of `FormField` for the form.
-- |
-- | ```purs
-- | form <- useForm (\_ -> initialFormState) $ buildForm
-- |   { field1: FormField \field -> ...
-- |   , field2: FormField \field -> ...
-- |   }
-- | ```
useForm
  :: forall m h form fields value
   . (Unit -> { | form })
  -> BuildFormField form form m h fields value
  -> Hooks.Hook m (UseForm form m h) (FormInterface form m fields value)
useForm initialState = Hooks.wrap <<< Hooks.bind (useFormState initialState) <<< flip useFormFields

type FormState form =
  { touched :: Boolean
  , form :: form
  }

type UseFormState' form = Hooks.UseState (FormState form)

-- | The Hook type for `useFormState`, which accepts the form type as a type
-- | parameter.
foreign import data UseFormState :: Type -> Hooks.HookType

instance newtypeUseFormState
  :: Hooks.HookEquals h (UseFormState' form)
  => Hooks.HookNewtype (UseFormState form) h

-- | A Hook for managing form state with Formless. Requires a function to produce
-- | the initial form state, which is usually `\_ -> initialFormState`.
-- |
-- | This Hook is typically used when one field depends on another field's form
-- | state, in which case you'll need to create the form state and form fields
-- | separately (instead of using the single-step `useForm`).
-- |
-- | ```purs
-- | import Data.Tuple.Nested ((/\))
-- |
-- | state /\ modifyState <- useFormState (\_ -> initialFormState)
-- | form <- useFormFields (state /\ modifyState) $ buildForm
-- |  { field1: FormField \field ->
-- |      { value: ...
-- |        -- We can, for example, check its value against the form state for
-- |        -- another field.
-- |      , isValid: field.value == state.form.field2
-- |      }
-- |  , field2: FormField \field -> ...
-- |  }
-- | ```
useFormState
  :: forall form m
   . (Unit -> form)
  -> Hooks.Hook m (UseFormState form) (FormState form /\ ((FormState form -> FormState form) -> HookM m Unit))
useFormState initialForm =
  Hooks.wrap $ map (map Hooks.modify_) (Hooks.useState { touched: false, form: initialForm unit })

type UseFormFields' form m h =
  Hooks.UseMemo (({ | form } -> { | form }) -> HookM m Unit)
    Hooks.<> h
    Hooks.<> Hooks.Pure

-- | The Hook type for `UseFormFields`, which accepts the form row, monad, and
-- | hook type (typically `UseBuildForm`) as arguments.
foreign import data UseFormFields :: # Type -> (Type -> Type) -> Hooks.HookType -> Hooks.HookType

instance newtypeUseFormFields
  :: Hooks.HookEquals x (UseFormFields' form m h)
  => Hooks.HookNewtype (UseFormFields form m h) x

-- | A Hook for managing form fields with Formless. Requires a state tuple to
-- | read and modify the form state, provided by `Formless.useFormState`, and a
-- | function to build the form fields, provided by `Formless.buildForm`.
-- |
-- | This Hook is typically used when one field depends on another field's form
-- | state, in which case you'll need to create the form state and form fields
-- | separately (instead of using the single-step `useForm`).
-- |
-- | ```purs
-- | import Data.Tuple.Nested ((/\))
-- |
-- | state /\ modifyState <- useFormState (\_ -> initialFormState)
-- | form <- useFormFields (state /\ modifyState) $ buildForm
-- |  { field1: FormField \field ->
-- |      { value: ...
-- |        -- We can, for example, check its value against the form state for
-- |        -- another field.
-- |      , isValid: field.value == state.form.field2
-- |      }
-- |  , field2: FormField \field -> ...
-- |  }
-- | ```
useFormFields
  :: forall m h form fields value
   . Tuple (FormState { | form }) ((FormState { | form } -> FormState { | form }) -> HookM m Unit)
  -> BuildFormField form form m h fields value
  -> Hooks.Hook m (UseFormFields form m h) (FormInterface form m fields value)
useFormFields ({ form, touched } /\ modifyState) (BuildFormField step) = Hooks.wrap Hooks.do
  modifyForm <- Hooks.captures {} Hooks.useMemo \_ fn ->
    modifyState \st -> { form: fn st.form, touched: true }

  { fields, value } <- step
    { form
    , modifyForm
    }

  Hooks.pure { touched, fields, form, modifyForm, value }

type BuildFormFieldInput form m =
  { form :: { | form }
  , modifyForm :: ({ | form } -> { | form }) -> HookM m Unit
  }

-- | The result of building a set of form fields with `buildForm`, which should
-- | then be provided to the `useForm` or `useFormFields` Hook as an argument.
-- |
-- | It is rare that you want to use this type directly. Instead, pass the result
-- | of `buildForm` directly to the form Hook you are using.
-- |
-- | ```purs
-- | form <- useForm (\_ -> initialFormState) $ buildForm
-- |   { field1: FormField \field -> ...
-- |   , field2: FormField \field -> ...
-- |   }
-- | ```
newtype BuildFormField (closed :: # Type) form m h fields value =
  BuildFormField
    (BuildFormFieldInput form m
      -> Hooks.Hook m h { fields :: { | fields }, value :: Maybe { | value }})

type UseFormField' h1 = h1 Hooks.<> Hooks.Pure

-- | The Hook type for building a form field, which accepts the hook type for
-- | the form field as its argument.
foreign import data UseFormField :: Hooks.HookType -> Hooks.HookType

instance newtypeUseFormField
  :: Hooks.HookEquals x (UseFormField' h1)
  => Hooks.HookNewtype (UseFormField h1) x

-- | An internal helper function for building a single form field.
buildFormField
  :: forall sym m h ro i o form' form closed fields value
   . IsSymbol sym
  => Row.Cons sym (Maybe i) form' form
  => Row.Cons sym (Maybe i) () closed
  => Row.Cons sym { value :: Maybe o | ro } () fields
  => Row.Cons sym o () value
  => Row.Lacks sym ()
  => SProxy sym
  -> FormField m h ro i o
  -> BuildFormField closed form m (UseFormField h) fields value
buildFormField sym (FormField _ formField) =
  BuildFormField \{ form, modifyForm } -> Hooks.wrap Hooks.do
    let
      fieldValue = Record.get sym form
      onChange = modifyForm <<< Record.set sym <<< Just
      reset = modifyForm (Record.set sym Nothing)

    result <- formField { onChange, value: fieldValue, reset }

    let
      fields = Record.insert sym result {}
      value = flip (Record.insert sym) {} <$> result.value

    Hooks.pure { fields, value }

type UseMergedFormFields' h1 h2 =
  h1
    Hooks.<> h2
    Hooks.<> Hooks.Pure

foreign import data UseMergedFormFields :: Hooks.HookType -> Hooks.HookType -> Hooks.HookType

instance newtypeUseMergedFormFields
  :: Hooks.HookEquals x (UseMergedFormFields' h1 h2)
  => Hooks.HookNewtype (UseMergedFormFields h1 h2) x

-- | An internal helper function for merging form fields to produce a single
-- | resulting form.
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

foreign import data UseBuildForm :: # Type -> Hooks.HookType

-- | Build a form from a record of form fields, which can then be passed to
-- | `useForm` or `useFormFields` to produce your full form.
-- |
-- | It is best to pass the result of `buildForm` directly to the form Hook you
-- | are using, rather than attempt to give the intermediate structure a type.
-- |
-- | ```purs
-- | form <- useForm (\_ -> initialFormState) $ buildForm
-- |   { field1: FormField \field -> ...
-- |   , field2: FormField \field -> ...
-- |   }
-- | ```
buildForm
  :: forall r r' sym m h2 ro i o tail form' form closed2 closed3 h3 fields2 fields3 value2 value3 hform hform'
   . RowList.RowToList r (RowList.Cons sym (FormField m h2 ro i o) tail)
  => Row.Cons sym (FormField m h2 ro i o) r' r
  => IsSymbol sym
  => BuildForm r tail
      (BuildFormField closed2 form m (UseFormField h2) fields2 value2)
      (BuildFormField closed3 form m h3 fields3 value3)
      hform'
  => Row.Cons sym (Maybe i) form' form
  => Row.Cons sym (Maybe i) () closed2
  => Row.Cons sym { value :: Maybe o | ro } () fields2
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
  , Row.Cons sym (Maybe i) form' form
  , Row.Cons sym (Maybe i) () closed'
  , Row.Cons sym { value :: Maybe o | ro } () fields'
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

data InitialFormState = InitialFormState

instance foldingInitialFormState ::
  ( TE.TypeEquals (Maybe i) formFieldState
  , Row.Cons sym formFieldState rb rc
  , Row.Lacks sym rb
  , IsSymbol sym
  ) =>
  FoldingWithIndex InitialFormState (SProxy sym) (Builder { | ra } { | rb }) (Proxy formFieldState) (Builder { | ra } { | rc }) where
  foldingWithIndex _ sym builder _ =
    builder >>> Builder.insert sym (TE.to Nothing)

-- | A helper function to build an initial form state where all fields are
-- | initialized to `Nothing`. This function should be provided to the `useForm`
-- | or `useFormState` Hooks as an argument.
-- |
-- | ```purs
-- | form <- useFormState (\_ -> initialFormState) ...
-- | ```
initialFormState
  :: forall r rl
   . RowToList r rl
  => HFoldlWithIndex InitialFormState (Builder {} {}) (RLProxy rl) (Builder {} { | r })
  => { | r }
initialFormState =
  Builder.build (hfoldlWithIndex InitialFormState (identity :: Builder {} {}) (RLProxy :: RLProxy rl)) {}
