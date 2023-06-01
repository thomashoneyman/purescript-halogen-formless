module Formless
  ( formless
  , FieldInput
  , FieldState
  , FieldAction
  , FieldResult
  , FieldOutput
  , FieldValidation
  , FieldValidationM
  , validate
  , validateM
  , FormContext
  , FormConfig
  , OptionalFormConfig
  , FormQuery(..)
  , FormState
  , FormAction
  , FormOutput(..)
  , eval
  , raise
  , FormlessAction -- don't export constructors
  , handleSubmitValidate
  , handleSubmitValidateM
  -- The below exports are classes and functions that must be exported for type
  -- inference for Formless to work, but they shouldn't be needed explicitly in
  -- user code.
  , class MkConfig
  , mkConfig
  , MkFieldState
  , class MkFieldStates
  , mkFieldStates
  , MkFieldAction
  , class MkFieldActions
  , mkFieldActions
  , MkFieldResult
  , class MkFieldResults
  , mkFieldResults
  , MkFieldOutput
  , class MkFieldOutputs
  , mkFieldOutputs
  ) where

import Prelude

import ConvertableOptions (class Defaults, defaults)
import Data.Either (Either(..), hush)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (class VariantMapCases, Variant)
import Data.Variant as Variant
import Data.Variant.Internal (class VariantTraverseCases, VariantRep(..))
import Effect.Class (class MonadEffect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Foreign.Object.Unsafe as Object.Unsafe
import Halogen as H
import Halogen.HTML as HH
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Heterogeneous.Mapping (class HMap, class HMapWithIndex, class Mapping, class MappingWithIndex, hmap, hmapWithIndex)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Safe.Coerce (coerce)
import Type.Equality (class TypeEquals)
import Type.Equality as Type.Equality
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Web.UIEvent.FocusEvent (FocusEvent)

-- | A type synonym which picks the `input` type from a form field.
type FieldInput :: Type -> Type -> Type -> Type
type FieldInput input error output = input

-- | A type synonym which represents the current state of a form field.
type FieldState :: Type -> Type -> Type -> Type
type FieldState input error output =
  { initialValue :: input
  , value :: input
  , result :: Maybe (Either error output)
  }

-- | A type synonym which represents the available actions that can be called
-- | on a form field.
type FieldAction :: Type -> Type -> Type -> Type -> Type
type FieldAction action input error output =
  { key :: String
  , modify :: (FieldState input error output -> FieldState input error output) -> action
  , reset :: action
  , validate :: action
  , handleChange :: input -> action
  , handleBlur :: FocusEvent -> action
  }

-- | A type synonm which represents a pure validation function for a form field.
type FieldValidation :: Type -> Type -> Type -> Type
type FieldValidation input error output = input -> Either error output

-- | Validate a variant of form field inputs by providing a record of validation
-- | functions, one per possible case in the variant. Used for pure validation.
validate
  :: forall validators xs r1 r2 r3 inputs results
   . RL.RowToList validators xs
  => VariantMapCases xs r1 r2
  => Row.Union r1 r3 inputs
  => Row.Union r2 r3 results
  => Variant inputs
  -> Record validators
  -> Variant results
validate = flip Variant.over

-- | A type synonm which represents an effectful validation function for a form
-- | field.
type FieldValidationM :: (Type -> Type) -> Type -> Type -> Type -> Type
type FieldValidationM m input error output = input -> m (Either error output)

-- | Validate a variant of form field inputs by providing a record of validation
-- | functions, one per possible case in the variant. Used for effectful
-- | validation. It is possible to use `HalogenM` as your validation monad,
-- | which gives you full access to your component state (including form fields).
validateM
  :: forall xs r1 r2 r3 inputs validators results m
   . RL.RowToList validators xs
  => VariantTraverseCases m xs r1 r2
  => Row.Union r1 r3 inputs
  => Row.Union r2 r3 results
  => Applicative m
  => Variant inputs
  -> Record validators
  -> m (Variant results)
validateM = flip Variant.traverse

-- | A type synonym which represents the result of validating the `input` type
-- | of a form field to produce either its `error` or `output`.
type FieldResult :: Type -> Type -> Type -> Type
type FieldResult input error output = Either error output

-- | A type synonym which picks the `output` type from a form field.
type FieldOutput :: Type -> Type -> Type -> Type
type FieldOutput input error output = output

-- | Available settings for controlling the form's behavior.
type FormConfig = { | OptionalFormConfig }

type InitialFormConfig :: Row Type -> Type -> Type
type InitialFormConfig fields action =
  { liftAction :: FormlessAction fields -> action
  | OptionalFormConfig
  }

-- | Formless uses queries to notify you when important events happen in your
-- | form. You are expected to handle these queries in your `handleQuery`
-- | function so that Formless works correctly.
-- |
-- | You can use `handleSubmitValidate` or `handleSubmitValidateM` if you only
-- | need to handle form submission and validation events.
-- |
-- | - **Query**
-- |   Formless has proxied a query from the parent component to you. You are
-- |   expected to handle the query.
-- |
-- | - **Validate**
-- |   A field needs to be validated. You are expected to validate the field and
-- |   return its validated result. You can use the `validate` and `validateM`
-- |   functions provided for Formless to perform validation.
-- |
-- | - **SubmitAttempt**
-- |   The form was submitted, but not all fields passed validation. You are
-- |   given a record of all form fields along with their validation result.
-- |
-- | - **Submit**
-- |   The form was submitted and all fields passed validation. You are given
-- |   a record of the validated output types for every field in the form.
-- |
-- | - **Reset**
-- |   The form was reset to its initial state.
data FormQuery :: (Type -> Type) -> Row Type -> Row Type -> Row Type -> Type -> Type
data FormQuery query inputs results outputs a
  = Query (query a)
  | Validate (Variant inputs) (Variant results -> a)
  | SubmitAttempt (Record results) a
  | Submit (Record outputs) a
  | Reset a

-- | A default implementation for `handleQuery` which only handles successful
-- | submission and validation events, used when you only need non-monadic
-- | validation. See `FormQuery` for all available events in Formless.
handleSubmitValidate
  :: forall inputs fields validators results outputs query state action slots output m a
   . ({ | outputs } -> H.HalogenM state action slots (FormOutput fields output) m Unit)
  -> (Variant inputs -> Record validators -> Variant results)
  -> Record validators
  -> FormQuery query inputs results outputs a
  -> H.HalogenM state action slots (FormOutput fields output) m (Maybe a)
handleSubmitValidate onSubmit validate' validators = case _ of
  Submit outputs next -> do
    onSubmit outputs
    pure $ Just next
  Validate changed reply -> do
    pure $ Just $ reply $ validate' changed validators
  _ ->
    pure Nothing

-- | A default implementation for `handleQuery` which only handles successful
-- | submission and validation events, used when you need monadic validation.
-- | See `FormQuery` for all available events in Formless.
handleSubmitValidateM
  :: forall inputs fields validators results outputs query state action slots output m a
   . ({ | outputs } -> H.HalogenM state action slots (FormOutput fields output) m Unit)
  -> (Variant inputs -> Record validators -> H.HalogenM state action slots (FormOutput fields output) m (Variant results))
  -> Record validators
  -> FormQuery query inputs results outputs a
  -> H.HalogenM state action slots (FormOutput fields output) m (Maybe a)
handleSubmitValidateM onSubmit validateM' validators = case _ of
  Submit outputs next -> do
    onSubmit outputs
    pure $ Just next
  Validate changed reply -> do
    validated <- validateM' changed validators
    pure $ Just $ reply validated
  _ ->
    pure Nothing

-- | Available form-wide actions you can tell Formless to do.
type FormAction fields action =
  { setFields :: { | fields } -> action
  , reset :: action
  , submit :: action
  , setConfig :: FormConfig -> action
  , handleSubmit :: Event -> action
  }

-- | The summary state of the entire form.
type FormState =
  { errorCount :: Int
  , submitCount :: Int
  , allTouched :: Boolean
  }

-- | The full form context which is provided to you. It includes any component
-- | input you wish to take, along with the current state of the form fields and
-- | the form and a set of actions you can use on the form fields or the form.
type FormContext :: Row Type -> Row Type -> Type -> Type -> Type
type FormContext fields actions input action =
  { input :: input
  , fields :: { | fields }
  , actions :: { | actions }
  , formState :: FormState
  , formActions :: FormAction fields action
  }

-- | Formless uses `FormOutput` to let you notify it of events it should handle.
-- |
-- | - **Raise**
-- |   Tell Formless to proxy the provided output to the parent component.
-- |
-- | - **Eval**
-- |   Tell Formless to evaluate an action on the form. Actions are provided to
-- |   you via the `FormContext`, which gives you actions you can call on
-- |   individual form fields or on the form as a whole.
data FormOutput :: Row Type -> Type -> Type
data FormOutput fields output
  = Raise output
  | Eval (FormlessAction fields)

-- | A drop-in replacement for Halogen's `raise` function, which you can use to
-- | proxy output through Formless up to the parent component.
raise
  :: forall fields state action slots output m
   . output
  -> H.HalogenM state action slots (FormOutput fields output) m Unit
raise = H.raise <<< Raise

-- | Tell Formless to evaluate a Formless action.
eval
  :: forall fields state action slots output m
   . FormlessAction fields
  -> H.HalogenM state action slots (FormOutput fields output) m Unit
eval = H.raise <<< Eval

-- | Internal actions that Formless evaluates to modify the state of the form.
--
-- These constructors should never be exported. These actions are only provided
-- to the user via the form context after being constructed by Formless.
data FormlessAction :: Row Type -> Type
data FormlessAction fields
  = SubmitForm (Maybe Event)
  | ResetForm
  | SetForm (Record fields)
  | SetFormConfig FormConfig
  | ChangeField (Variant fields) INPUT
  | BlurField (Variant fields) FocusEvent
  | ModifyField (Variant fields) (FieldState INPUT ERROR OUTPUT -> FieldState INPUT ERROR OUTPUT)
  | ValidateField (Variant fields)
  | ResetField (Variant fields)

data InternalFormAction :: Row Type -> Type -> Type -> Type
data InternalFormAction fields input output
  = Initialize
  | Receive input
  | HandleForm (FormOutput fields output)

type InternalFormState :: Row Type -> Row Type -> Type -> Type -> Type
type InternalFormState fields actions input action =
  { input :: input
  , fieldObject :: FieldObject
  , fieldActions :: { | actions }
  , formState :: FormState
  , formActions :: FormAction fields action
  , formConfig :: FormConfig
  }

-- | The Formless higher-order component. Expects a form configuration, the
-- | initial input values for each field in the form, and the component that
-- | you want to provide `FormContext` to (ie. your form component).
-- |
-- | Please see the Formless README.md and examples directory for a thorough
-- | description of this component.
formless
  :: forall config inputs fields actions results outputs query action input output m
   . MonadEffect m
  => MkFieldStates inputs fields
  => MkFieldActions fields actions action
  => MkFieldResults fields results
  => MkFieldOutputs results outputs
  => MkConfig config (InitialFormConfig fields action)
  => { | config }
  -> { | inputs }
  -> H.Component (FormQuery query inputs results outputs) (FormContext fields actions input action) (FormOutput fields output) m
  -> H.Component query input output m
formless providedConfig initialForm component = H.mkComponent
  { initialState
  , render: \state -> do
      let
        context =
          { input: state.input
          , fields: fromFieldObject state.fieldObject
          , actions: state.fieldActions
          , formState: state.formState
          , formActions: state.formActions
          }
      HH.slot _inner unit component context HandleForm
  , eval: H.mkEval
      { initialize: Just Initialize
      , receive: Just <<< Receive
      , finalize: Nothing
      , handleAction: handleAction
      , handleQuery: H.query _inner unit <<< Query
      }
  }
  where
  _inner :: Proxy "inner"
  _inner = Proxy

  initialState :: input -> InternalFormState fields actions input action
  initialState input = do
    let
      initialFullConfig :: InitialFormConfig fields action
      initialFullConfig = mkConfig providedConfig

      liftAction :: FormlessAction fields -> action
      liftAction = initialFullConfig.liftAction

      initialConfig :: FormConfig
      initialConfig = Record.delete (Proxy :: Proxy "liftAction") initialFullConfig

      initialFormState :: FormState
      initialFormState =
        { submitCount: 0
        , errorCount: 0
        , allTouched: false
        }

      initialFormActions :: FormAction fields action
      initialFormActions =
        { setFields: liftAction <<< SetForm
        , reset: liftAction $ ResetForm
        , setConfig: liftAction <<< SetFormConfig
        , submit: liftAction $ SubmitForm Nothing
        , handleSubmit: liftAction <<< SubmitForm <<< Just
        }

      initialFieldStates :: { | fields }
      initialFieldStates = mkFieldStates initialForm

      initialFieldActions :: { | actions }
      initialFieldActions = mkFieldActions liftAction initialFieldStates

    { input
    , fieldObject: toFieldObject initialFieldStates
    , fieldActions: initialFieldActions
    , formState: initialFormState
    , formActions: initialFormActions
    , formConfig: initialConfig
    }

  handleAction :: InternalFormAction fields input output -> _
  handleAction = case _ of
    Initialize -> do
      { fieldObject, formConfig } <- H.get
      when' formConfig.validateOnMount \_ ->
        for_ (getKeys fieldObject) runValidation

    Receive input -> do
      { input: oldInput } <- H.get
      when' (not (unsafeRefEq oldInput input)) \_ ->
        H.modify_ _ { input = input }

    HandleForm (Raise output) ->
      H.raise output

    HandleForm (Eval action) -> case action of
      SubmitForm mbEvent -> do
        for_ mbEvent \event ->
          H.liftEffect $ Event.preventDefault event
        H.get >>= \{ fieldObject } ->
          for_ (getKeys fieldObject) runValidation
        H.get >>= \{ fieldObject } -> do
          H.modify_ \state -> state { formState { submitCount = state.formState.submitCount + 1 } }
          for_ (mkFieldResults (fromFieldObject fieldObject)) \results -> case mkFieldOutputs results of
            Nothing -> H.tell _inner unit $ SubmitAttempt results
            Just outputs -> H.tell _inner unit $ Submit outputs

      SetForm values ->
        H.modify_ \state -> state { fieldObject = toFieldObject values }

      SetFormConfig config ->
        H.modify_ \state -> state { formConfig = config }

      ResetForm -> do
        let reset field = field { value = field.initialValue, result = Nothing }
        H.modify_ \state -> state
          { fieldObject = map reset state.fieldObject
          , formState = { submitCount: 0, errorCount: 0, allTouched: false }
          }
        H.tell _inner unit Reset

      ChangeField changed input -> do
        { formConfig } <- H.get
        let modify = mkFieldRep changed (_ { value = input })
        H.modify_ \state -> state { fieldObject = modifyField modify state.fieldObject }
        when' formConfig.validateOnChange \_ ->
          runFormAction $ ValidateField changed

      BlurField changed _ -> do
        { formConfig } <- H.get
        when' formConfig.validateOnBlur \_ ->
          runFormAction $ ValidateField changed

      ModifyField changed modifyFn -> do
        { formConfig } <- H.get
        let modify = mkFieldRep changed modifyFn
        H.modify_ \state -> state { fieldObject = modifyField modify state.fieldObject }
        when' formConfig.validateOnModify \_ ->
          runFormAction $ ValidateField changed

      ValidateField changed ->
        runValidation (fieldsKey changed)

      ResetField changed -> do
        let
          reset field = field { value = field.initialValue, result = Nothing }
          modify = mkFieldRep changed reset
        H.modify_ \state -> state { fieldObject = modifyField modify state.fieldObject }

  runFormAction :: FormlessAction fields -> H.HalogenM _ _ _ _ _ Unit
  runFormAction action = handleAction $ HandleForm $ Eval action

  -- Validation must always be run using this helper function because it is not
  -- safe to validate based on a `Variant inputs` alone. Validation always needs
  -- to pull the latest field value from object state before validating it.
  runValidation :: FieldKey -> H.HalogenM _ _ _ _ m Unit
  runValidation fieldKey = do
    mbResult <- H.request _inner unit <<< (Validate <<< injInput <<< getField fieldKey) =<< H.gets _.fieldObject
    for_ mbResult \resultVariant -> do
      let result = coerceResultsRep resultVariant
      H.modify_ \state -> do
        let fieldObject = setFieldResult result state.fieldObject
        state
          { fieldObject = fieldObject
          , formState = state.formState
              { errorCount = countErrors fieldObject
              , allTouched = allTouched fieldObject
              }
          }

  countErrors :: FieldObject -> Int
  countErrors = 0 # Object.fold \acc _ { result } -> case result of
    Just (Left _) -> acc + 1
    _ -> acc

  allTouched :: FieldObject -> Boolean
  allTouched = true # Object.fold \acc _ { result } -> case result of
    Just _ -> acc && true
    _ -> false

  -- The `FieldObject` coercions below are only safe given two conditions:
  --
  -- 1. Each value in the `fields` row is a `FieldState` as asserted by `MkFieldStates`
  -- 2. The PureScript compiler represents records as objects in JavaScript
  toFieldObject :: { | fields } -> FieldObject
  toFieldObject = unsafeCoerce

  fromFieldObject :: FieldObject -> { | fields }
  fromFieldObject = unsafeCoerce

  -- The `Variant` coercions below are only safe given two conditions:
  --
  -- 1. The values in the variant are never read or modified, only the labels.
  -- 2. Each value in the `results` variant is a `FieldResult` as asserted by `MkFieldResults`
  injInput :: VariantRep INPUT -> Variant inputs
  injInput = unsafeCoerce

  fieldsKey :: Variant fields -> FieldKey
  fieldsKey = coerceRep >>> \(VariantRep rep) -> coerce rep.type
    where
    coerceRep :: Variant fields -> VariantRep (FieldState INPUT ERROR OUTPUT)
    coerceRep = unsafeCoerce

  coerceResultsRep :: Variant results -> VariantRep (Either ERROR OUTPUT)
  coerceResultsRep = unsafeCoerce

  mkFieldRep :: forall a. Variant fields -> a -> VariantRep a
  mkFieldRep variant value = VariantRep { type: coerce (fieldsKey variant), value }

  getKeys :: FieldObject -> Array FieldKey
  getKeys = coerce <<< Object.keys

  getField :: FieldKey -> FieldObject -> VariantRep INPUT
  getField (FieldKey key) object = do
    let field = Object.Unsafe.unsafeIndex object key
    VariantRep { type: key, value: field.value }

  modifyField
    :: VariantRep (FieldState INPUT ERROR OUTPUT -> FieldState INPUT ERROR OUTPUT)
    -> FieldObject
    -> FieldObject
  modifyField (VariantRep rep) object = do
    let field = Object.Unsafe.unsafeIndex object rep.type
    Object.insert rep.type (rep.value field) object

  setFieldResult :: VariantRep (Either ERROR OUTPUT) -> FieldObject -> FieldObject
  setFieldResult (VariantRep rep) object = do
    let field = Object.Unsafe.unsafeIndex object rep.type
    Object.insert rep.type (field { result = Just rep.value }) object

-- The types and functions below are intended for internal use only. Some must
-- be exported for the sake of type inference.

-- The state of the user's form fields is represented as a plain object. This is
-- safe so long as the field object is only ever accessed using fields we can
-- guarantee are part of the `fields` row as asserted by the `MkFieldStates` class.
type FieldObject = Object (FieldState INPUT ERROR OUTPUT)

foreign import data INPUT :: Type
foreign import data ERROR :: Type
foreign import data OUTPUT :: Type

newtype FieldKey = FieldKey String

type OptionalFormConfig =
  ( validateOnBlur :: Boolean
  , validateOnChange :: Boolean
  , validateOnModify :: Boolean
  , validateOnMount :: Boolean
  )

defaultConfig :: { | OptionalFormConfig }
defaultConfig =
  { validateOnBlur: true
  , validateOnChange: false
  , validateOnModify: false
  , validateOnMount: false
  }

class Defaults { | OptionalFormConfig } { | config } config' <= MkConfig config config' where
  mkConfig :: { | config } -> config'

instance Defaults { | OptionalFormConfig } { | config } config' => MkConfig config config' where
  mkConfig provided = defaults defaultConfig provided

data MkFieldState = MkFieldState

class HMap MkFieldState { | inputs } { | fields } <= MkFieldStates inputs fields | inputs -> fields where
  mkFieldStates :: { | inputs } -> { | fields }

instance HMap MkFieldState { | inputs } { | fields } => MkFieldStates inputs fields where
  mkFieldStates = hmap MkFieldState

instance Mapping MkFieldState input (FieldState input error output) where
  mapping MkFieldState input = { initialValue: input, value: input, result: Nothing }

newtype MkFieldAction fields action = MkFieldAction (FormlessAction fields -> action)

class HMapWithIndex (MkFieldAction fields action) { | fields } { | actions } <= MkFieldActions fields actions action | fields -> actions where
  mkFieldActions :: (FormlessAction fields -> action) -> { | fields } -> { | actions }

instance HMapWithIndex (MkFieldAction fields action) { | fields } { | actions } => MkFieldActions fields actions action where
  mkFieldActions lift = hmapWithIndex (MkFieldAction lift)

instance
  ( IsSymbol sym
  , TypeEquals (FieldState input error output) field
  , Row.Cons sym (FieldState input error output) _1 fields
  ) =>
  MappingWithIndex (MkFieldAction fields action) (Proxy sym) field (FieldAction action input error output) where
  mappingWithIndex (MkFieldAction lift) sym _ = do
    let
      -- We use an the field variant /only/ for access to labels, and /never/
      -- read the value at the label.
      fieldVariant :: Variant fields
      fieldVariant = Variant.inj sym (unsafeCoerce unit :: FieldState input error output)

      mkInput :: input -> INPUT
      mkInput = unsafeCoerce

      mkModify :: (FieldState input error output -> FieldState input error output) -> (FieldState INPUT ERROR OUTPUT -> FieldState INPUT ERROR OUTPUT)
      mkModify = unsafeCoerce

    { key: reflectSymbol sym
    , modify: lift <<< ModifyField fieldVariant <<< mkModify
    , reset: lift $ ResetField fieldVariant
    , validate: lift $ ValidateField fieldVariant
    , handleChange: lift <<< ChangeField fieldVariant <<< mkInput
    , handleBlur: lift <<< BlurField fieldVariant
    }

data MkFieldResult = MkFieldResult

class HFoldlWithIndex MkFieldResult (Maybe (Builder {} {})) { | fields } (Maybe (Builder {} { | results })) <= MkFieldResults fields results | fields -> results where
  mkFieldResults :: { | fields } -> Maybe { | results }

instance HFoldlWithIndex MkFieldResult (Maybe (Builder {} {})) { | fields } (Maybe (Builder {} { | results })) => MkFieldResults fields results where
  mkFieldResults = map (flip Builder.build {}) <<< hfoldlWithIndex MkFieldResult (pure identity :: Maybe (Builder {} {}))

instance
  ( IsSymbol sym
  , TypeEquals (FieldState input error output) field
  , Row.Lacks sym rb
  , Row.Cons sym (Either error output) rb rc
  ) =>
  FoldingWithIndex MkFieldResult (Proxy sym) (Maybe (Builder { | ra } { | rb })) field (Maybe (Builder { | ra } { | rc }))
  where
  foldingWithIndex MkFieldResult prop rin field = do
    let { result } = Type.Equality.from field
    (>>>) <$> rin <*> (Builder.insert prop <$> result)

data MkFieldOutput = MkFieldOutput

class HFoldlWithIndex MkFieldOutput (Maybe (Builder {} {})) { | results } (Maybe (Builder {} { | outputs })) <= MkFieldOutputs results outputs | results -> outputs where
  mkFieldOutputs :: { | results } -> Maybe { | outputs }

instance HFoldlWithIndex MkFieldOutput (Maybe (Builder {} {})) { | results } (Maybe (Builder {} { | outputs })) => MkFieldOutputs results outputs where
  mkFieldOutputs = map (flip Builder.build {}) <<< hfoldlWithIndex MkFieldOutput (pure identity :: Maybe (Builder {} {}))

instance
  ( IsSymbol sym
  , TypeEquals (FieldResult input error output) result
  , Row.Lacks sym rb
  , Row.Cons sym output rb rc
  ) =>
  FoldingWithIndex MkFieldOutput (Proxy sym) (Maybe (Builder { | ra } { | rb })) result (Maybe (Builder { | ra } { | rc }))
  where
  foldingWithIndex MkFieldOutput prop rin field = do
    let result = Type.Equality.from field
    (>>>) <$> rin <*> (Builder.insert prop <$> hush result)

-- A lazy version of `when`, for internal use.
when' :: forall m. Applicative m => Boolean -> (Unit -> m Unit) -> m Unit
when' true k = k unit
when' _ _ = pure unit
