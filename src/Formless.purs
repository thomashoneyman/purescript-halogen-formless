-- | Formless is a renderless component to help you build forms in Halogen.
-- | It expects that you have already written a form spec and validation and
-- | you simply need a component to run it on your behalf.

module Formless
  ( Query(..)
  , Query'(..)
  , StateStore(..)
  , Component(..)
  , HTML(..)
  , HTML'(..)
  , DSL(..)
  , State(..)
  , PublicState(..)
  , Input(..)
  , Message(..)
  , Message'(..)
  , StateRow(..)
  , InternalState(..)
  , ValidStatus(..)
  , component
  , module Formless.Spec
  , module Formless.Spec.Transform
  , module Formless.Class.Initial
  , send'
  , modify
  , modify_
  , modifyValidate
  , modifyValidate_
  , validate
  , validate_
  , reset
  , reset_
  )
  where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, store)
import Data.Const (Const)
import Data.Eq (class EqRecord)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive)
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Traversable (traverse, traverse_)
import Data.Variant (Variant, inj)
import Formless.Class.Initial (class Initial, initial)
import Formless.Internal as Internal
import Formless.Spec (ErrorType, FormField(..), FormFieldRow, FormProxy(..), InputField(..), InputType, OutputField(..), OutputType, Validator(..), _Error, _Field, _Input, _Output, _Result, _Touched, _input, _result, _touched, _validator, getError, getField, getInput, getOutput, getResult, getTouched)
import Formless.Spec.Transform (class MakeInputFieldsFromRow, class MakeSProxies, SProxies, makeSProxiesBuilder, mkInputFields, mkInputFieldsFromProxy, mkInputFieldsFromRowBuilder, mkSProxies, mkValidators, unwrapOutput)
import Halogen as H
import Halogen.Component.ChildPath (ChildPath, injQuery, injSlot)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Renderless.State (getState, modifyState, modifyState_, modifyStore_, putState)

data Query pq cq cs form out m a
  = Modify (form Variant InputField) a
  | Validate (form Variant InputField) a
  | ModifyValidate (form Variant InputField) a
  | Reset (form Variant InputField) a
  | ResetAll a
  | ValidateAll a
  | Submit a
  | SubmitReply (Maybe out -> a)
  | Reply (PublicState form m -> a)
  | Send cs (cq Unit) a
  | SyncFormData a
  | Raise (pq Unit) a
  | ReplaceInputs (form Record InputField) a
  | Receive (Input pq cq cs form out m) a
  | AndThen (Query pq cq cs form out m Unit) (Query pq cq cs form out m Unit) a

-- | The overall component state type, which contains the local state type
-- | and also the render function
type StateStore pq cq cs form out m =
  Store (State form out m) (HTML pq cq cs form out m)

-- | The component type
type Component pq cq cs form out m
  = H.Component
      HH.HTML
      (Query pq cq cs form out m)
      (Input pq cq cs form out m)
      (Message pq form out m)
      m

-- | The component's HTML type, the result of the render function.
type HTML pq cq cs form out m
  = H.ParentHTML (Query pq cq cs form out m) cq cs m

-- | The component's DSL type, the result of the eval function.
type DSL pq cq cs form out m
  = H.ParentDSL
      (StateStore pq cq cs form out m)
      (Query pq cq cs form out m)
      cq
      cs
      (Message pq form out m)
      m

-- | The component local state
type State form out m = Record (StateRow form m (internal :: InternalState form out m))

-- | The component's public state
type PublicState form m = Record (StateRow form m ())

-- | The component's public state
type StateRow form m r =
  ( validity :: ValidStatus
  , dirty :: Boolean
  , submitting :: Boolean
  , errors :: Int
  , submitAttempts :: Int
  , form :: form Record (FormField m)
  | r
  )

-- | A newtype to make easier type errors for end users to
-- | read by hiding internal fields
newtype InternalState form out m = InternalState
  { initialInputs :: form Record InputField
  , formResult :: Maybe out
  , allTouched :: Boolean
  , submitter :: form Record OutputField -> m out
  }
derive instance newtypeInternalState :: Newtype (InternalState form out m) _

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

-- | The component's input type
type Input pq cq cs form out m =
  { submitter :: form Record OutputField -> m out
  , inputs :: form Record InputField
  , validators :: PublicState form m -> form Record (Validator m)
  , render :: State form out m -> HTML pq cq cs form out m
  }

-- | The component tries to require as few messages to be handled as possible. You
-- | can always use the *Reply variants of queries to perform actions and receive
-- | a result out the other end.
data Message pq form out m
  = Submitted out
  | Changed (PublicState form m)
  | Emit (pq Unit)

-- | When you are using several different types of child components in Formless
-- | the component needs a child path to be able to pick the right slot to send
-- | a query to.
send' :: ∀ pq cq' cs' cs cq form out m a
  . ChildPath cq cq' cs cs'
 -> cs
 -> cq Unit
 -> a
 -> Query pq cq' cs' form out m a
send' path p q = Send (injSlot path p) (injQuery path q)

-- | Simple types

-- | A simple query type when you have no child slots in use
type Query' form out m = Query (Const Void) (Const Void) Void form out m

-- | A simple HTML type when the component does not need embedding
type HTML' form out m = H.ParentHTML (Query' form out m) (Const Void) Void m

-- | A simple Message type when the component does not need embedding
type Message' form out m = Message (Const Void) form out m


-- | The component itself
component
  :: ∀ pq cq cs form out m fields fieldxs output countxs count inputs inputsxs vs
   . Ord cs
  => Monad m
  => RL.RowToList fields fieldxs
  => RL.RowToList count countxs
  => RL.RowToList inputs inputsxs
  => EqRecord inputsxs inputs
  => Internal.InputFieldsToFormFields inputsxs inputs fields
  => Internal.FormFieldsToInputFields fieldxs fields inputs
  => Internal.TransformFormFields fieldxs fields fields m
  => Internal.TransformFormFieldsM fieldxs fields fields m
  => Internal.FormFieldToMaybeOutput fieldxs fields output
  => Internal.CountErrors fieldxs fields count
  => Internal.AllTouched fieldxs fields
  => Internal.SumRecord countxs count (Additive Int)
  => Internal.UpdateInputVariantRL inputsxs inputs fields m
  => Internal.ReplaceFormFieldInputs inputs fieldxs fields fields
  => Internal.ReplaceFormFieldValidators vs fieldxs fields fields
  => Newtype (form Record (Validator m)) (Record vs)
  => Newtype (form Record (FormField m)) (Record fields)
  => Newtype (form Variant (FormField m)) (Variant fields)
  => Newtype (form Record OutputField) (Record output)
  => Newtype (form Record InputField) (Record inputs)
  => Newtype (form Variant InputField) (Variant inputs)
  => Component pq cq cs form out m
component =
  H.parentComponent
    { initialState
    , render: extract
    , eval
    , receiver: HE.input Receive
    }
  where

  initialState :: Input pq cq cs form out m -> StateStore pq cq cs form out m
  initialState { inputs, validators, render, submitter } = store render $
    -- The validators can themselves rely on form state, so must be constructed once there already
    -- exists some initial state.
    intermediate { form = Internal.replaceFormFieldValidators validators' intermediate.form }
    where
      validators' = validators (getPublicState intermediate)
      intermediate =
        { validity: Incomplete
        , dirty: false
        , errors: 0
        , submitAttempts: 0
        , submitting: false
        , form: Internal.inputFieldsToFormFields inputs
        , internal: InternalState
          { formResult: Nothing
          , allTouched: false
          , initialInputs: inputs
          , submitter
          }
        }

  eval :: Query pq cq cs form out m ~> DSL pq cq cs form out m
  eval = case _ of
    Modify variant a -> do
      st <- getState
      new <- H.lift $ modifyWithInputVariant variant st
      putState new
      eval $ SyncFormData a

    Validate variant a -> do
      st <- getState
      new <- H.lift $ validateWithInputVariant variant st
      putState new
      eval $ SyncFormData a

    ModifyValidate variant a -> do
      st <- getState
      new <- H.lift $ modifyValidateWithInputVariant variant st
      putState new
      eval $ SyncFormData a

    Reset variant a -> do
      st <- getState
      new <- H.lift $ resetWithInputVariant variant st
      putState new
      eval $ SyncFormData a

    ValidateAll a -> do
      let runField :: (∀ e i o. FormField m e i o -> m (FormField m e i o))
          runField (FormField field) = case field.validator of
            Nothing -> pure (FormField field) -- TODO: Exception?
            Just f -> do
              res <- f field.input
              pure (FormField
                { input: field.input
                , touched: true
                , result: Just res
                , validator: field.validator
                })

      st <- getState
      form <- H.lift $ Internal.transformFormFieldsM runField st.form
      modifyState_ _ { form = form }
      eval $ SyncFormData a

    -- A query to sync the overall state of the form after an individual field change
    -- or overall validation.
    SyncFormData a -> do
      modifyState_ \st -> st
        { errors = Internal.countErrors st.form
          -- Dirty state is computed by checking equality of original input fields
          -- vs. current ones. This relies on input fields passed by the user having
          -- equality defined.
        , dirty = not $ (==)
            (unwrap (Internal.formFieldsToInputFields st.form))
            (unwrap (unwrap st.internal).initialInputs)
        }

      st <- getState
      -- Need to verify the validity status of the form.
      new <- case (unwrap st.internal).allTouched of
        true -> modifyState _
          { validity = if not (st.errors == 0) then Invalid else Valid }

        -- If not all fields are touched, then we need to quickly sync the form state
        -- to verify this is actually the case.
        _ -> case Internal.checkTouched st.form of

          -- The sync revealed all fields really have been touched
          true -> modifyState _
            { validity = if not (st.errors == 0) then Invalid else Valid
            , internal = over InternalState (_ { allTouched = true }) st.internal
            }

          -- The sync revealed that not all fields have been touched
          _ -> modifyState _
            { validity = Incomplete }

      H.raise $ Changed $ getPublicState new
      pure a

    -- Submit, also raising a message to the user
    Submit a -> a <$ do
      st <- runSubmit
      traverse_ (H.raise <<< Submitted) st

    -- Submit, without raising a message, but returning the result directly
    SubmitReply reply -> do
       st <- runSubmit
       pure $ reply st

    -- | Should completely reset the form to its initial state
    ResetAll a -> do
      new <- modifyState \st -> st
        { validity = Incomplete
        , dirty = false
        , errors = 0
        , submitAttempts = 0
        , form = Internal.replaceFormFieldInputs (unwrap st.internal).initialInputs st.form
        , internal = over InternalState (_
            { formResult = Nothing
            , allTouched = false
            }
          ) st.internal
        }
      H.raise $ Changed $ getPublicState new
      pure a

    Reply reply -> do
      st <- getState
      pure $ reply $ getPublicState st

    -- Only allows actions; always returns nothing. In Halogen v5.0.0 branch this does return
    -- requests as expected in a Halogen component.
    Send cs cq a -> H.query cs cq $> a

    Raise query a -> do
      H.raise (Emit query)
      pure a

    ReplaceInputs formInputs a -> do
      st <- getState
      new <- modifyState _
        { validity = Incomplete
        , dirty = false
        , errors = 0
        , submitAttempts = 0
        , submitting = false
        , form = Internal.replaceFormFieldInputs formInputs st.form
        , internal =
            InternalState
              { formResult: Nothing
              , allTouched: false
              , initialInputs: formInputs
              , submitter: (unwrap st.internal).submitter
              }
        }
      H.raise $ Changed $ getPublicState new
      pure a

    Receive { render } a -> do
      modifyStore_ render identity
      pure a

    AndThen q1 q2 a -> do
      _ <- eval q1
      _ <- eval q2
      pure a

  -- Remove internal fields and return the public state
  getPublicState :: State form out m -> PublicState form m
  getPublicState = Record.delete (SProxy :: SProxy "internal")

  -- Use a form variant to update the value of a single form field in state
  modifyWithInputVariant
    :: form Variant InputField -> State form out m -> m (State form out m)
  modifyWithInputVariant form state = do
    form' <- updater (unwrap state.form)
    pure $ state { form = wrap form' }
    where
      updater :: { | fields } -> m { | fields }
      updater = Internal.updateInputVariant
        (\(InputField i) (FormField { validator }) -> pure $
          FormField
            { input: i
            , touched: true
            , result: Nothing
            , validator
            }
        )
        (unwrap form)

  -- Use a form variant to update the value of a single form field in state
  -- and also run its validation
  modifyValidateWithInputVariant
    :: form Variant InputField -> State form out m -> m (State form out m)
  modifyValidateWithInputVariant form state = do
    form' <- updater (unwrap state.form)
    pure (state { form = wrap form' })
    where
      updater :: { | fields } -> m { | fields }
      updater = Internal.updateInputVariant
        (\(InputField i) field@(FormField { validator }) -> do
          case validator of
            Nothing -> pure field
            Just f -> do
              res <- f i
              pure (FormField
                { input: i
                , touched: true
                , result: Just res
                , validator
                })
        )
        (unwrap form)

  -- Validate a field without modifying its input
  validateWithInputVariant
    :: form Variant InputField -> State form out m -> m (State form out m)
  validateWithInputVariant form state = do
    form' <- updater (unwrap state.form)
    pure (state { form = wrap form' })
    where
      updater :: { | fields } -> m { | fields }
      updater = Internal.updateInputVariant
        (\_ field@(FormField { input, validator }) -> do
          case validator of
            -- IMPROVE: Is this ever possible?
            Nothing -> pure field
            Just f -> do
              res <- f input
              pure (FormField
                { input
                , touched: true
                , result: Just res
                , validator
                })
        )
        (unwrap form)

  -- Reset a field (and update state to ensure the form is not marked with
  -- all fields touched).
  resetWithInputVariant
    :: form Variant InputField -> State form out m -> m (State form out m)
  resetWithInputVariant form state = do
    form' <- updater (unwrap state.form)
    pure $ state
      { form = wrap form'
      , internal = over InternalState (_ { allTouched = false }) state.internal
      }
    where
      updater :: { | fields } -> m { | fields }
      updater = Internal.updateInputVariant
        -- The input field provided could be anything, but if helper functions are
        -- used, it'll be the value provided by the Initial type class.
        (\(InputField i) (FormField { validator }) -> pure $
          FormField
            { input: i
            , touched: false
            , result: Nothing
            , validator
            }
        )
        (unwrap form)

  -- Run submission without raising messages or replies
  runSubmit :: DSL pq cq cs form out m (Maybe out)
  runSubmit = do
    init <- modifyState \st -> st
      { submitAttempts = st.submitAttempts + 1
      , submitting = true
      }

    -- For performance purposes, avoid running this if possible
    let internal = unwrap init.internal
    when (not internal.allTouched) do
      modifyState_ _
       { form = Internal.transformFormFields (over FormField (_ { touched = true })) init.form
       , internal = over InternalState (_ { allTouched = true }) init.internal
       }

    -- Necessary to validate after fields are touched, but before parsing
    _ <- eval $ ValidateAll unit

    -- For performance purposes, only attempt to submit if the form is valid
    validated <- getState
    when (validated.validity == Valid) do
      output <- H.lift $
        traverse internal.submitter (Internal.inputFieldToMaybeOutput validated.form)
      modifyState_ _
        { internal = over InternalState (_ { formResult = output }) validated.internal }

    -- Ensure the form is no longer marked submitting
    result <- modifyState \st -> st { submitting = false }
    pure (unwrap result.internal).formResult


----------
-- Component helper functions for variants

modify :: WithInput
modify sym i = Modify (wrap (inj sym (wrap i)))

modify_ :: WithInputAction
modify_ sym i = Modify (wrap (inj sym (wrap i))) unit

modifyValidate :: WithInput
modifyValidate sym i = ModifyValidate (wrap (inj sym (wrap i)))

modifyValidate_ :: WithInputAction
modifyValidate_ sym i = ModifyValidate (wrap (inj sym (wrap i))) unit

reset :: WithoutInput
reset sym = Reset (wrap (inj sym (wrap initial)))

reset_ :: WithoutInputAction
reset_ sym = Reset (wrap (inj sym (wrap initial))) unit

-- TODO: Shouldn't require an instance of Initial! Only there until
-- I'm able to come up with a different class for accessing via variants.
validate :: WithoutInput
validate sym = Validate (wrap (inj sym (wrap initial)))

validate_ :: WithoutInputAction
validate_ sym = Validate (wrap (inj sym (wrap initial))) unit


----------
-- Helper type synonyms

type WithInput =
  forall pq cq cs form out m a sym inputs t0 e i o
   . IsSymbol sym
  => Newtype (form Variant InputField) (Variant inputs)
  => Row.Cons sym (InputField e i o) t0 inputs
  => SProxy sym
  -> i
  -> a
  -> Query pq cq cs form out m a

type WithInputAction =
  forall pq cq cs form out m sym inputs t0 e i o
   . IsSymbol sym
  => Newtype (form Variant InputField) (Variant inputs)
  => Row.Cons sym (InputField e i o) t0 inputs
  => SProxy sym
  -> i
  -> Query pq cq cs form out m Unit

type WithoutInput =
  forall pq cq cs form out m sym a inputs t0 e i o
   . IsSymbol sym
  => Initial i
  => Newtype (form Variant InputField) (Variant inputs)
  => Row.Cons sym (InputField e i o) t0 inputs
  => SProxy sym
  -> a
  -> Query pq cq cs form out m a

type WithoutInputAction =
  forall pq cq cs form out m sym inputs t0 e i o
   . IsSymbol sym
  => Initial i
  => Newtype (form Variant InputField) (Variant inputs)
  => Row.Cons sym (InputField e i o) t0 inputs
  => SProxy sym
  -> Query pq cq cs form out m Unit

