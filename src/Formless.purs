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
  , SpecRow(..)
  , Input(..)
  , Message(..)
  , Message'(..)
  , StateRow(..)
  , InternalState(..)
  , InternalStateRow(..)
  , ValidStatus(..)
  , component
  , module Formless.Spec
  , module Formless.Spec.Transform
  , module Formless.Class.Initial
  , send'
  , modify
  , modifyValidate
  , validate
  , reset
  )
  where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, store)
import Data.Const (Const)
import Data.Eq (class EqRecord)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid.Additive (Additive)
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Traversable (traverse, traverse_)
import Data.Variant (Variant, inj)
import Formless.Class.Initial (class Initial, initial)
import Formless.Internal as Internal
import Formless.Spec (ErrorType, FormField(..), FormFieldRow, FormProxy(..), FormSpec(..), InputField(..), InputType, OutputField(..), OutputType, _Error, _Field, _Input, _Output, _Result, _Touched, _input, _result, _touched, _validator)
import Formless.Spec.Transform (class MakeSProxies, SProxies, getInput, getResult, makeSProxiesBuilder, mkSProxies, modifyInput, resetField, setInput, touchField, unwrapOutput)
import Halogen as H
import Halogen.Component.ChildPath (ChildPath, injQuery, injSlot)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Renderless.State (getState, modifyState, modifyState_, modifyStore_, putState)
import Type.Row (type (+))

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
  | Raise (pq Unit) a
  | ReplaceSpec (form Record (FormSpec m)) a
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

-- | Values provided by the user but maintained by the component
type SpecRow form out m r =
  ( validator :: Maybe (form Record (FormField m) -> m (form Record (FormField m)))
  , submitter :: form Record OutputField -> m out
  , formSpec :: form Record (FormSpec m)
  | r
  )

-- | Values created and maintained by the component
type InternalStateRow form out =
  ( initialInputs :: form Record InputField
  , formResult :: Maybe out
  , allTouched :: Boolean
  )

-- | A newtype to make easier type errors for end users to
-- | read by hiding internal fields
newtype InternalState form out m = InternalState
  (Record (SpecRow form out m + InternalStateRow form out))
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
type Input pq cq cs form out m = Record
  ( SpecRow form out m
    (render :: State form out m -> HTML pq cq cs form out m)
  )

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
  :: ∀ pq cq cs form out m spec specxs fields fieldxs output countxs count inputs inputsxs
   . Ord cs
  => Monad m
  => RL.RowToList spec specxs
  => RL.RowToList fields fieldxs
  => RL.RowToList count countxs
  => RL.RowToList inputs inputsxs
  => EqRecord inputsxs inputs
  => Internal.FormSpecToFormField specxs spec fields
  => Internal.FormFieldsToInput fieldxs fields inputs
  => Internal.SetFormFieldsTouched fieldxs fields fields
  => Internal.FormFieldToMaybeOutput fieldxs fields output
  => Internal.CountErrors fieldxs fields count
  => Internal.AllTouched fieldxs fields
  => Internal.SumRecord countxs count (Additive Int)
  => Internal.UpdateInputVariantRL inputsxs inputs fields m
  => Newtype (form Record (FormSpec m)) (Record spec)
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
  initialState { formSpec, validator, render, submitter } = store render $
    { validity: Incomplete
    , dirty: false
    , errors: 0
    , submitAttempts: 0
    , submitting: false
    , form: inputFields
    , internal: InternalState
      { formResult: Nothing
      , formSpec
      , allTouched: false
      , initialInputs: Internal.inputFieldsToInput inputFields
      , validator
      , submitter
      }
    }
    where
      inputFields = Internal.formSpecToFormFields formSpec

  eval :: Query pq cq cs form out m ~> DSL pq cq cs form out m
  eval = case _ of
    Modify variant a -> do
      st <- getState
      new <- H.lift $ modifyWithInputVariant variant st
      putState new
      H.raise $ Changed $ getPublicState new
      pure a

    Validate variant a -> do
      st <- getState
      new <- H.lift $ validateWithInputVariant variant st
      putState new
      H.raise $ Changed $ getPublicState new
      pure a

    ModifyValidate variant a -> do
      st <- getState
      new <- H.lift $ modifyValidateWithInputVariant variant st
      putState new
      H.raise $ Changed $ getPublicState new
      pure a

    Reset variant a -> do
      st <- getState
      new <- H.lift $ resetWithInputVariant variant st
      putState new
      H.raise $ Changed $ getPublicState new
      pure a

    -- Doesn't trigger individual validation functions, but _does_ run the global validation
    -- function. Perhaps should do both -- set all fields, validate, and then run global
    -- validation on the result? (form Record OutputField -> m out)? Remove the need for a
    -- submitter altogether.
    ValidateAll a -> do
      init <- getState
      let internal = unwrap init.internal
      form <- maybe (pure init.form) (\f -> H.lift $ f init.form) internal.validator
      let errors = Internal.countErrors form

      -- At this point we can modify most of the state, except for the valid status
      modifyState_ _
        { form = form
        , errors = errors
          -- Dirty state is computed by checking equality of original input fields
          -- vs. current ones. This relies on input fields passed by the user having
          -- equality defined.
        , dirty = not
          $ unwrap (Internal.inputFieldsToInput form) == unwrap internal.initialInputs
        }

      -- Need to verify the validity status of the form.
      new <- case internal.allTouched of
        true -> modifyState _
          { validity = if not (errors == 0) then Invalid else Valid }
        -- If not all fields are touched, then we need to quickly sync the form state
        -- to verify this is actually the case.
        _ -> case Internal.checkTouched form of
          -- The sync revealed all fields really have been touched
          true -> modifyState \st -> st
            { validity = if not (errors == 0) then Invalid else Valid
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
        , form = Internal.formSpecToFormFields (_.formSpec $ unwrap st.internal)
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

    -- Only allows actions; always returns nothing.
    Send cs cq a -> H.query cs cq $> a

    Raise query a -> do
      H.raise (Emit query)
      pure a

    ReplaceSpec formSpec a -> do
      let inputFields = Internal.formSpecToFormFields formSpec
      new <- modifyState \st -> st
        { validity = Incomplete
        , dirty = false
        , errors = 0
        , submitAttempts = 0
        , submitting = false
        , form = inputFields
        , internal =
            InternalState
              { formResult: Nothing
              , formSpec
              , allTouched: false
              , initialInputs: Internal.inputFieldsToInput inputFields
              , validator: (unwrap st.internal).validator
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
        (\(InputField i) (FormField { validator }) -> do
          res <- validator i
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
        (\_ (FormField { input, validator }) -> do
          res <- validator input
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
       { form = Internal.setFormFieldsTouched init.form
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
    pure $ _.formResult $ unwrap result.internal

----------
-- Component Helper Functions for Variants

modify
  :: ∀ pq cq cs form out m sym inputs t0 e i o
   . IsSymbol sym
  => Newtype (form Variant InputField) (Variant inputs)
  => Row.Cons sym (InputField e i o) t0 inputs
  => SProxy sym
  -> i
  -> Query pq cq cs form out m Unit
modify sym i = Modify (wrap (inj sym (wrap i))) unit

-- TODO: Shouldn't require an instance of Initial! Only there until
-- I'm able to come up with a different class for accessing via variants.
validate
  :: ∀ pq cq cs form out m sym inputs t0 e i o
   . IsSymbol sym
  => Initial i
  => Newtype (form Variant InputField) (Variant inputs)
  => Row.Cons sym (InputField e i o) t0 inputs
  => SProxy sym
  -> Query pq cq cs form out m Unit
validate sym = Validate (wrap (inj sym (wrap initial))) unit

modifyValidate
  :: ∀ pq cq cs form out m sym inputs t0 e i o
   . IsSymbol sym
  => Newtype (form Variant InputField) (Variant inputs)
  => Row.Cons sym (InputField e i o) t0 inputs
  => SProxy sym
  -> i
  -> Query pq cq cs form out m Unit
modifyValidate sym i = ModifyValidate (wrap (inj sym (wrap i))) unit

reset
  :: ∀ pq cq cs form out m sym inputs t0 e i o
   . IsSymbol sym
  => Initial i
  => Newtype (form Variant InputField) (Variant inputs)
  => Row.Cons sym (InputField e i o) t0 inputs
  => SProxy sym
  -> Query pq cq cs form out m Unit
reset sym = Reset (wrap (inj sym (wrap initial))) unit
