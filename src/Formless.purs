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
  , Input'(..)
  , Message(..)
  , Message'(..)
  , StateRow(..)
  , InternalState(..)
  , ValidStatus(..)
  , component
  , module Formless.Spec
  , module Formless.Class.Initial
  , module Formless.Validation
  , send
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

import Formless.Spec
import Formless.Transform.Record
import Formless.Validation
import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, store)
import Control.Monad.Free (liftF)
import Data.Const (Const)
import Data.Coyoneda (liftCoyoneda)
import Data.Eq (class EqRecord)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive)
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Traversable (traverse_)
import Data.Variant (Variant, inj)
import Data.Variant.Internal (VariantRep(..), unsafeGet)
import Formless.Class.Initial (class Initial, initial)
import Formless.Spec.Retrieve (GetInputField(..), getInputAll)
import Halogen as H
import Halogen.Component.ChildPath (ChildPath, injQuery, injSlot)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Heterogeneous.Folding (class HFoldl, class HFoldlWithIndex)
import Heterogeneous.Mapping (class HMap, class HMapWithIndex)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Builder (Builder)
import Renderless.State (getState, modifyState, modifyState_, modifyStore_)
import Unsafe.Coerce (unsafeCoerce)

data Query pq cq cs form m a
  = Modify (form Variant InputField) a
  | Validate (form Variant U) a
  | ModifyValidate (form Variant InputField) a
  | Reset (form Variant InputField) a
  | ResetAll a
  | ValidateAll a
  | Submit a
  | SubmitReply (Maybe (form Record OutputField) -> a)
  | GetState (PublicState form -> a)
  | Send cs (cq a)
  | SyncFormData a
  | Raise (pq Unit) a
  | ReplaceInputs (form Record InputField) a
  | Receive (Input pq cq cs form m) a
  | AndThen (Query pq cq cs form m Unit) (Query pq cq cs form m Unit) a

-- | The overall component state type, which contains the local state type
-- | and also the render function
type StateStore pq cq cs form m =
  Store (State form m) (HTML pq cq cs form m)

-- | The component type
type Component pq cq cs form m
  = H.Component
      HH.HTML
      (Query pq cq cs form m)
      (Input pq cq cs form m)
      (Message pq form)
      m

-- | The component's HTML type, the result of the render function.
type HTML pq cq cs form m
  = H.ParentHTML (Query pq cq cs form m) cq cs m

-- | The component's DSL type, the result of the eval function.
type DSL pq cq cs form m
  = H.ParentDSL
      (StateStore pq cq cs form m)
      (Query pq cq cs form m)
      cq
      cs
      (Message pq form)
      m

-- | The component local state
type State form m = Record (StateRow form (internal :: InternalState form m))

-- | The component's public state
type PublicState form = Record (StateRow form ())

-- | The component's public state
type StateRow form r =
  ( validity :: ValidStatus
  , dirty :: Boolean
  , submitting :: Boolean
  , errors :: Int
  , submitAttempts :: Int
  , form :: form Record FormField
  | r
  )

-- | A newtype to make easier type errors for end users to
-- | read by hiding internal fields
newtype InternalState form m = InternalState
  { initialInputs :: form Record InputField
  , validators :: form Record (Validation form m)
  , allTouched :: Boolean
  }
derive instance newtypeInternalState :: Newtype (InternalState form m) _

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
type Input pq cq cs form m =
  { initialInputs :: form Record InputField
  , validators :: form Record (Validation form m)
  , render :: State form m -> HTML pq cq cs form m
  }

-- | The component tries to require as few messages to be handled as possible. You
-- | can always use the *Reply variants of queries to perform actions and receive
-- | a result out the other end.
data Message pq form
  = Submitted (form Record OutputField)
  | Changed (PublicState form)
  | Emit (pq Unit)

-- | Simple types

-- | A simple query type when you have no child slots in use
type Query' form m = Query (Const Void) (Const Void) Void form m

-- | A simple HTML type when the component does not need embedding
type HTML' form m = H.ParentHTML (Query' form m) (Const Void) Void m

-- | A simple Message type when the component does not need embedding
type Message' form = Message (Const Void) form

-- | A simple Input type when the component does not need embedding
type Input' form m = Input (Const Void) (Const Void) Void form m

-- | The component itself
component
  :: ∀ pq cq cs form m is ixs ivs fs us vs
   . Ord cs
  => Monad m
  => RL.RowToList is ixs
  => EqRecord ixs is
  => HMap InputFieldToFormField { | is } { | fs }
  => HMap GetInputField { | fs } { | fs }
  => HMap SetFormFieldTouched { | fs } { | fs }
  => HMapWithIndex (ReplaceInput is) { | fs } { | fs }
  => HFoldl CountError Int { | fs } Int
  => HFoldl Touched Boolean { | fs } Boolean
  => Newtype (form Record InputField) { | is }
  => Newtype (form Variant InputField) (Variant ivs)
  => Newtype (form Record FormField) { | fs }
  => Newtype (form Record (Validation form m)) { | vs }
  => Newtype (form Variant U) (Variant us)
  => Component pq cq cs form m
component =
  H.parentComponent
    { initialState
    , render: extract
    , eval
    , receiver: HE.input Receive
    }
  where

  initialState :: Input pq cq cs form m -> StateStore pq cq cs form m
  initialState { initialInputs, validators, render } = store render $
    { validity: Incomplete
    , dirty: false
    , errors: 0
    , submitAttempts: 0
    , submitting: false
    , form: wrap $ inputFieldsToFormFields $ unwrap initialInputs
    , internal: InternalState { allTouched: false, initialInputs, validators }
    }

  eval :: Query pq cq cs form m ~> DSL pq cq cs form m
  eval = case _ of
    Modify variant a -> do
      modifyState_ \st -> st { form = unsafeSetInputVariant variant st.form }
      eval $ SyncFormData a

    Validate variant a -> do
      st <- getState
      form <- H.lift
        $ unsafeRunValidationVariant variant (unwrap st.internal).validators st.form
      modifyState_ _ { form = form }
      eval $ SyncFormData a

    ModifyValidate variant a -> do
      void $ eval $ Modify variant a
      void $ eval $ Validate (unsafeCoerce variant :: form Variant U) a
      eval $ SyncFormData a

    Reset variant a -> do
      modifyState_ \st -> st
        { form = wrap
          $ replaceFormFieldInputs
            (unwrap (unwrap st.internal).initialInputs)
            (unwrap st.form)
        , internal = over InternalState (_ { allTouched = false }) st.internal
        }
      eval $ SyncFormData a

    ValidateAll a -> do
			-- TODO
      --  st <- getState
      --  form <- H.lift $ applyValidation st.form (unwrap (unwrap st.internal).validators)
      --  modifyState_ _ { form = wrap form }
      eval $ SyncFormData a

    -- A query to sync the overall state of the form after an individual field change
    -- or overall validation.
    SyncFormData a -> do
      modifyState_ \st -> st
        { errors = countErrors (unwrap st.form)
          -- Dirty state is computed by checking equality of original input fields
          -- vs. current ones. This relies on input fields passed by the user having
          -- equality defined.
        , dirty = not $ (==)
            (getInputAll st.form)
            (unwrapRecord (unwrap st.internal).initialInputs)
        }

      st <- getState
      -- Need to verify the validity status of the form.
      new <- case (unwrap st.internal).allTouched of
        true -> modifyState _
          { validity = if not (st.errors == 0) then Invalid else Valid }

        -- If not all fields are touched, then we need to quickly sync the form state
        -- to verify this is actually the case.
        _ -> case allTouched $ unwrap st.form of

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
        , form = wrap
            $ replaceFormFieldInputs
              (unwrap (unwrap st.internal).initialInputs)
              (unwrap st.form)
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

    ReplaceInputs formInputs a -> do
      st <- getState
      new <- modifyState _
        { validity = Incomplete
        , dirty = false
        , errors = 0
        , submitAttempts = 0
        , submitting = false
        , form = wrap $ replaceFormFieldInputs (unwrap formInputs) (unwrap st.form)
        , internal = over
            InternalState
            (_ { allTouched = false, initialInputs = formInputs })
            st.internal
        }
      H.raise $ Changed $ getPublicState new
      pure a

    Receive { render, validators } a -> do
      let applyOver = over InternalState (_ { validators = validators })
      modifyStore_ render (\st -> st { internal = applyOver st.internal })
      pure a

    AndThen q1 q2 a -> do
      _ <- eval q1
      _ <- eval q2
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
       { form = wrap $ setFormFieldsTouched true $ unwrap init.form
       , internal = over InternalState (_ { allTouched = true }) init.internal
       }

    -- Necessary to validate after fields are touched, but before parsing
    _ <- eval $ ValidateAll unit

    -- For performance purposes, only attempt to submit if the form is valid
    validated <- getState

    form <- case validated.validity of
      Valid -> do
        output <- H.lift $ ?sequenceRecord (?toMaybeOut validated.form)
        pure (Just output)
      _ -> pure Nothing

    modifyState_ \st -> st { submitting = false }
    pure form


-----
-- Querying

-- | For use when you need to query a component through Formless
send :: ∀ pq cs cq form m a
  . cs
 -> cq a
 -> Query pq cq cs form m a
send p q = Send p q

-- | When you are using several different types of child components in Formless
-- | the component needs a child path to be able to pick the right slot to send
-- | a query to.
send' :: ∀ pq cq' cs' cs cq form m a
  . ChildPath cq cq' cs cs'
 -> cs
 -> cq a
 -> Query pq cq' cs' form m a
send' path p q = Send (injSlot path p) (injQuery path q)

-----
-- Queries

-- | A helper to create the correct `Modify` query for Formless given a label and
-- | an input value
modify
  :: ∀ pq cq cs form inputs m sym t0 e i o a
   . IsSymbol sym
  => Newtype (form Variant InputField) (Variant inputs)
  => Row.Cons sym (InputField e i o) t0 inputs
  => SProxy sym
  -> i
  -> a
  -> Query pq cq cs form m a
modify sym i = Modify (wrap (inj sym (wrap i)))

-- | A helper to create the correct `Modify` query for Formless given a label and
-- | an input value, as an action
modify_
  :: ∀ pq cq cs form inputs m sym t0 e i o
   . IsSymbol sym
  => Newtype (form Variant InputField) (Variant inputs)
  => Row.Cons sym (InputField e i o) t0 inputs
  => SProxy sym
  -> i
  -> Query pq cq cs form m Unit
modify_ sym i = Modify (wrap (inj sym (wrap i))) unit

-- | A helper to create the correct `ModifyValidate` query for Formless given a
-- | label and an input value
modifyValidate
  :: ∀ pq cq cs form inputs m sym t0 e i o a
   . IsSymbol sym
  => Newtype (form Variant InputField) (Variant inputs)
  => Row.Cons sym (InputField e i o) t0 inputs
  => SProxy sym
  -> i
  -> a
  -> Query pq cq cs form m a
modifyValidate sym i = ModifyValidate (wrap (inj sym (wrap i)))

-- | A helper to create the correct `ModifyValidate` query for Formless given a
-- | label and an input value, as an action
modifyValidate_
  :: ∀ pq cq cs form inputs m sym t0 e i o
   . IsSymbol sym
  => Newtype (form Variant InputField) (Variant inputs)
  => Row.Cons sym (InputField e i o) t0 inputs
  => SProxy sym
  -> i
  -> Query pq cq cs form m Unit
modifyValidate_ sym i = ModifyValidate (wrap (inj sym (wrap i))) unit

-- | A helper to create the correct `Reset` query for Formless given a label
reset
  :: ∀ pq cq cs form inputs m sym a t0 e i o
   . IsSymbol sym
  => Initial i
  => Newtype (form Variant InputField) (Variant inputs)
  => Row.Cons sym (InputField e i o) t0 inputs
  => SProxy sym
  -> a
  -> Query pq cq cs form m a
reset sym = Reset (wrap (inj sym (wrap initial)))

-- | A helper to create the correct `Reset` query for Formless given a label,
-- | as an action.
reset_
  :: ∀ pq cq cs form inputs m sym t0 e i o
   . IsSymbol sym
  => Initial i
  => Newtype (form Variant InputField) (Variant inputs)
  => Row.Cons sym (InputField e i o) t0 inputs
  => SProxy sym
  -> Query pq cq cs form m Unit
reset_ sym = Reset (wrap (inj sym (wrap initial))) unit

-- | A helper to create the correct `Validate` query for Formless, given
-- | a label
validate
  :: ∀ pq cq cs form us m sym a t0 e i o
   . IsSymbol sym
  => Newtype (form Variant U) (Variant us)
  => Row.Cons sym (U e i o) t0 us
  => SProxy sym
  -> a
  -> Query pq cq cs form m a
validate sym = Validate (wrap (inj sym U))

-- | A helper to create the correct `Validate` query for Formless given
-- | a label, as an action
validate_
  :: ∀ pq cq cs form us m sym t0 e i o
   . IsSymbol sym
  => Newtype (form Variant U) (Variant us)
  => Row.Cons sym (U e i o) t0 us
  => SProxy sym
  -> Query pq cq cs form m Unit
validate_ sym = Validate (wrap (inj sym U)) unit
