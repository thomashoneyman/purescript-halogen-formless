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
  , Input'(..)
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
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse, traverse_)
import Data.Variant (Variant)
import Formless.Class.Initial (class Initial, initial)
import Formless.Internal as Internal
import Formless.Spec (ErrorType, FormField(..), FormFieldRow, FormProxy(..), FormSpec(..), InputField(..), InputType, OutputField(..), OutputType, _Error, _Field, _Input, _Output, _Result, _Touched, _input, _result, _touched)
import Formless.Spec.Transform (class MakeFormSpecFromRow, class MakeSProxies, SProxies, getInput, getResult, makeSProxiesBuilder, mkFormSpec, mkFormSpecFromProxy, mkFormSpecFromRowBuilder, mkSProxies, modifyInput, resetField, setInput, touchField, unwrapOutput)
import Halogen as H
import Halogen.Component.ChildPath (ChildPath, injQuery, injSlot)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Prim.RowList as RL
import Record as Record
import Renderless.State (getState, modifyState, modifyState_, modifyStore_)
import Type.Row (type (+))

data Query pq cq cs form out m a
  = Modify (form Variant InputField) a
  | ModifyValidate (form Variant InputField) a
  | Reset (form Variant InputField) a
  | ResetAll a
  | ValidateAll a
  | Submit a
  | SubmitReply (Maybe out -> a)
  | Reply (PublicState form -> a)
  | Send cs (cq Unit) a
  | Raise (pq Unit) a
  | ReplaceSpec (form Record FormSpec) a
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
      (Message pq form out)
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
      (Message pq form out)
      m

-- | The component local state
type State form out m = Record (StateRow form (internal :: InternalState form out m))

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

-- | Values provided by the user but maintained by the component
type SpecRow form out m r =
  ( validator :: form Record FormField -> m (form Record FormField)
  , submitter :: form Record OutputField -> m out
  , formSpec :: form Record FormSpec
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
data Message pq form out
  = Submitted out
  | Changed (PublicState form)
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
type Message' form out = Message (Const Void) form out

-- | A simple input type for when you aren't embedding anything
type Input' form out m = Input (Const Void) (Const Void) Void form out m


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
  => Internal.RecordVariantUpdateRL inputsxs inputs fields
  => Newtype (form Record FormSpec) (Record spec)
  => Newtype (form Record FormField) (Record fields)
  => Newtype (form Variant FormField) (Variant fields)
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
      new <- modifyState (withInputVariant variant)
      pure a

    ModifyValidate variant a -> do
      new <- modifyState (withInputVariant variant)
      -- TODO: field-level validation
      H.raise $ Changed $ getPublicState new
      pure a

    Reset _ a -> do
      -- TODO: reset
      --  modifyState_ \st -> st
      --    { form = fs st.form
      --    , internal = over InternalState (_ { allTouched = false }) st.internal
      --    }
      pure a

    ValidateAll a -> do
      init <- getState
      let internal = unwrap init.internal
      form <- H.lift $ internal.validator init.form
      let errors = Internal.countErrors form

      -- At this point we can modify most of the state, except for the valid status
      modifyState_ _
        { form = form
        , errors = errors
          -- Dirty state is computed by checking equality of original input fields vs. current ones.
          -- This relies on input fields passed by the user having equality defined.
        , dirty = not $ unwrap (Internal.inputFieldsToInput form) == unwrap internal.initialInputs
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
  getPublicState :: State form out m -> PublicState form
  getPublicState = Record.delete (SProxy :: SProxy "internal")

  withInputVariant
    :: form Variant InputField -> State form out m -> State form out m
  withInputVariant form state =
    state { form = wrap (updater formR) }
    where
      updater :: { | fields } -> { | fields }
      updater = Internal.rvUpdate formV

      formR :: Record fields
      formR = unwrap state.form

      formV :: Variant inputs
      formV = unwrap form

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

