-- | Formless is a renderless component to help you build forms in Halogen.
-- | It expects that you have already written a form spec and validation and
-- | you simply need a component to run it on your behalf.

module Formless where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, store)
import Data.Eq (class EqRecord)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens as Lens
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive)
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Traversable (traverse, traverse_)
import Formless.Class.Initial (class Initial, initial)
import Formless.Internal as Internal
import Formless.Spec (FormSpec, InputField, OutputField)
import Formless.Spec as FSpec
import Halogen as H
import Halogen.Component.ChildPath (ChildPath, injQuery, injSlot)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prim.Row (class Cons)
import Prim.RowList (class RowToList) as RL
import Record as Record
import Renderless.State (getState, modifyState, modifyState_, modifyStore_)
import Web.Event.Event (Event)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.MouseEvent (MouseEvent)

data Query pq cq cs (form :: (Type -> Type -> Type -> Type) -> Type) out m a
  = HandleBlur (form InputField -> form InputField) a
  | HandleChange (form InputField -> form InputField) a
  | HandleReset (form InputField -> form InputField) a
  | Reset a
  | Validate a
  | Submit a
  | SubmitReply (Maybe out -> a)
  | Send cs (cq Unit) a
  | Raise (pq Unit) a
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
type State' form = Record (StateRow form ())

-- | The component's state as a row type
type StateRow form r =
  ( validity :: ValidStatus
  , dirty :: Boolean
  , submitting :: Boolean
  , errors :: Int
  , submitAttempts :: Int
  , form :: form InputField
  | r
  )

-- | A newtype to make easier type errors for end users to
-- | read by hiding internal fields
newtype InternalState form out m = InternalState
  { validator :: form InputField -> m (form InputField)
  , submitter :: form OutputField -> m out
  , formSpec :: form FormSpec
  , initialInputs :: form Internal.Input
  , formResult :: Maybe out
  , allTouched :: Boolean
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
type Input pq cq cs (form :: (Type -> Type -> Type -> Type) -> Type) out m =
  { formSpec :: form FormSpec
  , validator :: form InputField -> m (form InputField)
  , submitter :: form OutputField -> m out
  , render :: State form out m -> HTML pq cq cs form out m
  }

-- | The component tries to require as few messages to be handled as possible. You
-- | can always use the *Reply variants of queries to perform actions and receive
-- | a result out the other end.
data Message pq form out
  = Submitted out
  | Changed (State' form)
  | Emit (pq Unit)

-- | The component itself
component
  :: ∀ pq cq cs form out m spec specxs field fieldxs output countxs count inputs inputsxs
   . Ord cs
  => Monad m
  => RL.RowToList spec specxs
  => RL.RowToList field fieldxs
  => RL.RowToList count countxs
  => RL.RowToList inputs inputsxs
  => EqRecord inputsxs inputs
  => Internal.FormSpecToInputField specxs spec () field
  => Internal.InputFieldsToInput fieldxs field () inputs
  => Internal.SetInputFieldsTouched fieldxs field () field
  => Internal.InputFieldToMaybeOutput fieldxs field () output
  => Internal.CountErrors fieldxs field () count
  => Internal.AllTouched fieldxs field
  => Internal.SumRecord countxs count (Additive Int)
  => Newtype (form FormSpec) (Record spec)
  => Newtype (form InputField) (Record field)
  => Newtype (form OutputField) (Record output)
  => Newtype (form Internal.Input) (Record inputs)
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
      inputFields = Internal.formSpecToInputFields formSpec

  eval :: Query pq cq cs form out m ~> DSL pq cq cs form out m
  eval = case _ of
    HandleBlur fs a -> do
      modifyState_ \st -> st { form = fs st.form }
      eval $ Validate a

    HandleChange fs a -> do
      modifyState_ \st -> st { form = fs st.form }
      pure a

    HandleReset fs a -> do
      modifyState_ \st -> st { form = fs st.form }
      eval $ Validate a

    Validate a -> do
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
    Reset a -> do
      new <- modifyState \st -> st
        { validity = Incomplete
        , dirty = false
        , errors = 0
        , submitAttempts = 0
        , form = Internal.formSpecToInputFields (_.formSpec $ unwrap st.internal)
        , internal = over InternalState (_
            { formResult = Nothing
            , allTouched = false
            }
          ) st.internal
        }
      H.raise $ Changed $ getPublicState new
      pure a

    -- Only allows actions; always returns nothing.
    Send cs cq a -> do
      _ <- H.query cs cq
      pure a

    Raise query a -> do
      H.raise (Emit query)
      pure a

    Receive { render } a -> do
      modifyStore_ render (\s -> s)
      pure a

    AndThen q1 q2 a -> do
      _ <- eval q1
      _ <- eval q2
      pure a

  ----------
  -- Effectful eval helpers

  -- Remove internal fields and return the public state
  getPublicState :: State form out m -> State' form
  getPublicState = Record.delete (SProxy :: SProxy "internal")

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
       { form = Internal.setInputFieldsTouched init.form
       , internal = over InternalState (_ { allTouched = true }) init.internal
       }

    -- Necessary to validate after fields are touched, but before parsing
    _ <- eval $ Validate unit

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



--------------------
-- External to the component
--------------------

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

-- | Provided as a query
modify
  :: ∀ sym pq cq cs out m form form' i e o r
   . IsSymbol sym
  => Cons sym (InputField i e o) r form
  => Newtype (form' InputField) (Record form)
  => SProxy sym
  -> (i -> i)
  -> Query pq cq cs form' out m Unit
modify sym f = HandleChange (modify' sym f) unit

-- | Allows you to modify a field rather than set its value
modify'
  :: ∀ sym form form' inp err out r
   . IsSymbol sym
  => Cons sym (InputField inp err out) r form
  => Newtype form' (Record form)
  => SProxy sym
  -> (inp -> inp)
  -> form'
  -> form'
modify' sym f = wrap <<< setInput f <<< setTouched true <<< unwrap
  where
    _sym :: Lens.Lens' (Record form) (InputField inp err out)
    _sym = prop sym
    setInput =
      Lens.over (_sym <<< _Newtype <<< prop FSpec._input)
    setTouched =
      Lens.set (_sym <<< _Newtype <<< prop FSpec._touched)

-- | Handles resetting a single field, but is only possible if the field is
-- | a member of the Initial type class
handleReset
  :: ∀ pq cq cs m sym form' form i e o out r
   . IsSymbol sym
  => Cons sym (InputField i e o) r form
  => Newtype (form' InputField) (Record form)
  => Initial i
  => SProxy sym
  -> Query pq cq cs form' out m Unit
handleReset sym = HandleReset (handleReset' sym) unit

handleReset'
  :: ∀ sym form' form i e o r
   . IsSymbol sym
  => Cons sym (InputField i e o) r form
  => Newtype form' (Record form)
  => Initial i
  => SProxy sym
  -> form'
  -> form'
handleReset' sym = wrap <<< unsetTouched <<< unsetResult <<< unsetValue <<< unwrap
  where
    _sym :: Lens.Lens' (Record form) (InputField i e o)
    _sym = prop sym
    unsetTouched = Lens.set (_sym <<< _Newtype <<< prop FSpec._touched) false
    unsetResult = Lens.set (_sym <<< _Newtype <<< prop FSpec._result) Nothing
    unsetValue = Lens.set (_sym <<< _Newtype <<< prop FSpec._input) initial

-- | Performs behaviors for both blur and change events
handleBlurAndChange
  :: ∀ pq cq cs m sym form' form i e o out r
   . IsSymbol sym
  => Cons sym (InputField i e o) r form
  => Newtype (form' InputField) (Record form)
  => SProxy sym
  -> i
  -> Query pq cq cs form' m out Unit
handleBlurAndChange sym val = HandleBlur (handleBlur' sym <<< handleChange' sym val) unit

-- | Given a proxy symbol, will trigger validation on that field using
-- | its validator and current input
onBlurWith
  :: ∀ pq cq cs m sym form' form i e o out r props
   . IsSymbol sym
  => Cons sym (InputField i e o) r form
  => Newtype (form' InputField) (Record form)
  => SProxy sym
  -> HP.IProp (onBlur :: FocusEvent | props) (Query pq cq cs form' out m Unit)
onBlurWith sym = HE.onBlur $ const $ Just $ handleBlur sym

handleBlur
  :: ∀ pq cq cs m sym form' form i e o out r
   . IsSymbol sym
  => Cons sym (InputField i e o) r form
  => Newtype (form' InputField) (Record form)
  => SProxy sym
  -> Query pq cq cs form' m out Unit
handleBlur sym = HandleBlur (handleBlur' sym) unit

handleBlur'
  :: ∀ sym form' form i e o r
   . IsSymbol sym
  => Cons sym (InputField i e o) r form
  => Newtype form' (Record form)
  => SProxy sym
  -> form'
  -> form'
handleBlur' sym form = wrap <<< setTouched $ unwrap form
  where
    _sym :: Lens.Lens' (Record form) (InputField i e o)
    _sym = prop sym
    setTouched = Lens.set (_sym <<< _Newtype <<< prop FSpec._touched) true

-- | Replace the value at a given field with a new value of the correct type.
onValueInputWith
  :: ∀ pq cq cs m sym form' form e o out r props
   . IsSymbol sym
  => Cons sym (InputField String e o) r form
  => Newtype (form' InputField) (Record form)
  => SProxy sym
  -> HP.IProp (onInput :: Event, value :: String | props) (Query pq cq cs form' out m Unit)
onValueInputWith sym =
  HE.onValueInput \str -> Just (handleChange sym str)

onValueChangeWith
  :: ∀ pq cq cs m sym form' form e o out r props
   . IsSymbol sym
  => Cons sym (InputField String e o) r form
  => Newtype (form' InputField) (Record form)
  => SProxy sym
  -> HP.IProp (onChange :: Event, value :: String | props) (Query pq cq cs form' out m Unit)
onValueChangeWith sym =
  HE.onValueChange \str -> Just (handleChange sym str)

onChangeWith
  :: ∀ pq cq cs m sym form' form i e o out r props
   . IsSymbol sym
  => Cons sym (InputField i e o) r form
  => Newtype (form' InputField) (Record form)
  => SProxy sym
  -> i
  -> HP.IProp (onChange :: Event | props) (Query pq cq cs form' out m Unit)
onChangeWith sym i =
  HE.onChange \_ -> Just (handleChange sym i)

onClickWith
  :: ∀ pq cq cs m sym form' form i e o out r props
   . IsSymbol sym
  => Cons sym (InputField i e o) r form
  => Newtype (form' InputField) (Record form)
  => SProxy sym
  -> i
  -> HP.IProp (onClick :: MouseEvent | props) (Query pq cq cs form' out m Unit)
onClickWith sym i =
  HE.onClick \_ -> Just (handleChange sym i)

handleChange
  :: ∀ pq cq cs m sym form' form i e o out r
   . IsSymbol sym
  => Cons sym (InputField i e o) r form
  => Newtype (form' InputField) (Record form)
  => SProxy sym
  -> i
  -> Query pq cq cs form' out m Unit
handleChange sym val = HandleChange (handleChange' sym val) unit

handleChange'
  :: ∀ sym form form' inp err out r
   . IsSymbol sym
  => Cons sym (InputField inp err out) r form
  => Newtype form' (Record form)
  => SProxy sym
  -> inp
  -> form'
  -> form'
handleChange' sym val = wrap <<< setInput val <<< setTouched true <<< unwrap
  where
    _sym :: Lens.Lens' (Record form) (InputField inp err out)
    _sym = prop sym
    setInput =
      Lens.set (_sym <<< _Newtype <<< prop FSpec._input)
    setTouched =
      Lens.set (_sym <<< _Newtype <<< prop FSpec._touched)
