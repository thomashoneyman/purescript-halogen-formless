-- | Formless is a renderless component to help you build forms in Halogen.
-- | It expects that you have already written a form spec and validation and
-- | you simply need a component to run it on your behalf.

module Formless where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, store)
import Data.Eq (class EqRecord)
import Data.Lens as Lens
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive)
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Traversable (traverse_)
import Formless.Internal as Internal
import Formless.Spec (FormSpec, InputField, OutputField)
import Formless.Spec as FSpec
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prim.Row (class Cons)
import Prim.RowList (class RowToList) as RL
import Record (delete) as Record
import Renderless.State (getState, modifyState, modifyState_, modifyStore_)
import Web.Event.Event (Event)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.MouseEvent (MouseEvent)

data Query pq cq cs (form :: (Type -> Type -> Type -> Type) -> Type) out m a
  = HandleBlur (form InputField -> form InputField) a
  | HandleChange (form InputField -> form InputField) a
  | TouchAll a
  | Validate a
  | ValidateReply (State' form -> a)
  | Submit a
  | SubmitReply (Maybe out -> a)
  | Reset a
  | ResetReply (State' form -> a)
  | GetState (State' form -> a)
  | Send cs (cq Unit) a
  | Raise (pq Unit) a
  | Receive (Input pq cq cs form out m) a

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
      (Message pq out)
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
      (Message pq out)
      m

-- | The component local state
type State form out m = Record (StateRow form (internal :: InternalState form out m))

-- | The component's public state
type State' form = Record (StateRow form ())

-- | The component's state as a row type
type StateRow form r =
  ( validity :: ValidStatus
  , dirty :: Boolean
  , errors :: Int
  , form :: form InputField
  | r
  )

-- | A newtype to make easier type errors for end users to
-- | read by hiding internal fields
newtype InternalState form out m = InternalState
  { validator :: form InputField -> m (form InputField)
  , parser :: form OutputField -> out
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
derive instance eqValidStatus :: Eq ValidStatus
derive instance ordValidStatus :: Ord ValidStatus

-- | The component's input type
type Input pq cq cs (form :: (Type -> Type -> Type -> Type) -> Type) out m =
  { formSpec :: form FormSpec
  -- TODO: form InputField -> m (V (form InputField) out), provide helpers?
  , validator :: form InputField -> m (form InputField)
  , parser :: form OutputField -> out
  , render :: State form out m -> HTML pq cq cs form out m
  }

-- | The component tries to require as few messages to be handled as possible. You
-- | can always use the *Reply variants of queries to perform actions and receive
-- | a result out the other end.
data Message pq out
  = Submitted out
  | Emit (pq Unit)

-- | The component itself
component
  :: ∀ pq cq cs form out m spec specxs field fieldxs mboutput mboutputxs output countxs count inputs inputsxs
   . Ord cs
  => Monad m
  => RL.RowToList spec specxs
  => RL.RowToList field fieldxs
  => RL.RowToList mboutput mboutputxs
  => RL.RowToList count countxs
  => RL.RowToList inputs inputsxs
  => EqRecord inputsxs inputs
  => Internal.FormSpecToInputField specxs spec () field
  => Internal.InputFieldsToInput fieldxs field () inputs
  => Internal.SetInputFieldsTouched fieldxs field () field
  => Internal.InputFieldToMaybeOutput fieldxs field () mboutput
  => Internal.MaybeOutputToOutputField mboutputxs mboutput () output
  => Internal.CountErrors fieldxs field () count
  => Internal.SumRecord countxs count (Additive Int)
  => Newtype (form FormSpec) (Record spec)
  => Newtype (form InputField) (Record field)
  => Newtype (form OutputField) (Record output)
  => Newtype (form Internal.MaybeOutput) (Record mboutput)
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
  initialState { formSpec, validator, render, parser } = store render $
    { validity: Incomplete
    , dirty: false
    , errors: 0
    , form: inputFields
    , internal: InternalState
      { formResult: Nothing
      , formSpec
      , allTouched: false
      , initialInputs: Internal.inputFieldsToInput inputFields
      , validator
      , parser
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

    Validate a -> do
      init <- getState
      let internal = unwrap init.internal
      form <- H.lift $ internal.validator init.form
      let errors = Internal.countErrors form
      modifyState_ \st -> st
        { form = form
        , errors = errors
          -- Dirty state is computed by checking equality of original input fields vs. current ones.
          -- This relies on input fields passed by the user having equality defined.
        , dirty = not $ unwrap (Internal.inputFieldsToInput st.form) == unwrap internal.initialInputs
        , validity =
            if not internal.allTouched
              then Incomplete
              else if not (errors == 0) then Invalid else Valid
        }
      pure a

    ValidateReply reply -> do
      _ <- eval $ Validate unit
      eval $ GetState reply

    -- | Set all fields to true, then set allTouched to true to
    -- | avoid recomputing
    TouchAll a -> do
      st <- getState
      when (not (_.allTouched (unwrap st.internal))) do
        modifyState_ _
         { form = Internal.setInputFieldsTouched st.form
         , internal = over InternalState (_ { allTouched = true }) st.internal
         }
      pure a

    -- | Should raise a submit message, because this does not
    -- | return the result values to the parent.
    Submit a -> do
      _ <- eval $ TouchAll unit
      _ <- eval $ Validate unit
      st <- runSubmit
      traverse_ (H.raise <<< Submitted) (_.formResult $ unwrap st.internal)
      pure a

    -- | Should not raise a submit message, because it returns
    -- | the value directly.
    SubmitReply reply -> do
      _ <- eval $ TouchAll unit
      _ <- eval $ Validate unit
      st <- runSubmit
      pure $ reply (_.formResult $ unwrap st.internal)


    -- | Should completely reset the form to its initial state
    Reset a -> do
      modifyState_ \st -> st
        { validity = Incomplete
        , dirty = false
        , errors = 0
        , form = Internal.formSpecToInputFields (_.formSpec $ unwrap st.internal)
        , internal = over InternalState (_
            { formResult = Nothing
            , allTouched = false
            }
          ) st.internal
        }
      pure a

    ResetReply reply -> do
      _ <- eval $ Reset unit
      eval $ GetState reply

    -- We'll allow component users to fetch the public form state at any point, but
    -- will remove the internal state first. (TODO: Is this really better than simply
    -- allowing them access to the entire state? After all, they'll see it anyway
    -- in their render functions).
    GetState reply -> do
      st <- getState
      pure $ reply $ Record.delete (SProxy :: SProxy "internal") st

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

  ----------
  -- Effectful eval helpers

  -- Only attempt to parse the form to an output if it is already
  -- valid, in order to save effort.
  runSubmit :: DSL pq cq cs form out m (State form out m)
  runSubmit = do
    output <- runFormParser
    modifyState \st -> st
      { internal = over InternalState (_ { formResult = output }) st.internal }

  -- Only attempt to parse the form to an output if it is already
  -- valid, in order to save effort.
  runFormParser :: DSL pq cq cs form out m (Maybe out)
  runFormParser = do
    st <- getState
    pure $
      if st.validity == Valid
        then let internal = unwrap st.internal
                 outputs =
                   Internal.maybeOutputToOutputField
                   $ Internal.inputFieldToMaybeOutput
                   $ st.form
              in internal.parser <$> outputs
        else Nothing


--------------------
-- External to the component
--------------------

----------
-- Render Helpers

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
