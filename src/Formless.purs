-- | Formless is a renderless component to help you build forms in Halogen.
-- | It expects that you have already written a form spec and validation and
-- | you simply need a component to run it on your behalf.

module Formless where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, store)
import Data.Either (Either, note)
import Data.Lens as Lens
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (class IsSymbol, SProxy)
import Formless.Internal as Internal
import Formless.Spec (FormSpec, InputField, MaybeOutput, OutputField)
import Formless.Spec as FSpec
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prim.Row (class Cons)
import Prim.RowList (class RowToList) as RL
import Renderless.State (getState, modifyState_, modifyStore_)
import Web.Event.Event (Event)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.MouseEvent (MouseEvent)

data Query pq cq cs (form :: (Type -> Type -> Type -> Type) -> Type) m a
  = HandleBlur (form InputField -> form InputField) a
  | HandleChange (form InputField -> form InputField) a
  | TouchAll a
  | RunValidation a
  | SubmitReply (Either (form InputField) (form OutputField) -> a)
  | Submit a
  | Send cs (cq Unit) a
  | Raise (pq Unit) a
  | Receive (Input pq cq cs form m) a

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
type State form m =
  { isValid :: Boolean
  , allTouched :: Boolean
  , formResult :: Maybe (form OutputField)
  , errors :: Int
  , formSpec :: form FormSpec
  , validator :: form InputField -> m (form InputField)
  , form :: form InputField
  }

-- | The component's input type
type Input pq cq cs (form :: (Type -> Type -> Type -> Type) -> Type) m =
  { formSpec :: form FormSpec
  , validator :: form InputField -> m (form InputField)
  , render :: State form m -> HTML pq cq cs form m
  }

data Message pq form
  = Submitted (Either (form InputField) (form OutputField))
  | Validated (form InputField) Int
  | Emit (pq Unit)

-- | The component itself
component
  :: ∀ pq cq cs form m spec specxs field fieldxs mboutput mboutputxs output countxs count
   . Ord cs
  => Monad m
  => RL.RowToList spec specxs
  => RL.RowToList field fieldxs
  => RL.RowToList mboutput mboutputxs
  => RL.RowToList count countxs
  => Internal.FormSpecToInputField specxs spec () field
  => Internal.SetInputFieldsTouched fieldxs field () field
  => Internal.InputFieldToMaybeOutput fieldxs field () mboutput
  => Internal.MaybeOutputToOutputField mboutputxs mboutput () output
  => Internal.CountErrors fieldxs field () count
  => Internal.SumRecord countxs count (Additive Int)
  => Newtype (form FormSpec) (Record spec)
  => Newtype (form InputField) (Record field)
  => Newtype (form MaybeOutput) (Record mboutput)
  => Newtype (form OutputField) (Record output)
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
  initialState { formSpec, validator, render } = store render $
    { isValid: false
    , allTouched: false
    , errors: 0
    , formResult: Nothing
    , formSpec
    , validator
    , form: Internal.formSpecToInputFields formSpec
    }

  eval :: Query pq cq cs form m ~> DSL pq cq cs form m
  eval = case _ of
    HandleBlur fs a -> do
      modifyState_ \st -> st { form = fs st.form }
      eval $ RunValidation a

    HandleChange fs a -> do
      modifyState_ \st -> st { form = fs st.form }
      pure a

    RunValidation a -> do
      st <- getState
      form <- H.lift $ st.validator st.form
      modifyState_ _
        { form = form
        , formResult =
            Internal.maybeOutputToOutputField
            $ Internal.inputFieldToMaybeOutput
            $ form
        , errors = Internal.countErrors form
        }
      st' <- getState
      H.raise $ Validated st'.form st'.errors
      pure a

    -- | Set all fields to true, then set allTouched to true to
    -- | avoid recomputing
    TouchAll a -> do
      st <- getState
      if st.allTouched
        then pure a
        else do
          modifyState_ _
           { form = Internal.setInputFieldsTouched st.form
           , allTouched = true }
          pure a

    -- | Should not raise a submit message, because it returns
    -- | the value directly.
    SubmitReply reply -> do
      _ <- eval $ TouchAll unit
      _ <- eval $ RunValidation unit
      st <- getState
      pure $ reply $ note st.form st.formResult

    -- | Should raise a submit message, because this does not
    -- | return the result values to the parent.
    Submit a -> do
      _ <- eval $ TouchAll unit
      _ <- eval $ RunValidation unit
      st <- getState
      H.raise $ Submitted $ note st.form st.formResult
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


---------
-- Helpers

-- | Given a proxy symbol, will trigger validation on that field using
-- | its validator and current input
onBlurWith
  :: ∀ pq cq cs m sym form' form inp err out r props
   . IsSymbol sym
  => Cons sym (InputField inp err out) r form
  => Newtype (form' InputField) (Record form)
  => SProxy sym
  -> HP.IProp (onBlur :: FocusEvent | props) (Query pq cq cs form' m Unit)
onBlurWith sym = HE.onBlur $ const $ Just $ handleBlur sym

handleBlur
  :: ∀ pq cq cs m sym form' form inp err out r
   . IsSymbol sym
  => Cons sym (InputField inp err out) r form
  => Newtype (form' InputField) (Record form)
  => SProxy sym
  -> Query pq cq cs form' m Unit
handleBlur sym = HandleBlur (handleBlur' sym) unit

handleBlur'
  :: ∀ sym form' form inp err out r
   . IsSymbol sym
  => Cons sym (InputField inp err out) r form
  => Newtype form' (Record form)
  => SProxy sym
  -> form'
  -> form'
handleBlur' sym form = wrap <<< setTouched $ unwrap form
  where
    _sym :: Lens.Lens' (Record form) (InputField inp err out)
    _sym = prop sym
    setTouched = Lens.set (_sym <<< _Newtype <<< prop FSpec._touched) true


-- | Replace the value at a given field with a new value of the correct type.
onValueInputWith
  :: ∀ pq cq cs m sym form' form err out r props
   . IsSymbol sym
  => Cons sym (InputField String err out) r form
  => Newtype (form' InputField) (Record form)
  => SProxy sym
  -> HP.IProp (onInput :: Event, value :: String | props) (Query pq cq cs form' m Unit)
onValueInputWith sym =
  HE.onValueInput \str -> Just (handleChange sym str)

onValueChangeWith
  :: ∀ pq cq cs m sym form' form err out r props
   . IsSymbol sym
  => Cons sym (InputField String err out) r form
  => Newtype (form' InputField) (Record form)
  => SProxy sym
  -> HP.IProp (onChange :: Event, value :: String | props) (Query pq cq cs form' m Unit)
onValueChangeWith sym =
  HE.onValueChange \str -> Just (handleChange sym str)

onChangeWith
  :: ∀ pq cq cs m sym form' form i err out r props
   . IsSymbol sym
  => Cons sym (InputField i err out) r form
  => Newtype (form' InputField) (Record form)
  => SProxy sym
  -> i
  -> HP.IProp (onChange :: Event | props) (Query pq cq cs form' m Unit)
onChangeWith sym i =
  HE.onChange \_ -> Just (handleChange sym i)

onClickWith
  :: ∀ pq cq cs m sym form' form i err out r props
   . IsSymbol sym
  => Cons sym (InputField i err out) r form
  => Newtype (form' InputField) (Record form)
  => SProxy sym
  -> i
  -> HP.IProp (onClick :: MouseEvent | props) (Query pq cq cs form' m Unit)
onClickWith sym i =
  HE.onClick \_ -> Just (handleChange sym i)

handleChange
  :: ∀ pq cq cs m sym form' form inp err out r
   . IsSymbol sym
  => Cons sym (InputField inp err out) r form
  => Newtype (form' InputField) (Record form)
  => SProxy sym
  -> inp
  -> Query pq cq cs form' m Unit
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
