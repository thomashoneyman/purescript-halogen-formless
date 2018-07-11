-- | Formless is a renderless component to help you build forms in Halogen.
-- | It expects that you have already written a form spec and validation and
-- | you simply need a component to run it on your behalf.

module Formless where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, store)
import Data.Either (Either(..))
import Data.Lens as Lens
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (class IsSymbol, SProxy)
import Debug.Trace (spy)
import Formless.Record as FR
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

data Query pq cq cs (form :: (Type -> Type -> Type -> Type) -> Type) m a
  = HandleBlur (form InputField -> form InputField) a
  | HandleChange (form InputField -> form InputField) a
  | ValidateAll a
  | Submit a
  | Send cs (cq Unit) a
  | Raise (pq Unit) a
  | Receive (Input pq cq cs form m) a

-- | The overall component state type, which contains the local state type
-- | and also the render function
type StateStore pq cq cs form m =
  Store (State form) (HTML pq cq cs form m)

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
type State form =
  { isValid :: Boolean
  , formResult :: Maybe (form OutputField)
  , errors :: Int
  , formSpec :: form FormSpec
  , form :: form InputField
  }

-- | The component's input type
type Input pq cq cs (form :: (Type -> Type -> Type -> Type) -> Type) m =
  { formSpec :: form FormSpec
  , render :: State form -> HTML pq cq cs form m }

data Message pq form
  = Submitted (Either (form InputField) (form OutputField))
  | Emit (pq Unit)

-- | The component itself
component
  :: ∀ pq cq cs form m spec specxs field fieldxs mboutput mboutputxs output
   . Ord cs
  => RL.RowToList spec specxs
  => RL.RowToList field fieldxs
  => RL.RowToList mboutput mboutputxs
  => FR.FormSpecToInputField specxs spec () field
  --  => FR.ValidateInputFields fieldxs field () field
  => FR.InputFieldToMaybeOutput fieldxs field () mboutput
  => FR.MaybeOutputToOutputField mboutputxs mboutput () output
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
  initialState { formSpec, render } = store render $
    { isValid: false
    , errors: 0
    , formResult: Nothing
    , formSpec
    , form: FR.formSpecToInputFields formSpec
    }

  eval :: Query pq cq cs form m ~> DSL pq cq cs form m
  eval = case _ of
    HandleBlur fs a -> do
      modifyState_ \st -> st { form = fs st.form }
      pure a

    HandleChange fs a -> do
      modifyState_ \st -> st { form = fs st.form }
      pure a

    ValidateAll a -> do
      st <- getState
      let form = st.form -- FR.validateInputFields st.form
      modifyState_ _
        { form = form
        , formResult = FR.maybeOutputToOutputField $ FR.inputFieldToMaybeOutput form
        }
      pure a

    Submit a -> do
      _ <- eval $ ValidateAll a
      st <- getState
      H.raise $ maybe (Submitted $ Left st.form) (Submitted <<< Right) st.formResult
      pure a

    Send cs cq a -> do
      let _ = spy "Received send childslot: " cs
          _ = spy "Received send childquery: " cq
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
    input =
      Lens.view (_sym <<< _Newtype <<< prop FSpec._input) (unwrap form)
    --  validator =
    --    Lens.view (_sym <<< _Newtype <<< prop FSpec._validator) (unwrap form)
    --  setResult =
    --    Lens.set (_sym <<< _Newtype <<< prop FSpec._result) (Just $ validator input)
    setTouched =
      Lens.set (_sym <<< _Newtype <<< prop FSpec._touched) true

-- | Replace the value at a given field with a new value of the correct type.
onValueInputWith
  :: ∀ pq cq cs m sym form' form err out r props
   . IsSymbol sym
  => Cons sym (InputField String err out) r form
  => Newtype (form' InputField) (Record form)
  => SProxy sym
  -> HP.IProp (onInput :: Event, value :: String | props) (Query pq cq cs form' m Unit)
onValueInputWith sym =
  HE.onValueInput $ \str -> Just (handleChange sym str)

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
