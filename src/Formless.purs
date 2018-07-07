-- | Formless is a renderless component to help you build forms in Halogen.
-- | It expects that you have already written a form spec and validation and
-- | you simply need a component to run it on your behalf.

module Formless where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, store)
import Data.Lens as Lens
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (class IsSymbol, SProxy)
import Effect.Aff.Class (class MonadAff)
import Formless.Spec (InputField, OutputField, _input, _result, _touched, _validator)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prim.Row (class Cons)
import Renderless.State (modifyState_, modifyStore_)
import Web.Event.Event (Event)
import Web.UIEvent.FocusEvent (FocusEvent)

data Query pq cq cs (form :: (Type -> Type -> Type -> Type) -> Type) m a
  = HandleBlur (form InputField -> form InputField) a
  | HandleChange (form InputField -> form InputField) a
  | ValidateAll a
  | Submit a
  | Raise (pq Unit) a
  | Receive (Input pq cq cs form m) a

-- | The overall component state type, which contains the local state type
-- | and also the render function
type StateStore pq cq cs form m =
  Store (State form) (HTML pq cq cs form m)

-- | The component type
type Component pq cq cs form m
  = H.Component HH.HTML (Query pq cq cs form m) (Input pq cq cs form m) (Message pq) m

-- | The component's HTML type, the result of the render function.
type HTML pq cq cs form m
  = H.ParentHTML (Query pq cq cs form m) cq cs m

-- | The component's DSL type, the result of the eval function.
type DSL pq cq cs form m
  = H.ParentDSL (StateStore pq cq cs form m) (Query pq cq cs form m) cq cs (Message pq) m

-- | The component local state
type State form =
  { isValid :: Boolean
  , formResult :: Maybe (form OutputField)
  , errors :: Int -- Count of all error fields. Starts at 0. Validators increment / decrement.
  , formSpec :: form InputField
  , form :: form InputField
  }

-- | The component's input type
type Input pq cq cs (form :: (Type -> Type -> Type -> Type) -> Type) m =
  { formSpec :: form InputField
  , render :: State form -> HTML pq cq cs form m }

data Message pq
  = Submitted
  | Emit (pq Unit)

-- | The component itself
component :: ∀ pq cq cs form m. Ord cs => MonadAff m => Component pq cq cs form m
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
    , formSpec -- This should be the original form spec from the user.
    , form: formSpec -- TODO: formSpecToInputFields formSpec
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
      -- traverse the record calling all validate functions
      -- call any record-wide validation functions
      pure a

    Submit a -> do
      H.raise Submitted
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
handleBlur' sym form = wrap <<< setResult <<< setTouched $ unwrap form
  where
    _sym :: Lens.Lens' (Record form) (InputField inp err out)
    _sym = prop sym
    input = Lens.view (_sym <<< _Newtype <<< prop _input) (unwrap form)
    validator = Lens.view (_sym <<< _Newtype <<< prop _validator) (unwrap form)
    setResult = Lens.set (_sym <<< _Newtype <<< prop _result) (Just $ validator input)
    setTouched = Lens.set (_sym <<< _Newtype <<< prop _touched) true

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
    setInput = Lens.set (_sym <<< _Newtype <<< prop _input)
    setTouched = Lens.set (_sym <<< _Newtype <<< prop _touched)
