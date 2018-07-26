module Formless.Events where

import Prelude

import Data.Lens as Lens
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (class IsSymbol, SProxy)
import Formless (Query(..))
import Formless.Class.Initial (class Initial, initial)
import Formless.Spec as Spec
import Halogen.Component.ChildPath (ChildPath, injQuery, injSlot)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prim.Row (class Cons)
import Web.Event.Event (Event)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.MouseEvent (MouseEvent)

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
  => Cons sym (Spec.InputField e i o) r form
  => Newtype (form' Spec.InputField) (Record form)
  => SProxy sym
  -> (i -> i)
  -> Query pq cq cs form' out m Unit
modify sym f = HandleChange (modify' sym f) unit

-- | Allows you to modify a field rather than set its value
modify'
  :: ∀ sym form form' e i o r
   . IsSymbol sym
  => Cons sym (Spec.InputField e i o) r form
  => Newtype form' (Record form)
  => SProxy sym
  -> (i -> i)
  -> form'
  -> form'
modify' sym f = wrap <<< setInput f <<< setTouched true <<< unwrap
  where
    _sym :: Lens.Lens' (Record form) (Spec.InputField e i o)
    _sym = prop sym
    setInput =
      Lens.over (_sym <<< _Newtype <<< prop Spec._input)
    setTouched =
      Lens.set (_sym <<< _Newtype <<< prop Spec._touched)

-- | Handles resetting a single field, but is only possible if the field is
-- | a member of the Initial type class
handleReset
  :: ∀ pq cq cs m sym form' form i e o out r
   . IsSymbol sym
  => Cons sym (Spec.InputField e i o) r form
  => Newtype (form' Spec.InputField) (Record form)
  => Initial i
  => SProxy sym
  -> Query pq cq cs form' out m Unit
handleReset sym = HandleReset (handleReset' sym) unit

handleReset'
  :: ∀ sym form' form i e o r
   . IsSymbol sym
  => Cons sym (Spec.InputField e i o) r form
  => Newtype form' (Record form)
  => Initial i
  => SProxy sym
  -> form'
  -> form'
handleReset' sym = wrap <<< unsetTouched <<< unsetResult <<< unsetValue <<< unwrap
  where
    _sym :: Lens.Lens' (Record form) (Spec.InputField e i o)
    _sym = prop sym
    unsetTouched = Lens.set (_sym <<< _Newtype <<< prop Spec._touched) false
    unsetResult = Lens.set (_sym <<< _Newtype <<< prop Spec._result) Nothing
    unsetValue = Lens.set (_sym <<< _Newtype <<< prop Spec._input) initial

-- | Performs behaviors for both blur and change events
handleBlurAndChange
  :: ∀ pq cq cs m sym form' form i e o out r
   . IsSymbol sym
  => Cons sym (Spec.InputField e i o) r form
  => Newtype (form' Spec.InputField) (Record form)
  => SProxy sym
  -> i
  -> Query pq cq cs form' m out Unit
handleBlurAndChange sym val = HandleBlur (handleBlur' sym <<< handleChange' sym val) unit

onClickWith
  :: ∀ pq cq cs m sym form' form i e o out r props
   . IsSymbol sym
  => Cons sym (Spec.InputField e i o) r form
  => Newtype (form' Spec.InputField) (Record form)
  => SProxy sym
  -> i
  -> HP.IProp (onClick :: MouseEvent | props) (Query pq cq cs form' out m Unit)
onClickWith sym i =
  HE.onClick \_ -> Just (handleBlurAndChange sym i)

-- | Given a proxy symbol, will trigger validation on that field using
-- | its validator and current input
onBlurWith
  :: ∀ pq cq cs m sym form' form i e o out r props
   . IsSymbol sym
  => Cons sym (Spec.InputField e i o) r form
  => Newtype (form' Spec.InputField) (Record form)
  => SProxy sym
  -> HP.IProp (onBlur :: FocusEvent | props) (Query pq cq cs form' out m Unit)
onBlurWith sym = HE.onBlur $ const $ Just $ handleBlur sym

handleBlur
  :: ∀ pq cq cs m sym form' form i e o out r
   . IsSymbol sym
  => Cons sym (Spec.InputField e i o) r form
  => Newtype (form' Spec.InputField) (Record form)
  => SProxy sym
  -> Query pq cq cs form' m out Unit
handleBlur sym = HandleBlur (handleBlur' sym) unit

handleBlur'
  :: ∀ sym form' form i e o r
   . IsSymbol sym
  => Cons sym (Spec.InputField e i o) r form
  => Newtype form' (Record form)
  => SProxy sym
  -> form'
  -> form'
handleBlur' sym form = wrap <<< setTouched $ unwrap form
  where
    _sym :: Lens.Lens' (Record form) (Spec.InputField e i o)
    _sym = prop sym
    setTouched = Lens.set (_sym <<< _Newtype <<< prop Spec._touched) true

-- | Replace the value at a given field with a new value of the correct type.
onValueInputWith
  :: ∀ pq cq cs m sym form' form e o out r props
   . IsSymbol sym
  => Cons sym (Spec.InputField e String o) r form
  => Newtype (form' Spec.InputField) (Record form)
  => SProxy sym
  -> HP.IProp (onInput :: Event, value :: String | props) (Query pq cq cs form' out m Unit)
onValueInputWith sym =
  HE.onValueInput \str -> Just (handleChange sym str)

onValueChangeWith
  :: ∀ pq cq cs m sym form' form e o out r props
   . IsSymbol sym
  => Cons sym (Spec.InputField e String o) r form
  => Newtype (form' Spec.InputField) (Record form)
  => SProxy sym
  -> HP.IProp (onChange :: Event, value :: String | props) (Query pq cq cs form' out m Unit)
onValueChangeWith sym =
  HE.onValueChange \str -> Just (handleChange sym str)

onChangeWith
  :: ∀ pq cq cs m sym form' form i e o out r props
   . IsSymbol sym
  => Cons sym (Spec.InputField e i o) r form
  => Newtype (form' Spec.InputField) (Record form)
  => SProxy sym
  -> i
  -> HP.IProp (onChange :: Event | props) (Query pq cq cs form' out m Unit)
onChangeWith sym i =
  HE.onChange \_ -> Just (handleChange sym i)

handleChange
  :: ∀ pq cq cs m sym form' form i e o out r
   . IsSymbol sym
  => Cons sym (Spec.InputField e i o) r form
  => Newtype (form' Spec.InputField) (Record form)
  => SProxy sym
  -> i
  -> Query pq cq cs form' out m Unit
handleChange sym val = HandleChange (handleChange' sym val) unit

handleChange'
  :: ∀ sym form form' e i o r
   . IsSymbol sym
  => Cons sym (Spec.InputField e i o) r form
  => Newtype form' (Record form)
  => SProxy sym
  -> i
  -> form'
  -> form'
handleChange' sym val = wrap <<< setInput val <<< setTouched true <<< unwrap
  where
    _sym :: Lens.Lens' (Record form) (Spec.InputField e i o)
    _sym = prop sym
    setInput =
      Lens.set (_sym <<< _Newtype <<< prop Spec._input)
    setTouched =
      Lens.set (_sym <<< _Newtype <<< prop Spec._touched)

