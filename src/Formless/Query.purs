-- | This module exports helpers for working with Formless queries.
-- | Since many queries are used as actions and may involve injecting
-- | variants, these helpers are provided to remove any associated
-- | boilerplate. Prefer these over using data constructors from the
-- | Formless query algebra.
module Formless.Query where

import Prelude

import Data.Newtype (class Newtype, wrap)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant, inj)
import Formless.Class.Initial (class Initial, initial)
import Formless.Types.Form (InputField, InputFunction, U(..))
import Formless.Types.Component (Query(..))
import Halogen.Component.ChildPath (ChildPath, injQuery, injSlot)
import Prim.Row as Row

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

-- | Set the input value of a form field at the specified label
set
  :: ∀ pq cq cs form inputs m sym t0 e i o a
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) t0 inputs
  => SProxy sym
  -> i
  -> a
  -> Query pq cq cs form m a
set sym i = ModifyInput (wrap (inj sym (wrap (const i))))

-- | Set the input value of a form field at the specified label, as an action.
set_
  :: ∀ pq cq cs form inputs m sym t0 e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) t0 inputs
  => SProxy sym
  -> i
  -> Query pq cq cs form m Unit
set_ sym i = ModifyInput (wrap (inj sym (wrap (const i)))) unit

-- | Set the input value of a form field at the specified label, also triggering
-- | validation to run on the field.
setValidate
  :: ∀ pq cq cs form inputs m sym t0 e i o a
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) t0 inputs
  => SProxy sym
  -> i
  -> a
  -> Query pq cq cs form m a
setValidate sym i = ModifyValidateInput (wrap (inj sym (wrap (const i))))

-- | Set the input value of a form field at the specified label, also triggering
-- | validation to run on the field, as an action.
setValidate_
  :: ∀ pq cq cs form inputs m sym t0 e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) t0 inputs
  => SProxy sym
  -> i
  -> Query pq cq cs form m Unit
setValidate_ sym i = ModifyValidateInput (wrap (inj sym (wrap (const i)))) unit

-- | ModifyInput the input value of a form field at the specified label with the
-- | provided function.
modify
  :: ∀ pq cq cs form inputs m sym t0 e i o a
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) t0 inputs
  => SProxy sym
  -> (i -> i)
  -> a
  -> Query pq cq cs form m a
modify sym f = ModifyInput (wrap (inj sym (wrap f)))

-- | ModifyInput the input value of a form field at the specified label, as an action,
-- | with the provided function.
modify_
  :: ∀ pq cq cs form inputs m sym t0 e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) t0 inputs
  => SProxy sym
  -> (i -> i)
  -> Query pq cq cs form m Unit
modify_ sym f = ModifyInput (wrap (inj sym (wrap f))) unit

-- | ModifyInput the input value of a form field at the specified label, also triggering
-- | validation to run on the field, with the provided function.
modifyValidate
  :: ∀ pq cq cs form inputs m sym t0 e i o a
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) t0 inputs
  => SProxy sym
  -> (i -> i)
  -> a
  -> Query pq cq cs form m a
modifyValidate sym f = ModifyValidateInput (wrap (inj sym (wrap f)))

-- | ModifyInput the input value of a form field at the specified label, also triggering
-- | validation to run on the field, as an action, with the provided function.
modifyValidate_
  :: ∀ pq cq cs form inputs m sym t0 e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) t0 inputs
  => SProxy sym
  -> (i -> i)
  -> Query pq cq cs form m Unit
modifyValidate_ sym f = ModifyValidateInput (wrap (inj sym (wrap f))) unit

-- | Reset the value of the specified form field to its default value
-- | according to the `Initial` type class.
reset
  :: ∀ pq cq cs form inputs m sym a t0 e i o
   . IsSymbol sym
  => Initial i
  => Newtype (form Variant InputField) (Variant inputs)
  => Row.Cons sym (InputField e i o) t0 inputs
  => SProxy sym
  -> a
  -> Query pq cq cs form m a
reset sym = ResetInput (wrap (inj sym (wrap initial)))

-- | Reset the value of the specified form field to its default value
-- | according to the `Initial` type class, as an action.
reset_
  :: ∀ pq cq cs form inputs m sym t0 e i o
   . IsSymbol sym
  => Initial i
  => Newtype (form Variant InputField) (Variant inputs)
  => Row.Cons sym (InputField e i o) t0 inputs
  => SProxy sym
  -> Query pq cq cs form m Unit
reset_ sym = ResetInput (wrap (inj sym (wrap initial))) unit

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
validate sym = ValidateInput (wrap (inj sym U))

-- | A helper to create the correct `Validate` query for Formless given
-- | a label, as an action
validate_
  :: ∀ pq cq cs form us m sym t0 e i o
   . IsSymbol sym
  => Newtype (form Variant U) (Variant us)
  => Row.Cons sym (U e i o) t0 us
  => SProxy sym
  -> Query pq cq cs form m Unit
validate_ sym = ValidateInput (wrap (inj sym U)) unit

