-- | This module exports helpers for working with Formless queries.
-- | Since many queries are used as actions and may involve injecting
-- | variants, these helpers are provided to remove any associated
-- | boilerplate. Prefer these over using data constructors from the
-- | Formless query algebra.
module Formless.Query where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, wrap)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant, inj)
import Formless.Class.Initial (class Initial, initial)
import Formless.Types.Component (Query(..), PublicState)
import Formless.Types.Form (InputField, InputFunction, OutputField, U(..))
import Halogen (request) as H
import Halogen.Component.ChildPath (ChildPath, injQuery, injSlot)
import Prim.Row as Row

-- | Send a query transparently through Formless, when you only have one kind of child
-- | component. Use when you would use `Halogen.query`.
send :: ∀ pq cs cq form m a
  . cs
 -> cq a
 -> Query pq cq cs form m a
send p q = Send p q

-- | Send a query transparently through Formless, when you are using several
-- | different types of child components and the component needs a child path
-- | to dispatch the query to. Use when you would use `Halogen.query'`.
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
set sym i = Modify (wrap (inj sym (wrap (const i))))

-- | `set` as an action, so you don't need to specify a `Unit`
-- | result. Use to skip a use of `Halogen.action`.
set_
  :: ∀ pq cq cs form inputs m sym t0 e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) t0 inputs
  => SProxy sym
  -> i
  -> Query pq cq cs form m Unit
set_ sym i = Modify (wrap (inj sym (wrap (const i)))) unit

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
setValidate sym i = ModifyValidate (wrap (inj sym (wrap (const i))))

-- | `setValidate` as an action, so you don't need to specify a `Unit`
-- | result. Use to skip a use of `Halogen.action`.
setValidate_
  :: ∀ pq cq cs form inputs m sym t0 e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) t0 inputs
  => SProxy sym
  -> i
  -> Query pq cq cs form m Unit
setValidate_ sym i = ModifyValidate (wrap (inj sym (wrap (const i)))) unit

-- | Modify the input value of a form field at the specified label with the
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
modify sym f = Modify (wrap (inj sym (wrap f)))

-- | `modify` as an action, so you don't need to specify a `Unit`
-- | result. Use to skip a use of `Halogen.action`.
modify_
  :: ∀ pq cq cs form inputs m sym t0 e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) t0 inputs
  => SProxy sym
  -> (i -> i)
  -> Query pq cq cs form m Unit
modify_ sym f = Modify (wrap (inj sym (wrap f))) unit

-- | Modify the input value of a form field at the specified label, also triggering
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
modifyValidate sym f = ModifyValidate (wrap (inj sym (wrap f)))

-- | `modifyValidate` as an action, so you don't need to specify a `Unit`
-- | result. Use to skip a use of `Halogen.action`.
modifyValidate_
  :: ∀ pq cq cs form inputs m sym t0 e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) t0 inputs
  => SProxy sym
  -> (i -> i)
  -> Query pq cq cs form m Unit
modifyValidate_ sym f = ModifyValidate (wrap (inj sym (wrap f))) unit

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
reset sym = Reset (wrap (inj sym (wrap initial)))

-- | `reset` as an action, so you don't need to specify a `Unit`
-- | result. Use to skip a use of `Halogen.action`.
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

-- | `validate` as an action, so you don't need to specify a `Unit`
-- | result. Use to skip a use of `Halogen.action`.
validate_
  :: ∀ pq cq cs form us m sym t0 e i o
   . IsSymbol sym
  => Newtype (form Variant U) (Variant us)
  => Row.Cons sym (U e i o) t0 us
  => SProxy sym
  -> Query pq cq cs form m Unit
validate_ sym = Validate (wrap (inj sym U)) unit

-- | Provide a record of input fields to overwrite all current
-- | inputs. Unlike `replaceInputs`, this does not otherwise reset
-- | the form as if it were new. Similar to calling `set` on every
-- | field in the form.
setAll
  :: ∀ pq cq cs form m a
   . form Record InputField
	-> a
  -> Query pq cq cs form m a
setAll = SetAll

-- | `setAll` as an action, so you don't need to specify a `Unit`
-- | result. Use to skip a use of `Halogen.action`.
setAll_
  :: ∀ pq cq cs form m
   . form Record InputField
  -> Query pq cq cs form m Unit
setAll_ = flip SetAll unit

-- | Provide a record of input functions to modify all current
-- | inputs. Similar to calling `modify` on every field in the form.
modifyAll
  :: ∀ pq cq cs form m a
   . form Record InputFunction
	-> a
  -> Query pq cq cs form m a
modifyAll = ModifyAll

-- | `modifyAll` as an action, so you don't need to specify a `Unit`
-- | result. Use to skip a use of `Halogen.action`.
modifyAll_
  :: ∀ pq cq cs form m
   . form Record InputFunction
  -> Query pq cq cs form m Unit
modifyAll_ = flip ModifyAll unit

-- | Reset all fields to their initial values, and reset the form
-- | to its initial pristine state, no touched fields.
resetAll
  :: ∀ pq cq cs form m a
	 . a
  -> Query pq cq cs form m a
resetAll = ResetAll

-- | `resetAll` as an action, so you don't need to specify a `Unit`
-- | result. Use to skip a use of `Halogen.action`.
resetAll_
  :: ∀ pq cq cs form m
   . Query pq cq cs form m Unit
resetAll_ = ResetAll unit

-- | Validate all fields in the form, collecting errors
validateAll
  :: ∀ pq cq cs form m a
	 . a
  -> Query pq cq cs form m a
validateAll = ValidateAll

-- | `validateAll` as an action, so you don't need to specify a `Unit`
-- | result. Use to skip a use of `Halogen.action`.
validateAll_
  :: ∀ pq cq cs form m
   . Query pq cq cs form m Unit
validateAll_ = ValidateAll unit

-- | Submit the form, which will trigger a `Submitted` result if the
-- | form validates successfully.
submit
  :: ∀ pq cq cs form m a
	 . a
  -> Query pq cq cs form m a
submit = Submit

-- | `submit` as an action, so you don't need to specify a `Unit`
-- | result. Use to skip a use of `Halogen.action`.
submit_
  :: ∀ pq cq cs form m
   . Query pq cq cs form m Unit
submit_ = Submit unit

-- | Imperatively submit the form and collect the result of submission, without
-- | triggering a `Submitted` message. Useful when you need to submit multiple
-- | forms together without listening to submission events.
submitReply
	:: ∀ pq cq cs form m
	 . Query pq cq cs form m (Maybe (form Record OutputField))
submitReply = H.request SubmitReply

-- | Imperatively receive the current state of the form.
getState
	:: ∀ pq cq cs form m
	 . Query pq cq cs form m (PublicState form)
getState = H.request GetState

-- | Replace all form inputs with a new set of inputs, and re-initialize
-- | the form to a new state. Useful to set a new "initial state" for a form,
-- | especially when filling a form with data loaded asynchronously.
initialize
  :: ∀ pq cq cs form m a
   . form Record InputField
	-> a
  -> Query pq cq cs form m a
initialize = Initialize

-- | `initialize` as an action, so you don't need to specify a `Unit`
-- | result. Use to skip a use of `Halogen.action`.
initialize_
  :: ∀ pq cq cs form m
   . form Record InputField
  -> Query pq cq cs form m Unit
initialize_ = flip Initialize unit

-- | Perform two Formless actions in sequence. Can be chained arbitrarily.
-- | Useful when a field needs to modify itself on change and also trigger
-- | validation on one or more other fields, or when a modification on one
-- | field should also modify another field.
andThen
  :: ∀ pq cq cs form m a
	 . Query pq cq cs form m Unit
  -> Query pq cq cs form m Unit
  -> a
  -> Query pq cq cs form m a
andThen = AndThen

-- | `andThen` as an action, so you don't need to specify a `Unit`
-- | result. Use to skip a use of `Halogen.action`.
andThen_
  :: ∀ pq cq cs form m
	 . Query pq cq cs form m Unit
  -> Query pq cq cs form m Unit
  -> Query pq cq cs form m Unit
andThen_ a b = AndThen a b unit
