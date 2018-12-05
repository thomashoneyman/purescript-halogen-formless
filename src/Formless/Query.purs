-- | This module exports helpers for working with Formless queries.
-- | Since many queries are used as actions and may involve injecting
-- | variants, these helpers are provided to remove any associated
-- | boilerplate. Prefer these over using data constructors from the
-- | Formless query algebra.
module Formless.Query where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Time.Duration (Milliseconds)
import Data.Variant (Variant, inj)
import Formless.Class.Initial (class Initial, initial)
import Formless.Transform.Record (WrapField, wrapInputFields, wrapInputFunctions)
import Formless.Types.Component (Query(..), PublicState)
import Formless.Types.Form (InputField, InputFunction, OutputField, U(..))
import Halogen (request) as H
import Halogen.Component.ChildPath (ChildPath, injQuery, injSlot)
import Heterogeneous.Mapping as HM
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
loadForm
  :: ∀ pq cq cs form m a
   . form Record InputField
	-> a
  -> Query pq cq cs form m a
loadForm = LoadForm

-- | `initialize` as an action, so you don't need to specify a `Unit`
-- | result. Use to skip a use of `Halogen.action`.
loadForm_
  :: ∀ pq cq cs form m
   . form Record InputField
  -> Query pq cq cs form m Unit
loadForm_ = flip LoadForm unit

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

-- | Wrap a query from an external component embedded in Formless so it fits
-- | the Formless query algebra. Any time this query is triggered, Formless
-- | will then pass it up to your parent component via the `Emit` message.
raise
  :: ∀ pq cq cs form m a
	 . pq Unit
  -> a
  -> Query pq cq cs form m a
raise = Raise

-- | `raise` as an action, so you don't need to specify a `Unit`
-- | result. Use to skip a use of `Halogen.action`.
raise_
  :: ∀ pq cq cs form m
	 . pq Unit
  -> Query pq cq cs form m Unit
raise_ = flip Raise unit

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
setValidate sym i = ModifyValidate Nothing (wrap (inj sym (wrap (const i))))

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
setValidate_ sym i = ModifyValidate Nothing (wrap (inj sym (wrap (const i)))) unit

-- | Set the input value of a form field at the specified label, while debouncing
-- | validation so that it only runs after the specified amount of time has elapsed
-- | since the last modification. Useful when you need to avoid expensive validation
-- | but do not want to wait for a blur event to validate.
asyncSetValidate
  :: ∀ pq cq cs form inputs m sym t0 e i o a
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) t0 inputs
  => Milliseconds
  -> SProxy sym
  -> i
  -> a
  -> Query pq cq cs form m a
asyncSetValidate ms sym i = ModifyValidate (Just ms) (wrap (inj sym (wrap (const i))))

-- | `asyncSetValidate` as an action, so you don't need to specify a `Unit`
-- | result. Use to skip a use of `Halogen.action`.
asyncSetValidate_
  :: ∀ pq cq cs form inputs m sym t0 e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) t0 inputs
  => Milliseconds
  -> SProxy sym
  -> i
  -> Query pq cq cs form m Unit
asyncSetValidate_ ms sym i = ModifyValidate (Just ms) (wrap (inj sym (wrap (const i)))) unit

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
modifyValidate sym f = ModifyValidate Nothing (wrap (inj sym (wrap f)))

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
modifyValidate_ sym f = ModifyValidate Nothing (wrap (inj sym (wrap f))) unit

-- | Modify the input value of a form field at the specified label, while debouncing
-- | validation so that it only runs after the specified amount of time has elapsed
-- | since the last modification. Useful when you need to avoid expensive validation
-- | but do not want to wait for a blur event to validate.
asyncModifyValidate
  :: ∀ pq cq cs form inputs m sym t0 e i o a
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) t0 inputs
  => Milliseconds
  -> SProxy sym
  -> (i -> i)
  -> a
  -> Query pq cq cs form m a
asyncModifyValidate ms sym f = ModifyValidate (Just ms) (wrap (inj sym (wrap f)))

-- | `asyncModifyValidate` as an action, so you don't need to specify a `Unit`
-- | result. Use to skip a use of `Halogen.action`.
asyncModifyValidate_
  :: ∀ pq cq cs form inputs m sym t0 e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) t0 inputs
  => Milliseconds
  -> SProxy sym
  -> (i -> i)
  -> Query pq cq cs form m Unit
asyncModifyValidate_ ms sym f = ModifyValidate (Just ms) (wrap (inj sym (wrap f))) unit

-- | Reset the value of the specified form field to its default value
-- | according to the `Initial` type class.
reset
  :: ∀ pq cq cs form inputs m sym a t0 e i o
   . IsSymbol sym
  => Initial i
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) t0 inputs
  => SProxy sym
  -> a
  -> Query pq cq cs form m a
reset sym = Reset (wrap (inj sym (wrap (const initial))))

-- | `reset` as an action, so you don't need to specify a `Unit`
-- | result. Use to skip a use of `Halogen.action`.
reset_
  :: ∀ pq cq cs form inputs m sym t0 e i o
   . IsSymbol sym
  => Initial i
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) t0 inputs
  => SProxy sym
  -> Query pq cq cs form m Unit
reset_ sym = Reset (wrap (inj sym (wrap (const initial)))) unit

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
-- | inputs. Unlike `initialize`, this does not otherwise reset
-- | the form as if it were new. Similar to calling `set` on every
-- | field in the form.
setAll
  :: ∀ pq cq cs form m a is is'
   . Newtype (form Record InputField) { | is' }
  => HM.HMap WrapField { | is } { | is' }
  => { | is }
	-> a
  -> Query pq cq cs form m a
setAll = SetAll <<< wrapInputFields

-- | `setAll` as an action, so you don't need to specify a `Unit`
-- | result. Use to skip a use of `Halogen.action`.
setAll_
  :: ∀ pq cq cs form m is is'
   . Newtype (form Record InputField) { | is' }
  => HM.HMap WrapField { | is } { | is' }
  => { | is }
  -> Query pq cq cs form m Unit
setAll_ is = SetAll (wrapInputFields is) unit

-- | Provide a record of input functions to modify all current
-- | inputs. Similar to calling `modify` on every field in the form.
modifyAll
  :: ∀ pq cq cs form m ifs ifs' a
   . Newtype (form Record InputFunction) { | ifs' }
  => HM.HMap WrapField { | ifs } { | ifs' }
  => { | ifs }
	-> a
  -> Query pq cq cs form m a
modifyAll = ModifyAll <<< wrapInputFunctions

-- | `modifyAll` as an action, so you don't need to specify a `Unit`
-- | result. Use to skip a use of `Halogen.action`.
modifyAll_
  :: ∀ pq cq cs form m ifs ifs'
   . Newtype (form Record InputFunction) { | ifs' }
  => HM.HMap WrapField { | ifs } { | ifs' }
  => { | ifs }
  -> Query pq cq cs form m Unit
modifyAll_ ifs = ModifyAll (wrapInputFunctions ifs) unit

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

-- | Provide a record of inputs to overwrite all current inputs without
-- | resetting the form (as `initialize` does), and then validate the
-- | entire new set of fields. Similar to calling `setValidate` on every
-- | field in the form.
setValidateAll
  :: ∀ pq cq cs form m a is is'
   . Newtype (form Record InputField) { | is' }
  => HM.HMap WrapField { | is } { | is' }
  => { | is }
	-> a
  -> Query pq cq cs form m a
setValidateAll is = setAll_ is `andThen` validateAll_

-- | `setValidateAll` as an action, so you don't need to specify a `Unit`
-- | result. Use to skip a use of `Halogen.action`.
setValidateAll_
  :: ∀ pq cq cs form m is is'
   . Newtype (form Record InputField) { | is' }
  => HM.HMap WrapField { | is } { | is' }
  => { | is }
  -> Query pq cq cs form m Unit
setValidateAll_ is = setAll_ is `andThen_` validateAll_

-- | Provide a record of input functions to modify all current
-- | inputs, and then validate all fields.  Similar to calling
-- | `modifyValidate` on every field in the form.
modifyValidateAll
  :: ∀ pq cq cs form m ifs ifs' a
   . Newtype (form Record InputFunction) { | ifs' }
  => HM.HMap WrapField { | ifs } { | ifs' }
  => { | ifs }
	-> a
  -> Query pq cq cs form m a
modifyValidateAll ifs = modifyAll_ ifs `andThen` validateAll_

-- | `modifyValidateAll` as an action, so you don't need to specify a `Unit`
-- | result. Use to skip a use of `Halogen.action`.
modifyValidateAll_
  :: ∀ pq cq cs form m ifs ifs'
   . Newtype (form Record InputFunction) { | ifs' }
  => HM.HMap WrapField { | ifs } { | ifs' }
  => { | ifs }
  -> Query pq cq cs form m Unit
modifyValidateAll_ ifs = modifyAll_ ifs `andThen_` validateAll_
