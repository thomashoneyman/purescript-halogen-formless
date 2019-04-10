-- | This module exports helpers for working with Formless queries.
-- | Since many queries are used as actions and may involve injecting
-- | variants, these helpers are provided to remove any associated
-- | boilerplate. Prefer these over using data constructors from the
-- | Formless query algebra.
module Formless.Query where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, wrap)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Time.Duration (Milliseconds)
import Data.Variant (Variant, inj)
import Formless.Class.Initial (class Initial, initial)
import Formless.Transform.Record (WrapField, wrapInputFields, wrapInputFunctions)
import Formless.Types.Component (Action(..), Message, PublicState, Query(..))
import Formless.Types.Form (InputField, InputFunction, OutputField, U(..))
import Halogen as H
import Halogen.Data.Slot as Slot
import Halogen.Query.ChildQuery as CQ
import Heterogeneous.Mapping as HM
import Prim.Row as Row

-- | Set the input value of a form field at the specified label
set
  :: forall form query ps sym inputs r e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) r inputs
  => SProxy sym
  -> i
  -> Query form query ps Unit
set sym i = H.tell $ Modify (wrap (inj sym (wrap (const i))))

set_
  :: forall form query ps m sym inputs r e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) r inputs
  => SProxy sym
  -> i
  -> Action form query ps m
set_ sym i = AsAction $ set sym i

-- | Modify the input value of a form field at the specified label with the
-- | provided function.
modify
  :: forall form query ps sym inputs r e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) r inputs
  => SProxy sym
  -> (i -> i)
  -> Query form query ps Unit
modify sym f = H.tell $ Modify (wrap (inj sym (wrap f)))

modify_
  :: forall form query ps m sym inputs r e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) r inputs
  => SProxy sym
  -> (i -> i)
  -> Action form query ps m
modify_ sym f = AsAction $ modify sym f

-- | A helper to create the correct `Validate` query for Formless, given
-- | a label
validate
  :: forall form query ps sym us r e i o
   . IsSymbol sym
  => Newtype (form Variant U) (Variant us)
  => Row.Cons sym (U e i o) r us
  => SProxy sym
  -> Query form query ps Unit
validate sym = H.tell $ Validate (wrap (inj sym U))

validate_
  :: forall form query ps m sym us r e i o
   . IsSymbol sym
  => Newtype (form Variant U) (Variant us)
  => Row.Cons sym (U e i o) r us
  => SProxy sym
  -> Action form query ps m
validate_ = AsAction <<< validate

-- | Set the input value of a form field at the specified label, also triggering
-- | validation to run on the field.
setValidate
  :: forall form query ps sym inputs r e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) r inputs
  => SProxy sym
  -> i
  -> Query form query ps Unit
setValidate sym i = H.tell $ ModifyValidate Nothing (wrap (inj sym (wrap (const i))))

setValidate_
  :: forall form query ps m sym inputs r e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) r inputs
  => SProxy sym
  -> i
  -> Action form query ps m
setValidate_ sym i = AsAction $ setValidate sym i

-- | Set the input value of a form field at the specified label, while debouncing
-- | validation so that it only runs after the specified amount of time has elapsed
-- | since the last modification. Useful when you need to avoid expensive validation
-- | but do not want to wait for a blur event to validate.
asyncSetValidate
  :: forall form query ps sym inputs r e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) r inputs
  => Milliseconds
  -> SProxy sym
  -> i
  -> Query form query ps Unit
asyncSetValidate ms sym i = 
  H.tell $ ModifyValidate (Just ms) (wrap (inj sym (wrap (const i))))

asyncSetValidate_
  :: forall form query ps m sym inputs r e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) r inputs
  => Milliseconds
  -> SProxy sym
  -> i
  -> Action form query ps m
asyncSetValidate_ ms sym i = AsAction $ asyncSetValidate ms sym i

-- | Modify the input value of a form field at the specified label, also triggering
-- | validation to run on the field, with the provided function.
modifyValidate
  :: forall form query ps sym inputs r e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) r inputs
  => SProxy sym
  -> (i -> i)
  -> Query form query ps Unit
modifyValidate sym f = H.tell $ ModifyValidate Nothing (wrap (inj sym (wrap f)))

modifyValidate_
  :: forall form query ps m sym inputs r e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) r inputs
  => SProxy sym
  -> (i -> i)
  -> Action form query ps m
modifyValidate_ sym f = AsAction $ modifyValidate sym f

-- | Modify the input value of a form field at the specified label, while debouncing
-- | validation so that it only runs after the specified amount of time has elapsed
-- | since the last modification. Useful when you need to avoid expensive validation
-- | but do not want to wait for a blur event to validate.
asyncModifyValidate
  :: forall form query ps sym inputs r e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) r inputs
  => Milliseconds
  -> SProxy sym
  -> (i -> i)
  -> Query form query ps Unit
asyncModifyValidate ms s f = H.tell $ ModifyValidate (Just ms) (wrap (inj s (wrap f)))

asyncModifyValidate_
  :: forall form query ps m sym inputs r e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) r inputs
  => Milliseconds
  -> SProxy sym
  -> (i -> i)
  -> Action form query ps m
asyncModifyValidate_ ms sym f = AsAction $ asyncModifyValidate ms sym f

-- | Reset the value of the specified form field to its default value
-- | according to the `Initial` type class.
reset
  :: forall form query ps sym inputs r e i o
   . IsSymbol sym
  => Initial i
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) r inputs
  => SProxy sym
  -> Query form query ps Unit
reset sym = H.tell $ Reset (wrap (inj sym (wrap (const initial))))

reset_
  :: forall form query ps m sym inputs r e i o
   . IsSymbol sym
  => Initial i
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) r inputs
  => SProxy sym
  -> Action form query ps m
reset_ = AsAction <<< reset

-- | Provide a record of input fields to overwrite all current
-- | inputs. Unlike `initialize`, this does not otherwise reset
-- | the form as if it were new. Similar to calling `set` on every
-- | field in the form.
setAll
  :: forall form query ps is is'
   . Newtype (form Record InputField) {| is' }
  => HM.HMap WrapField {| is } {| is' }
  => {| is } 
  -> Query form query ps Unit
setAll = H.tell <<< SetAll <<< wrapInputFields

setAll_
  :: forall form query ps m is is'
   . Newtype (form Record InputField) {| is' }
  => HM.HMap WrapField {| is } {| is' }
  => {| is } 
  -> Action form query ps m
setAll_ = AsAction <<< setAll

-- | Provide a record of input functions to modify all current
-- | inputs. Similar to calling `modify` on every field in the form.
modifyAll
  :: forall form query ps ifs' ifs
   . Newtype (form Record InputFunction) {| ifs' }
  => HM.HMap WrapField {| ifs } {| ifs' }
  => {| ifs }
  -> Query form query ps Unit
modifyAll = H.tell <<< ModifyAll <<< wrapInputFunctions

modifyAll_
  :: forall form query ps m ifs' ifs
   . Newtype (form Record InputFunction) {| ifs' }
  => HM.HMap WrapField {| ifs } {| ifs' }
  => {| ifs }
  -> Action form query ps m
modifyAll_ = AsAction <<< modifyAll

-- | Validate all fields in the form, collecting errors
validateAll :: forall form query ps. Query form query ps Unit
validateAll = H.tell ValidateAll

validateAll_ :: forall form query ps m. Action form query ps m
validateAll_ = AsAction validateAll

-- | Provide a record of inputs to overwrite all current inputs without
-- | resetting the form (as `initialize` does), and then validate the
-- | entire new set of fields. Similar to calling `setValidate` on every
-- | field in the form.
setValidateAll
  :: forall form query ps is' is
   . Newtype (form Record InputField) {| is' }
  => HM.HMap WrapField {| is } {| is' }
  => {| is }
  -> Query form query ps Unit
setValidateAll is = setAll is `andThen` validateAll

setValidateAll_
  :: forall form query ps m is' is
   . Newtype (form Record InputField) {| is' }
  => HM.HMap WrapField {| is } {| is' }
  => {| is }
  -> Action form query ps m
setValidateAll_ = AsAction <<< setValidateAll

-- | Provide a record of input functions to modify all current
-- | inputs, and then validate all fields.  Similar to calling
-- | `modifyValidate` on every field in the form.
modifyValidateAll
  :: forall form query ps ifs' ifs
   . Newtype (form Record InputFunction) {| ifs' }
  => HM.HMap WrapField {| ifs } {| ifs' }
  => {| ifs }
  -> Query form query ps Unit
modifyValidateAll ifs = modifyAll ifs `andThen` validateAll

modifyValidateAll_
  :: forall form query ps m ifs' ifs
   . Newtype (form Record InputFunction) {| ifs' }
  => HM.HMap WrapField {| ifs } {| ifs' }
  => {| ifs }
  -> Action form query ps m
modifyValidateAll_ = AsAction <<< modifyValidateAll

-- | Reset all fields to their initial values, and reset the form
-- | to its initial pristine state, no touched fields.
resetAll :: forall form query ps. Query form query ps Unit
resetAll = H.tell ResetAll

resetAll_ :: forall form query ps m. Action form query ps m
resetAll_ = AsAction resetAll

-- | Submit the form, which will trigger a `Submitted` result if the
-- | form validates successfully.
submit :: forall form query ps. Query form query ps Unit
submit = H.tell Submit

submit_ :: forall form query ps m. Action form query ps m
submit_ = AsAction submit

-- | Submit the form, returning the output of validation if successful
-- | and `Nothing` otherwise.
submitReply 
  :: forall form query ps a
   . (Maybe (form Record OutputField) -> a)
  -> Query form query ps a
submitReply = SubmitReply

-- | Get the current form state
getState :: forall form query ps a. (PublicState form -> a) -> Query form query ps a
getState = GetState

-- | Load a form from a set of existing inputs. Useful for when you need to mount
-- | Formless, perform some other actions like request data from the server, and 
-- | then load an existing set of inputs. 
loadForm :: forall form query ps. form Record InputField -> Query form query ps Unit
loadForm = H.tell <<< LoadForm

loadForm_ :: forall form query ps m. form Record InputField -> Action form query ps m
loadForm_ = AsAction <<< loadForm

-- | Perform two action-style queries in sequence, one after another.
andThen 
  :: forall form query ps
   . Query form query ps Unit
  -> Query form query ps Unit
  -> Query form query ps Unit
andThen a b = H.tell $ AndThen a b

andThen_
  :: forall form query ps m
   . Query form query ps Unit
  -> Query form query ps Unit
  -> Action form query ps m
andThen_ a b = AsAction $ andThen a b

-- | When you have specified a child component within Formless and need to query it,
-- | you can do so in two ways. 
-- |
-- | First, you can use `H.query` as usual within the `handleExtraQuery` function you 
-- | provide to Formless as input. You can, for example, write your own query which 
-- | manages the child component, write it into the render function you supply 
-- | Formless, and then when that query is triggered, query the child component. 
-- | 
-- | Second, you can use the `sendQuery` function within your parent component's
-- | `handleAction` or `handleQuery` functions. Given the slot for Formless, the
-- | slot for the child component within Formless, and the query you'd like to
-- | send the child component, `sendQuery` will run through Formless and return
-- | the query result to the parent.
sendQuery
  :: forall outSym outSlot inSym inSlot form query msg ps cq cm pps r0 r1 st act pmsg m a
   . IsSymbol outSym
  => IsSymbol inSym
  => Row.Cons outSym (Slot.Slot (Query form query ps) (Message form msg) outSlot) r0 pps
  => Row.Cons inSym (Slot.Slot cq cm inSlot) r1 ps
  => Ord outSlot
  => Ord inSlot
  => SProxy outSym
  -> outSlot 
  -> SProxy inSym 
  -> inSlot
  -> cq a
  -> H.HalogenM st act pps pmsg m (Maybe a)
sendQuery ol os il is cq =
  H.query ol os
    $ SendQuery
    $ CQ.mkChildQueryBox
    $ CQ.ChildQuery (\k -> maybe (pure Nothing) k <<< Slot.lookup il is) cq identity

-- | Extend Formless' query algebra with a new query. You should also provide a handler
-- | for embedded queries via the `handleExtraQuery` function provided as an argument to
-- | the component.
embed :: forall form query ps a. query a -> Query form query ps a
embed = Embed

embed_ :: forall form query ps m. query Unit -> Action form query ps m
embed_ = AsAction <<< embed

