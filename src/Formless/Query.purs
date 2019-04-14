-- | This module exports helpers for working with Formless queries. Action-style
-- | queries are already specialized to `Unit` for you. Prefer these over using
-- | data constructors from the Formless query algebra. Remember that you can 
-- | freely extend the Formless query algebra with your own queries by using the
-- | `injQuery` function.
module Formless.Query where

import Prelude

import Data.Functor.Variant as VF
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, wrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Time.Duration (Milliseconds)
import Data.Variant (Variant, inj)
import Formless.Class.Initial (class Initial, initial)
import Formless.Transform.Record (WrapField, wrapInputFields, wrapInputFunctions)
import Formless.Types.Component (Message, QueryF(..), Query)
import Formless.Types.Form (InputField, InputFunction, OutputField, U(..))
import Halogen as H
import Halogen.Data.Slot as Slot
import Halogen.Query.ChildQuery as CQ
import Heterogeneous.Mapping as HM
import Prim.Row as Row

-- | Inject your own query into the Formless component. You will need to derive
-- | a `Functor` instance for your query type.
-- |
-- | ```purescript
-- | data MyQuery a = DoSomething a
-- | derive instance functorMyQuery :: Functor MyQuery
-- | ```
injQuery :: forall form query ps a. Functor query => query a -> Query form query ps a
injQuery = VF.inj (SProxy :: _ "userQuery")

-- | Set the input value of a form field at the specified label
set
  :: forall form query ps sym inputs r e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) r inputs
  => SProxy sym
  -> i
  -> Query form query ps Unit
set sym i = 
  VF.inj (SProxy :: _ "query") $ H.tell $ Modify (wrap (inj sym (wrap (const i))))

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
modify sym f = 
  VF.inj (SProxy :: _ "query") $ H.tell $ Modify (wrap (inj sym (wrap f)))

-- | A helper to create the correct `Validate` query for Formless, given
-- | a label
validate
  :: forall form query ps sym us r e i o
   . IsSymbol sym
  => Newtype (form Variant U) (Variant us)
  => Row.Cons sym (U e i o) r us
  => SProxy sym
  -> Query form query ps Unit
validate sym = 
  VF.inj (SProxy :: _ "query") $ H.tell $ Validate (wrap (inj sym U))

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
setValidate sym i = 
  VF.inj (SProxy :: _ "query") 
    $ H.tell 
    $ ModifyValidate Nothing (wrap (inj sym (wrap (const i))))

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
modifyValidate sym f = 
  VF.inj (SProxy :: _ "query") $ H.tell $ ModifyValidate Nothing (wrap (inj sym (wrap f)))

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
  VF.inj (SProxy :: _ "query") 
    $ H.tell 
    $ ModifyValidate (Just ms) (wrap (inj sym (wrap (const i))))

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
asyncModifyValidate ms s f = 
  VF.inj (SProxy :: _ "query") $ H.tell $ ModifyValidate (Just ms) (wrap (inj s (wrap f)))

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
reset sym = 
  VF.inj (SProxy :: _ "query") $ H.tell $ Reset (wrap (inj sym (wrap (const initial))))

-- | Provide a record of input fields to overwrite all current
-- | inputs. Unlike `loadForm`, this does not otherwise reset
-- | the form as if it were new. Similar to calling `set` on every
-- | field in the form. Does not run validation.
setAll
  :: forall form query ps is is'
   . Newtype (form Record InputField) {| is' }
  => HM.HMap WrapField {| is } {| is' }
  => {| is } 
  -> Query form query ps Unit
setAll is = 
  VF.inj (SProxy :: _ "query") $ H.tell $ SetAll (wrapInputFields is) false

-- | Provide a record of input functions to modify all current
-- | inputs. Similar to calling `modify` on every field in the form.
-- | Does not run validation.
modifyAll
  :: forall form query ps ifs' ifs
   . Newtype (form Record InputFunction) {| ifs' }
  => HM.HMap WrapField {| ifs } {| ifs' }
  => {| ifs }
  -> Query form query ps Unit
modifyAll fs = 
  VF.inj (SProxy :: _ "query") $ H.tell $ ModifyAll (wrapInputFunctions fs) false

-- | Validate all fields in the form, collecting errors
validateAll :: forall form query ps. Query form query ps Unit
validateAll = 
  VF.inj (SProxy :: _ "query") $ H.tell ValidateAll

-- | Provide a record of inputs to overwrite all current inputs without
-- | resetting the form (as `loadForm` does), and then validate the
-- | entire new set of fields. Similar to calling `setValidate` on every
-- | field in the form.
setValidateAll
  :: forall form query ps is' is
   . Newtype (form Record InputField) {| is' }
  => HM.HMap WrapField {| is } {| is' }
  => {| is }
  -> Query form query ps Unit
setValidateAll is = 
  VF.inj (SProxy :: _ "query") $ H.tell $ SetAll (wrapInputFields is) true

-- | Provide a record of input functions to modify all current
-- | inputs, and then validate all fields.  Similar to calling
-- | `modifyValidate` on every field in the form.
modifyValidateAll
  :: forall form query ps ifs' ifs
   . Newtype (form Record InputFunction) {| ifs' }
  => HM.HMap WrapField {| ifs } {| ifs' }
  => {| ifs }
  -> Query form query ps Unit
modifyValidateAll ifs = 
  VF.inj (SProxy :: _ "query") $ H.tell $ ModifyAll (wrapInputFunctions ifs) true

-- | Reset all fields to their initial values, and reset the form
-- | to its initial pristine state, no touched fields.
resetAll :: forall form query ps. Query form query ps Unit
resetAll = 
  VF.inj (SProxy :: _ "query") $ H.tell ResetAll

-- | Submit the form, which will trigger a `Submitted` result if the
-- | form validates successfully.
submit :: forall form query ps. Query form query ps Unit
submit = 
  VF.inj (SProxy :: _ "query") $ H.tell Submit

-- | Submit the form, returning the output of validation if successful
-- | and `Nothing` otherwise.
submitReply 
  :: forall form query ps a
   . (Maybe (form Record OutputField) -> a)
  -> Query form query ps a
submitReply = VF.inj (SProxy :: _ "query") <<< SubmitReply

-- | Load a form from a set of existing inputs. Useful for when you need to mount
-- | Formless, perform some other actions like request data from the server, and 
-- | then load an existing set of inputs. 
loadForm :: forall form query ps. form Record InputField -> Query form query ps Unit
loadForm = VF.inj (SProxy :: _ "query") <<< H.tell <<< LoadForm

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
    $ VF.inj (SProxy :: _ "query") 
    $ SendQuery
    $ CQ.mkChildQueryBox
    $ CQ.ChildQuery (\k -> maybe (pure Nothing) k <<< Slot.lookup il is) cq identity

