-- | This module exports helpers for working with Formless actions, which you
-- | will use in your render function to attach to appropriate fields. Prefer
-- | these over using data constructors from the Formless action type. You can
-- | also freely extend Formless with more actions of your own using `injAction`.
module Formless.Action where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Time.Duration (Milliseconds)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj)
import Formless.Class.Initial (class Initial, initial)
import Formless.Transform.Record (WrapField, wrapInputFields, wrapInputFunctions)
import Formless.UseFormless (FormlessReturn)
import Formless.Types.Form (InputField, InputFunction, U(..))
import Halogen.Hooks (HookM)
import Heterogeneous.Mapping as HM
import Prim.Row as Row

-- | Set the input value of a form field at the specified label.
-- |
-- | ```purescript
-- | [ HE.onValueInput $ Just <<< F.set _name ]
-- | ```
set
  :: forall form m sym inputs r e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) r inputs
  => (FormlessReturn form m)
  -> SProxy sym
  -> i
  -> HookM m Unit
set formless sym i =
  formless.modify (wrap (inj sym (wrap (const i))))

-- | Modify the input value of a form field at the specified label with the
-- | provided function.
-- |
-- | ```purescript
-- | [ HE.onChange \_ -> Just $ F.modify _enabled not ]
-- | ```
modify
  :: forall form m sym inputs r e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) r inputs
  => FormlessReturn form m
  -> SProxy sym
  -> (i -> i)
  -> HookM m Unit
modify formless sym f =
  formless.modify (wrap (inj sym (wrap f)))

-- | Trigger validation on a form field
-- |
-- | ```purescript
-- | [ HE.onBlur \_ -> Just $ F.validate _name ]
-- | ```
validate
  :: forall form m sym us r e i o
   . IsSymbol sym
  => Newtype (form Variant U) (Variant us)
  => Row.Cons sym (U e i o) r us
  => FormlessReturn form m
  -> SProxy sym
  -> HookM m Unit
validate formless sym =
  formless.validate (wrap (inj sym U))

-- | Set the input value of a form field at the specified label, also triggering
-- | validation to run on the field.
-- |
-- | ```purescript
-- | [ HE.onValueInput $ Just <<< F.setValidate _name ]
-- | ```
setValidate
  :: forall form m sym inputs r e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) r inputs
  => FormlessReturn form m
  -> SProxy sym
  -> i
  -> HookM m Unit
setValidate formless sym i =
  formless.modifyValidate (Tuple Nothing (wrap (inj sym (wrap (const i)))))

-- | Modify the input value of a form field at the specified label, also triggering
-- | validation to run on the field, with the provided function.
-- |
-- | ```purescript
-- | [ HE.onChange \_ -> Just $ F.modifyValidate _enabled not ]
-- | ```
modifyValidate
  :: forall form m sym inputs r e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) r inputs
  => FormlessReturn form m
  -> SProxy sym
  -> (i -> i)
  -> HookM m Unit
modifyValidate formless sym f =
  formless.modifyValidate (Tuple Nothing (wrap (inj sym (wrap f))))

-- | Set the input value of a form field at the specified label, while debouncing
-- | validation so that it only runs after the specified amount of time has elapsed
-- | since the last modification. Useful when you need to avoid expensive validation
-- | but do not want to wait for a blur event to validate.
-- |
-- | ```purescript
-- | [ HE.onValueInput $ Just <<< F.asncSetValidate (Milliseconds 300.0) _name ]
-- | ```
asyncSetValidate
  :: forall form m sym inputs r e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) r inputs
  => FormlessReturn form m
  -> Milliseconds
  -> SProxy sym
  -> i
  -> HookM m Unit
asyncSetValidate formless ms sym i =
  formless.modifyValidate (Tuple (Just ms) (wrap (inj sym (wrap (const i)))))

-- | Modify the input value of a form field at the specified label, while debouncing
-- | validation so that it only runs after the specified amount of time has elapsed
-- | since the last modification. Useful when you need to avoid expensive validation
-- | but do not want to wait for a blur event to validate.
-- |
-- | ```purescript
-- | [ HE.onChange \_ -> Just $ F.asncModifyValidate (Milliseconds 300.0) _enabled not ]
-- | ```
asyncModifyValidate
  :: forall form m sym inputs r e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) r inputs
  => FormlessReturn form m
  -> Milliseconds
  -> SProxy sym
  -> (i -> i)
  -> HookM m Unit
asyncModifyValidate formless ms s f =
  formless.modifyValidate (Tuple (Just ms) (wrap (inj s (wrap f))))

-- | Reset the value of the specified form field to its default value
-- | according to the `Initial` type class.
-- |
-- | ```purescript
-- | [ HE.onClick \_ -> Just $ F.reset _name ]
-- | ```
reset
  :: forall form m sym inputs r e i o
   . IsSymbol sym
  => Initial i
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) r inputs
  => FormlessReturn form m
  -> SProxy sym
  -> HookM m Unit
reset formless sym =
  formless.reset (wrap (inj sym (wrap (const initial))))

-- | Provide a record of input fields to overwrite all current
-- | inputs. Unlike `loadForm`, this does not otherwise reset
-- | the form as if it were new. Similar to calling `set` on every
-- | field in the form. Does not run validation.
-- |
-- | ```purescript
-- | [ HE.onClick \_ -> Just $ F.setAll
-- |     { name: "Default Name"
-- |     , enabled: false
-- |     }
-- | ]
-- | ```
setAll
  :: forall form m is is'
   . Newtype (form Record InputField) { | is' }
  => HM.HMap WrapField { | is } { | is' }
  => FormlessReturn form m
  -> { | is }
  -> HookM m Unit
setAll formless is =
  formless.setAll (Tuple (wrapInputFields is) false)

-- | Provide a record of input functions to modify all current
-- | inputs. Similar to calling `modify` on every field in the form.
-- | Does not run validation.
-- |
-- | ```purescript
-- | [ HE.onClick \_ -> Just $ F.modifyAll
-- |     { name: \str -> "User: " <> str
-- |     , enabled: \bool -> not bool
-- |     }
-- | ]
-- | ```
modifyAll
  :: forall form m ifs' ifs
   . Newtype (form Record InputFunction) { | ifs' }
  => HM.HMap WrapField { | ifs } { | ifs' }
  => FormlessReturn form m
  -> { | ifs }
  -> HookM m Unit
modifyAll formless fs =
  formless.modifyAll (Tuple (wrapInputFunctions fs) false)

-- TODO: Port docs elsewhere and remove this unused function
-- | Validate all fields in the form, collecting errors
-- |
-- | ```purescript
-- | [ HE.onClick \_ -> Just F.validateAll ]
-- | ```
-- validateAll :: forall v. Variant (validateAll :: Unit | v)
-- validateAll =
--   inj (SProxy :: _ "validateAll") unit

-- | Provide a record of inputs to overwrite all current inputs without
-- | resetting the form (as `loadForm` does), and then validate the
-- | entire new set of fields. Similar to calling `setValidate` on every
-- | field in the form.
-- |
-- | ```purescript
-- | [ HE.onClick \_ -> Just $ F.setValidateAll
-- |     { name: "Default Name"
-- |     , enabled: false
-- |     }
-- | ]
-- | ```
setValidateAll
  :: forall form m is' is
   . Newtype (form Record InputField) { | is' }
  => HM.HMap WrapField { | is } { | is' }
  => FormlessReturn form m
  -> { | is }
  -> HookM m Unit
setValidateAll formless is =
  formless.setAll (Tuple (wrapInputFields is) true)

-- | Provide a record of input functions to modify all current
-- | inputs, and then validate all fields.  Similar to calling
-- | `modifyValidate` on every field in the form.
-- |
-- | ```purescript
-- | [ HE.onClick \_ -> Just $ F.modifyValidateAll
-- |     { name: \str -> "User: " <> str
-- |     , enabled: \bool -> not bool
-- |     }
-- | ]
-- | ```
modifyValidateAll
  :: forall form m ifs' ifs
   . Newtype (form Record InputFunction) { | ifs' }
  => HM.HMap WrapField { | ifs } { | ifs' }
  => FormlessReturn form m
  -> { | ifs }
  -> HookM m Unit
modifyValidateAll formless ifs =
  formless.modifyAll (Tuple (wrapInputFunctions ifs) true)

-- TODO: port docs and remove unused function
-- | Reset all fields to their initial values, and reset the form
-- | to its initial pristine state, no touched fields.
-- |
-- | ```purescript
-- | [ HE.onClick \_ -> Just F.resetAll ]
-- | ```
-- resetAll :: forall v. Variant (resetAll :: Unit | v)
-- resetAll =
--   inj (SProxy :: _ "resetAll") unit

-- TODO: port docs and remove unused function
-- | Submit the form, which will trigger a `Submitted` result if the
-- | form validates successfully.
-- |
-- | ```purescript
-- | [ HE.onClick \_ -> Just F.submit ]
-- | ```
-- submit :: forall v. Variant (submit :: Unit | v)
-- submit =
--   inj (SProxy :: _ "submit") unit

-- TODO: port docs and remove unused function
-- | Load a form from a set of existing inputs. Useful for when you need to mount
-- | Formless, perform some other actions like request data from the server, and
-- | then load an existing set of inputs.
-- |
-- | ```purescript
-- | [ HE.onClick \_ -> Just $ F.loadForm $ F.wrapInputFields
-- |     { name: ""
-- |     , enabled: false
-- |     }
-- | ]
-- | ```
-- loadForm
--   :: forall form v
--    . form Record InputField
--   -> Variant (loadForm :: form Record InputField | v)
-- loadForm = inj (SProxy :: _ "loadForm")
