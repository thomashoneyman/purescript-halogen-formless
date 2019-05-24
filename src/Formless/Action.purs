-- | This module exports helpers for working with Formless actions, which you
-- | will use in your render function to attach to appropriate fields. Prefer
-- | these over using data constructors from the Formless action type. You can
-- | also freely extend Formless with more actions of your own using `injAction`.
module Formless.Action where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Time.Duration (Milliseconds)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj)
import Formless.Class.Initial (class Initial, initial)
import Formless.Transform.Record (WrapField, wrapInputFields, wrapInputFunctions)
import Formless.Types.Component (Action)
import Formless.Types.Form (InputField, InputFunction, U(..))
import Heterogeneous.Mapping as HM
import Prim.Row as Row

-- | Inject your own action into the Formless component so it can be used in HTML
injAction :: forall form act. act -> Action form act
injAction = inj (SProxy :: _ "userAction")

-- | Set the input value of a form field at the specified label.
-- |
-- | ```purescript
-- | [ HE.onValueInput $ Just <<< F.set _name ]
-- | ```
set
  :: forall form v sym inputs r e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) r inputs
  => SProxy sym
  -> i
  -> Variant (modify :: form Variant InputFunction | v)
set sym i =
  inj (SProxy :: _ "modify") (wrap (inj sym (wrap (const i))))

-- | Modify the input value of a form field at the specified label with the
-- | provided function.
-- |
-- | ```purescript
-- | [ HE.onChange \_ -> Just $ F.modify _enabled not ]
-- | ```
modify
  :: forall form v sym inputs r e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) r inputs
  => SProxy sym
  -> (i -> i)
  -> Variant (modify :: form Variant InputFunction | v)
modify sym f =
  inj (SProxy :: _ "modify") (wrap (inj sym (wrap f)))

-- | Trigger validation on a form field
-- |
-- | ```purescript
-- | [ HE.onBlur \_ -> Just $ F.validate _name ]
-- | ```
validate
  :: forall form v sym us r e i o
   . IsSymbol sym
  => Newtype (form Variant U) (Variant us)
  => Row.Cons sym (U e i o) r us
  => SProxy sym
  -> Variant (validate :: form Variant U | v)
validate sym =
  inj (SProxy :: _ "validate") (wrap (inj sym U))

-- | Set the input value of a form field at the specified label, also triggering
-- | validation to run on the field.
-- |
-- | ```purescript
-- | [ HE.onValueInput $ Just <<< F.setValidate _name ]
-- | ```
setValidate
  :: forall form v sym inputs r e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) r inputs
  => SProxy sym
  -> i
  -> Variant (modifyValidate :: Tuple (Maybe Milliseconds) (form Variant InputFunction) | v)
setValidate sym i =
  inj (SProxy :: _ "modifyValidate") (Tuple Nothing (wrap (inj sym (wrap (const i)))))

-- | Modify the input value of a form field at the specified label, also triggering
-- | validation to run on the field, with the provided function.
-- |
-- | ```purescript
-- | [ HE.onChange \_ -> Just $ F.modifyValidate _enabled not ]
-- | ```
modifyValidate
  :: forall form v sym inputs r e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) r inputs
  => SProxy sym
  -> (i -> i)
  -> Variant (modifyValidate :: Tuple (Maybe Milliseconds) (form Variant InputFunction) | v)
modifyValidate sym f =
  inj (SProxy :: _ "modifyValidate") (Tuple Nothing (wrap (inj sym (wrap f))))

-- | Set the input value of a form field at the specified label, while debouncing
-- | validation so that it only runs after the specified amount of time has elapsed
-- | since the last modification. Useful when you need to avoid expensive validation
-- | but do not want to wait for a blur event to validate.
-- |
-- | ```purescript
-- | [ HE.onValueInput $ Just <<< F.asncSetValidate (Milliseconds 300.0) _name ]
-- | ```
asyncSetValidate
  :: forall form v sym inputs r e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) r inputs
  => Milliseconds
  -> SProxy sym
  -> i
  -> Variant (modifyValidate :: Tuple (Maybe Milliseconds) (form Variant InputFunction) | v)
asyncSetValidate ms sym i =
  inj (SProxy :: _ "modifyValidate") (Tuple (Just ms) (wrap (inj sym (wrap (const i)))))

-- | Modify the input value of a form field at the specified label, while debouncing
-- | validation so that it only runs after the specified amount of time has elapsed
-- | since the last modification. Useful when you need to avoid expensive validation
-- | but do not want to wait for a blur event to validate.
-- |
-- | ```purescript
-- | [ HE.onChange \_ -> Just $ F.asncModifyValidate (Milliseconds 300.0) _enabled not ]
-- | ```
asyncModifyValidate
  :: forall form v sym inputs r e i o
   . IsSymbol sym
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) r inputs
  => Milliseconds
  -> SProxy sym
  -> (i -> i)
  -> Variant (modifyValidate :: Tuple (Maybe Milliseconds) (form Variant InputFunction) | v)
asyncModifyValidate ms s f =
  inj (SProxy :: _ "modifyValidate") (Tuple (Just ms) (wrap (inj s (wrap f))))

-- | Reset the value of the specified form field to its default value
-- | according to the `Initial` type class.
-- |
-- | ```purescript
-- | [ HE.onClick \_ -> Just $ F.reset _name ]
-- | ```
reset
  :: forall form v sym inputs r e i o
   . IsSymbol sym
  => Initial i
  => Newtype (form Variant InputFunction) (Variant inputs)
  => Row.Cons sym (InputFunction e i o) r inputs
  => SProxy sym
  -> Variant (reset :: form Variant InputFunction | v)
reset sym =
  inj (SProxy :: _ "reset") (wrap (inj sym (wrap (const initial))))

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
  :: forall form v is is'
   . Newtype (form Record InputField) { | is' }
  => HM.HMap WrapField { | is } { | is' }
  => { | is }
  -> Variant (setAll :: Tuple (form Record InputField) Boolean | v)
setAll is =
  inj (SProxy :: _ "setAll") (Tuple (wrapInputFields is) false)

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
  :: forall form v ifs' ifs
   . Newtype (form Record InputFunction) { | ifs' }
  => HM.HMap WrapField { | ifs } { | ifs' }
  => { | ifs }
  -> Variant (modifyAll :: Tuple (form Record InputFunction) Boolean | v)
modifyAll fs =
  inj (SProxy :: _ "modifyAll") (Tuple (wrapInputFunctions fs) false)

-- | Validate all fields in the form, collecting errors
-- |
-- | ```purescript
-- | [ HE.onClick \_ -> Just F.validateAll ]
-- | ```
validateAll :: forall v. Variant (validateAll :: Unit | v)
validateAll =
  inj (SProxy :: _ "validateAll") unit

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
  :: forall form v is' is
   . Newtype (form Record InputField) { | is' }
  => HM.HMap WrapField { | is } { | is' }
  => { | is }
  -> Variant (setAll :: Tuple (form Record InputField) Boolean | v)
setValidateAll is =
  inj (SProxy :: _ "setAll") (Tuple (wrapInputFields is) true)

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
  :: forall form v ifs' ifs
   . Newtype (form Record InputFunction) { | ifs' }
  => HM.HMap WrapField { | ifs } { | ifs' }
  => { | ifs }
  -> Variant (modifyAll :: Tuple (form Record InputFunction) Boolean | v)
modifyValidateAll ifs =
  inj (SProxy :: _ "modifyAll") (Tuple (wrapInputFunctions ifs) true)

-- | Reset all fields to their initial values, and reset the form
-- | to its initial pristine state, no touched fields.
-- |
-- | ```purescript
-- | [ HE.onClick \_ -> Just F.resetAll ]
-- | ```
resetAll :: forall v. Variant (resetAll :: Unit | v)
resetAll =
  inj (SProxy :: _ "resetAll") unit

-- | Submit the form, which will trigger a `Submitted` result if the
-- | form validates successfully.
-- |
-- | ```purescript
-- | [ HE.onClick \_ -> Just F.submit ]
-- | ```
submit :: forall v. Variant (submit :: Unit | v)
submit =
  inj (SProxy :: _ "submit") unit

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
loadForm
  :: forall form v
   . form Record InputField
  -> Variant (loadForm :: form Record InputField | v)
loadForm = inj (SProxy :: _ "loadForm")
