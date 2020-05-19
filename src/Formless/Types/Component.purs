module Formless.Types.Component where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant)
import Effect.Aff (Fiber, Milliseconds)
import Effect.Aff.AVar (AVar)
import Formless.Internal.Transform as IT
import Formless.Transform.Row (class MakeInputFieldsFromRow, mkInputFields)
import Formless.Types.Form (FormField, FormProxy(..), InputField, InputFunction, OutputField, U)
import Formless.Validation (Validation)
import Halogen as H
import Halogen.Hooks (Hook, UseRef, UseState, useRef, useState)
import Halogen.Hooks as Hooks
import Halogen.Query.HalogenM (ForkId)
import Prim.RowList as RL
import Type.Row (type (+))

-- | The component action type. While actions are typically considered
-- | internal to a component, in Formless you write the render function and will
-- | need to be able to use these directly. Many of these are shared with queries
-- | of the same name so they can be used either as queries or as actions. See
-- | `Formless.Action` and `Formless.Query`.
-- |
-- | You can freely extend this type with your own actions using `injAction`.
type Action form act = Variant
  ( userAction :: act
  | InternalAction act
  + PublicAction form
  )

type PublicAction form =
  ( modify :: form Variant InputFunction
  , validate :: form Variant U
  , modifyValidate :: Tuple (Maybe Milliseconds) (form Variant InputFunction)
  , reset :: form Variant InputFunction
  , setAll :: Tuple (form Record InputField) Boolean
  , modifyAll :: Tuple (form Record InputFunction) Boolean
  , validateAll :: Unit
  , resetAll :: Unit
  , submit :: Unit
  , loadForm :: form Record InputField
  )

type InternalAction act r =
  ( syncFormData :: Unit
  | r
  )

-- | A type to represent a running debouncer
type Debouncer =
  { var :: AVar Unit
  , fiber :: Fiber Unit
  , forkId :: ForkId
  }

-- | A type to represent validation status
data ValidStatus
  = Invalid
  | Incomplete
  | Valid

derive instance genericValidStatus :: Generic ValidStatus _
derive instance eqValidStatus :: Eq ValidStatus
derive instance ordValidStatus :: Ord ValidStatus

instance showValidStatus :: Show ValidStatus where
  show = genericShow

-- | The component tries to require as few messages to be handled as possible. You
-- | can always use the *Reply variants of queries to perform actions and receive
-- | a result out the other end, or extend these messages.
data Event form st
  = Submitted (form Record OutputField)
  | Changed (FormlessState form)

-- | A convenience export of formless as a symbol for use when mounting Formless
-- | as a child component
-- |
-- | ```purescript
-- | type ChildSlots = (formless :: F.Slot' Form FormResult)
-- | HH.slot F._formless unit (F.component spec) input handler
-- | ```
_formless = SProxy :: SProxy "formless"

-- | The hook's input type. If you provide `Nothing` as your `initialInputs`,
-- | then the form will fill in values based on the `Initial` type class for the
-- | field's input type. Otherwise, the form will contain the values you provide.
-- |
-- | Validators can be created using the Formless.Validation module.
type FormlessInput form m =
  { initialInputs :: Maybe (form Record InputField)
  , validators :: form Record (Validation form m)
  }

type FormlessState form =
  { validity :: ValidStatus
  , dirty :: Boolean
  , submitting :: Boolean
  , errors :: Int
  , submitAttempts :: Int
  , form :: form Record FormField
  }

type InternalState' =
  { allTouched :: Boolean -- I decided to just use Boolean at this point...
  -- , validationRef - use ref; outer Maybe only used for initial Ref value
  --                   due to NOT using `unsafePerformEffect $ liftEffect Ref.new`
  -- initialInputs is in-scope via FormlessInput
  -- validators is in-scope via FormlessInput
  -- debounceRef can be reimplemented via useDebouncer
  }

newtype UseFormless form hooks = UseFormless
  (UseRef (Maybe H.ForkId)
  (UseState Boolean -- was InternalState'
  (UseState (FormlessState form)
  hooks)))

derive instance newtypeUseFormless :: Newtype (UseFormless form hooks) _

useFormless
  :: forall form m is fs ixs
   . Monad m
  => Newtype (form Record InputField) { | is }
  => Newtype (form Record FormField) { | fs }

  => MakeInputFieldsFromRow ixs is is
  => IT.InputFieldsToFormFields ixs is fs
  => RL.RowToList is ixs
  => RL.RowToList fs ixs
  => FormlessInput form m
  -> Hook m (UseFormless form) Unit
useFormless inputRec =
  let
    initialInputs :: form Record InputField
    initialInputs = case inputRec.initialInputs of
      Nothing -> mkInputFields (FormProxy :: FormProxy form)
      Just inputs -> inputs

    initialForm :: form Record FormField
    initialForm = IT.inputFieldsToFormFields initialInputs

  in Hooks.wrap Hooks.do
    public /\ publicId <- useState
      { validity: Incomplete
      , dirty: false
      , errors: 0
      , submitAttempts: 0
      , submitting: false
      , form: IT.inputFieldsToFormFields initialInputs
      }
    allTouched /\ allTouchedId <- useState false
    _ /\ validationRef <- useRef Nothing

    Hooks.pure unit
  -- where
  --   modify :: form Variant InputFunction
  --   modify
  --
  --   validate :: form Variant U
  --   validate
  --
  --   modifyValidate :: Tuple (Maybe Milliseconds) (form Variant InputFunction)
  --   modifyValidate
  --
  --   reset :: form Variant InputFunction
  --   reset
  --
  --   setAll :: Tuple (form Record InputField) Boolean
  --   setAll
  --
  --   modifyAll :: Tuple (form Record InputFunction) Boolean
  --   modifyAll
  --
  --   validateAll :: Unit
  --   validateAll
  --
  --   resetAll :: Unit
  --   resetAll
  --
  --   submit :: Unit
  --   submit
  --
  --   loadForm :: form Record InputField
  --   loadForm
