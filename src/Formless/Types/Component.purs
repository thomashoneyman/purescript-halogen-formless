module Formless.Types.Component where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple)
import Data.Variant (Variant)
import Effect.Aff (Fiber, Milliseconds)
import Effect.Aff.AVar (AVar)
import Effect.Ref (Ref)
import Formless.Types.Form (FormField, InputField, InputFunction, OutputField, U)
import Formless.Validation (Validation)
import Halogen as H
import Halogen.Hooks (Hook, UseState, useState)
import Halogen.Hooks as Hooks
import Halogen.Query.HalogenM (ForkId)
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
  ( initialize :: Maybe act
  , syncFormData :: Unit
  | r
  )

-- | The component local state
type State form st m =
  { | StateRow form (internal :: InternalState form m | st) }

-- | A simple state type when the component does not need extension
type State' form m =
  State form () m

-- | The component's public state
type PublicState form st =
  { | StateRow form st }

-- | The component's public state, as an extensible row
type StateRow form st =
  ( validity :: ValidStatus
  , dirty :: Boolean
  , submitting :: Boolean
  , errors :: Int
  , submitAttempts :: Int
  , form :: form Record FormField
  | st
  )

-- | A newtype to make easier type errors for end users to
-- | read by hiding internal fields
newtype InternalState form m = InternalState
  { initialInputs :: form Record InputField
  , validators :: form Record (Validation form m)
  , allTouched :: Boolean
  , debounceRef :: Maybe (Ref (Maybe Debouncer))
  , validationRef :: Maybe (Ref (Maybe H.ForkId))
  }

derive instance newtypeInternalState :: Newtype (InternalState form m) _

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

-- | The component's input type. If you provide `Nothing` as your `initialInputs`
-- | then the form will fill in values based on the `Initial` type class for the
-- | field's input type. Otherwise, the form will contain the values you provide.
-- |
-- | Validators can be created using the Formless.Validation module.
type Input form st m =
  { initialInputs :: Maybe (form Record InputField)
  , validators :: form Record (Validation form m)
  | st
  }

-- | The component tries to require as few messages to be handled as possible. You
-- | can always use the *Reply variants of queries to perform actions and receive
-- | a result out the other end, or extend these messages.
data Event form st
  = Submitted (form Record OutputField)
  | Changed (PublicState form st)

type Event' form = Event form ()

-- | A convenience export of formless as a symbol for use when mounting Formless
-- | as a child component
-- |
-- | ```purescript
-- | type ChildSlots = (formless :: F.Slot' Form FormResult)
-- | HH.slot F._formless unit (F.component spec) input handler
-- | ```
_formless = SProxy :: SProxy "formless"

type FormlessInput form m = Input form () m

type FormlessState form = { | StateRow form () }

newtype UseFormless hooks = UseFormless (UseState Unit hooks)

derive instance newtypeUseFormless :: Newtype (UseFormless hooks) _

useFormless
  :: forall form m
   . Monad m
  => FormlessInput form m
  -> Hook m UseFormless Unit
useFormless inputRec = Hooks.wrap Hooks.do
  _ <- useState unit
  -- state /\ stateId <- useState {- FormlessState args -}
  -- internal /\ internalId <- useState {- InternalState arg -}

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
