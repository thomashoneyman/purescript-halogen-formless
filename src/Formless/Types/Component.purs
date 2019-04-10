module Formless.Types.Component where

import Prelude

import Data.Const (Const)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Variant (Variant)
import Effect.Aff (Fiber, Milliseconds)
import Effect.Aff.AVar (AVar)
import Effect.Ref (Ref)
import Formless.Types.Form (FormField, InputField, InputFunction, OutputField, U)
import Formless.Validation (Validation)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.ChildQuery (ChildQueryBox)

-- | The private component action type. To embed a parent query into Formless
-- | via the render function, see `Query` and the helpers in Formless.Query,
-- | specifically `embed`.
data Action form query ps m
  = Initialize
  | Receive (form Record (Validation form m))
  | SyncFormData
  | AsAction (Query form query ps Unit)

-- | The component query type. See Formless.Query for helpers related
-- | to constructing and using these queries.
data Query form query ps a
  = Modify (form Variant InputFunction) a
  | Validate (form Variant U) a
  | ModifyValidate (Maybe Milliseconds) (form Variant InputFunction) a
  | Reset (form Variant InputFunction) a
  | SetAll (form Record InputField) a
  | ModifyAll (form Record InputFunction) a
  | ValidateAll a
  | ResetAll a
  | Submit a
  | SubmitReply (Maybe (form Record OutputField) -> a)
  | GetState (PublicState form -> a)
  | LoadForm (form Record InputField) a
  | AndThen (Query form query ps Unit) (Query form query ps Unit) a
  | SendQuery (ChildQueryBox ps (Maybe a))
  | Embed (query a)

-- | The component type
type Component form st query ps msg m = 
  H.Component HH.HTML (Query form query ps) (Input form st m) (Message form msg) m

-- | The component's HTML type, the result of the render function.
type ComponentHTML form query ps m = 
  H.ComponentHTML (Action form query ps m) ps m

-- | The component's eval type
type HalogenM form st query ps msg m =
  H.HalogenM (State form st m) (Action form query ps m) ps (Message form msg) m

-- | The component local state
type State form st m = 
  {| StateRow form (internal :: InternalState form m | st) }

-- | The component's public state
type PublicState form = 
  {| StateRow form () }

-- | The component's public state
type StateRow form r =
  ( validity :: ValidStatus
  , dirty :: Boolean
  , submitting :: Boolean
  , errors :: Int
  , submitAttempts :: Int
  , form :: form Record FormField
  | r
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

-- | The component's input type
type Input form st m =
  { initialInputs :: form Record InputField
  , validators :: form Record (Validation form m)
  | st
  }

-- | The component tries to require as few messages to be handled as possible. You
-- | can always use the *Reply variants of queries to perform actions and receive
-- | a result out the other end or extend these messages.
data Message form msg
  = Submitted (form Record OutputField)
  | Changed (PublicState form)
  | Raised msg

-- | A slot type that can be used in the ChildSlots definition for your parent
-- | component
type Slot form query ps msg = H.Slot (Query form query ps) (Message form msg)

-- | A simple query type when the component does not need extension
type Query' form = Query form (Const Void) ()

-- | A simple HTML type when the component does not need extension
type HTML' form m = ComponentHTML form (Const Void) () m

-- | A simple Message type when the component does not need extension
type Message' form = Message form Void

-- | A simple Input type when the component does not need extension
type Input' form m = Input form () m

-- | A simple Slot type when the component does not need extension
type Slot' form = H.Slot (Query' form) (Message' form)
