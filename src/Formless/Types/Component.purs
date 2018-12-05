module Formless.Types.Component where

import Prelude

import Control.Comonad.Store (Store)
import Data.Const (Const)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Variant (Variant)
import Effect.Aff (Error, Fiber, Milliseconds)
import Effect.Aff.AVar (AVar)
import Effect.Ref (Ref)
import Formless.Types.Form (FormField, InputField, InputFunction, OutputField, U)
import Formless.Validation (Validation)
import Halogen as H
import Halogen.HTML as HH

-- | The component query type. See Formless.Query for helpers related
-- | to constructing and using these queries.
data Query pq cq cs form m a
  = Modify (form Variant InputFunction) a
  | Validate (form Variant U) a
  | ModifyValidate (Maybe Milliseconds) (form Variant InputFunction) a
  | Reset (form Variant InputFunction) a
  | SetAll (form Record InputField) a
  | ModifyAll (form Record InputFunction) a
  | ResetAll a
  | ValidateAll a
  | Submit a
  | SubmitReply (Maybe (form Record OutputField) -> a)
  | GetState (PublicState form -> a)
  | Send cs (cq a)
  | LoadForm (form Record InputField) a
  | SyncFormData a
  | Raise (pq Unit) a
  | Initialize a
  | Receive (Input pq cq cs form m) a
  | AndThen (Query pq cq cs form m Unit) (Query pq cq cs form m Unit) a

-- | The overall component state type, which contains the local state type
-- | and also the render function
type StateStore pq cq cs form m = Store (State form m) (HTML pq cq cs form m)

-- | The component type
type Component pq cq cs form m
  = H.Component
      HH.HTML
      (Query pq cq cs form m)
      (Input pq cq cs form m)
      (Message pq form)
      m

-- | The component's HTML type, the result of the render function.
type HTML pq cq cs form m = H.ParentHTML (Query pq cq cs form m) cq cs m

-- | The component's DSL type, the result of the eval function.
type DSL pq cq cs form m
  = H.ParentDSL
      (StateStore pq cq cs form m)
      (Query pq cq cs form m)
      cq
      cs
      (Message pq form)
      m

-- | The component local state
type State form m = Record (StateRow form (internal :: InternalState form m))

-- | The component's public state
type PublicState form = Record (StateRow form ())

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
  , validationRef :: Maybe (Ref (Maybe (Error -> m Unit)))
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
type Input pq cq cs form m =
  { initialInputs :: form Record InputField
  , validators :: form Record (Validation form m)
  , render :: State form m -> HTML pq cq cs form m
  }

-- | The component tries to require as few messages to be handled as possible. You
-- | can always use the *Reply variants of queries to perform actions and receive
-- | a result out the other end.
data Message pq form
  = Submitted (form Record OutputField)
  | Changed (PublicState form)
  | Emit (pq Unit)

-- | Simple types

-- | A simple query type when you have no child slots in use
type Query' form m = Query (Const Void) (Const Void) Void form m

-- | A simple HTML type when the component does not need embedding
type HTML' form m = H.ParentHTML (Query' form m) (Const Void) Void m

-- | A simple Message type when the component does not need embedding
type Message' form = Message (Const Void) form

-- | A simple Input type when the component does not need embedding
type Input' form m = Input (Const Void) (Const Void) Void form m
