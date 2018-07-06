-- | Formless is a renderless component to help you build forms in Halogen.
-- | It expects that you have already written a form spec and validation and
-- | you simply need a component to run it on your behalf.

module Formless where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, store)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Renderless.State (modifyStore_)

data Query pq cq cs m a
  = HandleBlur (State -> State) a
  | HandleChange (State -> State) a
  | Submit a
  | Raise (pq Unit) a
  | Receive (Input pq cq cs m) a

type StateStore pq cq cs m =
  Store State (H.ParentHTML (Query pq cq cs m) cq cs m)

-- | The component type
type Component pq cq cs m
  = H.Component HH.HTML (Query pq cq cs m) (Input pq cq cs m) (Message pq) m

-- | The component's HTML type, the result of the render function.
type HTML pq cq cs m
  = H.ParentHTML (Query pq cq cs m) cq cs m

-- | The component's DSL type, the result of the eval function.
type DSL pq cq cs m
  = H.ParentDSL (StateStore pq cq cs m) (Query pq cq cs m) cq cs (Message pq) m

-- | The component's internal state type, which manages form values
type State =
  { isValid :: Boolean
  , spec ::
      { inputs :: -- Raw form inputs on the DOM
          { name :: String
          , email :: String
          }
      , touched :: -- The value has been changed from its original
          { name :: Boolean
          , email :: Boolean
          }
      , results :: -- The result of validation on this field
          { name :: Maybe (Either (Array String) String)
          , email :: Maybe (Either (Array String) String)
          }
      , result :: -- The end result of validation on the entire form
          Maybe
            { name :: String
            , email :: String
            }
      }
  }

-- | The component's input type
type Input pq cq cs m =
  { render :: State -> H.ParentHTML (Query pq cq cs m) cq cs m }

data Message pq
  = Submitted
  | Emit (pq Unit)

-- | The component itself
component :: ∀ pq cq cs m. Ord cs => MonadAff m => Component pq cq cs m
component =
  H.parentComponent
    { initialState
    , render: extract
    , eval
    , receiver: HE.input Receive
    }
  where

  initialState :: Input pq cq cs m -> StateStore pq cq cs m
  initialState { render } = store render $
    { isValid: false
    , spec:
        { inputs:
            { name: ""
            , email: ""
            }
        , touched:
            { name: false
            , email: false
            }
        , results:
            { name: Nothing
            , email: Nothing
            }
        , result: Nothing
        }
    }

  eval :: Query pq cq cs m ~> DSL pq cq cs m
  eval = case _ of
    HandleBlur fs a -> pure a

    HandleChange fs a -> pure a

    Submit a -> pure a

    Raise query a -> do
      H.raise (Emit query)
      pure a

    Receive { render } a -> do
      modifyStore_ render (\s -> s)
      pure a

