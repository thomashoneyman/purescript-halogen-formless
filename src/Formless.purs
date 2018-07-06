-- | Formless is a renderless component to help you build forms in Halogen.
-- | It expects that you have already written a form spec and validation and
-- | you simply need a component to run it on your behalf.

module Formless where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, store)
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

type State = Unit

type Input pq cq cs m =
  { render :: State -> H.ParentHTML (Query pq cq cs m) cq cs m }

data Message pq
  = Submitted
  | Validated
  | Emit (pq Unit)

component
  :: ∀ pq cq cs m
   . Ord cs
  => MonadAff m
  => H.Component HH.HTML (Query pq cq cs m) (Input pq cq cs m) (Message pq) m
component =
  H.parentComponent
    { initialState
    , render: extract
    , eval
    , receiver: HE.input Receive
    }
  where

  initialState :: Input pq cq cs m -> StateStore pq cq cs m
  initialState { render } = store render unit

  eval
    :: Query pq cq cs m
    ~> H.ParentDSL (StateStore pq cq cs m) (Query pq cq cs m) cq cs (Message pq) m
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

