-- | Formless is a renderless component to help you build forms in Halogen.
-- | It expects that you have already written a form spec and validation and
-- | you simply need a component to run it on your behalf.

module Formless where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, store)
import Data.Const (Const)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Renderless.State (modifyStore_)

data Query o m a
  = Raise (o Unit) a
  | Receive (Input o m) a

type StateStore o m =
  Store State (H.ParentHTML (Query o m) ChildQuery ChildSlot m)

type State = Unit

type Input o m =
  { render :: State -> H.ParentHTML (Query o m) ChildQuery ChildSlot m }

data Message o
  = Emit (o Unit)

type ChildQuery
  = Const Void

type ChildSlot
  = Unit

component :: ∀ o m. MonadAff m => H.Component HH.HTML (Query o m) (Input o m) (Message o) m
component =
  H.parentComponent
    { initialState
    , render: extract
    , eval
    , receiver: HE.input Receive
    }
  where

  initialState :: Input o m -> StateStore o m
  initialState { render } = store render unit

  eval
    :: Query o m
    ~> H.ParentDSL (StateStore o m) (Query o m) ChildQuery ChildSlot (Message o) m
  eval = case _ of
    Raise query a -> do
      H.raise (Emit query)
      pure a

    Receive { render } a -> do
      modifyStore_ render (\s -> s)
      pure a

