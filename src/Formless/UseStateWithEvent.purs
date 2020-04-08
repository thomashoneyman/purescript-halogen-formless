module Formless.UseStateWithEvent
  ( useStateWithEvent
  , UseStateWithEvent
  )
  where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Halogen.Hooks (Hook, HookM, MemoValues, StateToken, UseEffect, UseState, useState)
import Halogen.Hooks as Hooks

newtype UseStateWithEvent a hooks = UseStateWithEvent (UseState a hooks)

derive instance newtypeUseStateWithEvent :: Newtype (UseStateWithEvent a hooks) _

-- | Authors of hooks should use `push` to push events into the handler.
-- | They don't return `push` in their custom hook, but instead return
-- | `props`. The end-user will use `props` as an argument to `subscribeTo`.
type UseStateWithEventApi slots output m a =
  { state :: a
  , token :: StateToken a
  , props :: UseStateWithEventProps slots output m a
  }

type UseStateWithEventProps slots output m a =
  { capturesWith ::
       ( { state :: a } -> { state :: a } -> Boolean)
      -> ( MemoValues
        -> HookM slots output m (Maybe (HookM slots output m Unit))
        -> Hook slots output m UseEffect Unit
         )
      -> HookM slots output m (Maybe (HookM slots output m Unit))
      -> Hook slots output m UseEffect Unit
  , subscribe :: (a -> HookM slots output m Unit) -> HookM slots output m Unit
  }

-- | Combines `useState` and `useEvent` together. Every time the state value
-- | is changed, its value will be emitted.
useStateWithEvent
  :: forall output m slots a
   . a
  -> Hook slots output m (UseStateWithEvent a) (UseStateWithEventApi slots output m a)
useStateWithEvent initialState = Hooks.wrap Hooks.do
  state /\ token <- useState initialState

  Hooks.pure { state
             , token
             , props: { capturesWith: \eqFn -> Hooks.capturesWith eqFn { state }
                      , subscribe: \cb -> do
                          state' <- Hooks.get token
                          cb state'
                      }
             }
