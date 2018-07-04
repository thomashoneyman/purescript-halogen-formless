module Formless.State where

import Prelude

import Control.Monad.State.Class (class MonadState)
import Control.Monad.State (get, modify, modify_, put)
import Control.Comonad.Store (Store, runStore, seeks, store)
import Data.Tuple (fst, snd)

-- | We are working within the `State` monad as is always the case
-- | in Halogen. However, the state type we really want to access
-- | is within a further layer, `Store`. This function works as a
-- | drop-in for `H.get` to let you access your state record and
-- | use it as you ordinarily would.
-- |
-- | ```purescript
-- | st <- getState
-- | ```
getState :: ∀ m s a. MonadState (Store s a) m => m s
getState = map snd <<< pure <<< runStore =<< get

-- | You can also retrieve the render function, if you need to.
-- |
-- | ```purescript
-- | render <- getRender
-- | ```
getRender :: ∀ m s a. MonadState (Store s a) m => m (s -> a)
getRender = map fst <<< pure <<< runStore =<< get

-- | When you are modifying the state type, you need to again get into
-- | `Store` and apply the function there. We can do this with `seeks`
-- | from the module. You could use this directly, or write helpers to
-- | make it feel more natural.
-- |
-- | ```purescript
-- | modifyState_ \st -> st { field = newValue }
-- | ```
modifyState :: ∀ m s a. MonadState (Store s a) m => (s -> s) -> m (Store s a)
modifyState = modify <<< seeks

modifyState_ :: ∀ m s a. MonadState (Store s a) m => (s -> s) -> m Unit
modifyState_ = modify_ <<< seeks

putState :: ∀ m s a. MonadState (Store s a) m => s -> m Unit
putState s = put <<< flip store s =<< getRender

-- | In rare cases you will actually want to update the
-- | render function in `Store`. For those cases, you can use this
-- | convenient function along with `H.modify` to easily perform
-- | this kind of wholesale update:
-- |
-- | ```purescript
-- | H.modify $ updateStore (\st -> html) (\st -> st)
-- | ```
-- |
-- | You almost never need to use this except in your `Receiver`
-- | query, where it is necessary to update the render function
-- | with the new one passed via `Input`.
updateStore :: ∀ state html
  . (state -> html)
 -> (state -> state)
 -> Store state html
 -> Store state html
updateStore r f
  = store r <<< snd <<< runStore <<< seeks f

-- | You could also use these helper functions directly
-- |
-- | ```purescript
-- | newStore <- modifyStore render stateTransform
-- | putStore render state
-- | ```
modifyStore :: ∀ m s a. MonadState (Store s a) m => (s -> a) -> (s -> s) -> m (Store s a)
modifyStore r f = modify (updateStore r f)

modifyStore_ :: ∀ m s a. MonadState (Store s a) m => (s -> a) -> (s -> s) -> m Unit
modifyStore_ r f = modify_ (updateStore r f)

putStore :: ∀ m s a. MonadState (Store s a) m => (s -> a) -> s -> m Unit
putStore r s = put (store r s)
