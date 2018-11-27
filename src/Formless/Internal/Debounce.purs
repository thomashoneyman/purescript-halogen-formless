module Formless.Internal.Debounce where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (over, unwrap)
import Effect.Aff (Milliseconds, delay, error, forkAff, killFiber)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff)
import Formless.Types.Component (DSL, InternalState(..))
import Formless.Types.Form (FormField)
import Halogen as H
import Renderless.State (getState, modifyState_)

-- | A helper function to debounce actions on the form and form fields. Implemented
-- | to reduce type variables necessary in the `State` type, but will cause
-- | unnecessary renders as implemented. 
-- | 
-- | TODO: Experiment with using a ref to avoid a state update when the debouncer
-- | is being re-triggered.

debounceForm 
  :: forall pq cq cs form m a
   . MonadAff m
  => Milliseconds
  -> DSL pq cq cs form m (form Record FormField)
  -> DSL pq cq cs form m (form Record FormField)
  -> DSL pq cq cs form m a
  -> DSL pq cq cs form m Unit
debounceForm ms pre post last = do
  state <- getState

  let 
    debouncer = (unwrap state.internal).debouncer
    mkFiber v = H.liftAff $ forkAff do
      delay ms
      AVar.put unit v

  case debouncer of
    Nothing -> do
      var <- H.liftAff $ AVar.empty
      fiber <- mkFiber var

      void $ H.fork do 
        void $ H.liftAff (AVar.take var)
        form <- post
        modifyState_ \st -> st
          { internal = over InternalState (_ { debouncer = Nothing }) st.internal
          , form = form 
          }
        last

      form <- pre
      modifyState_ \st -> st
        { internal = over InternalState (_ { debouncer = Just { var, fiber } }) st.internal
        , form = form 
        }

    Just db -> do
      let var = db.var
      void $ H.liftAff $ killFiber (error "") db.fiber
      fiber <- mkFiber var
      modifyState_ \st -> st
        { internal = over InternalState (_ { debouncer = Just { var, fiber } }) st.internal }