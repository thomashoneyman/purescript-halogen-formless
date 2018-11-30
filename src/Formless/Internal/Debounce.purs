module Formless.Internal.Debounce where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse, traverse_)
import Effect.Aff (Milliseconds, delay, error, forkAff, killFiber)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Formless.Types.Component (DSL, Debouncer)
import Formless.Types.Form (FormField)
import Halogen as H
import Renderless.State (getState, modifyState_)

-- | A helper function to debounce actions on the form and form fields. Implemented
-- | to reduce type variables necessary in the `State` type

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
    ref = (unwrap state.internal).debounceRef
    mkFiber v = H.liftAff $ forkAff do
      delay ms
      AVar.put unit v

  debouncer :: Maybe Debouncer <- H.liftEffect $ map join $ traverse Ref.read ref

  case debouncer of
    Nothing -> do
      var <- H.liftAff $ AVar.empty
      fiber <- mkFiber var

      void $ H.fork do 
        _ <- H.liftAff (AVar.take var)
        H.liftEffect $ traverse_ (Ref.write Nothing) ref
        form <- post
        modifyState_ _ { form = form }
        last

      H.liftEffect $ traverse_ (Ref.write $ Just { var, fiber }) ref
      form <- pre
      modifyState_ _ { form = form }
      pure unit

    Just db -> do
      let var = db.var
      _ <- H.liftAff $ killFiber (error "time's up!") db.fiber
      fiber <- mkFiber var
      H.liftEffect $ traverse_ (Ref.write $ Just { var, fiber }) ref