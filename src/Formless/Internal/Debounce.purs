module Formless.Internal.Debounce where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Traversable (for_, traverse_)
import Effect.Aff (Fiber, Milliseconds, delay, error, forkAff, killFiber)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Formless.Types.Component (Debouncer, FormlessState)
import Formless.Types.Form (FormField)
import Halogen (ForkId, liftEffect)
import Halogen as H
import Halogen.Hooks (HookM, StateId)
import Halogen.Hooks as Hooks

-- | A helper function to debounce actions on the form and form fields. Implemented
-- | to reduce type variables necessary in the `State` type

debounceForm
  :: forall form m a
   . MonadAff m
  => Ref { debouncer :: Maybe Debouncer, validation :: Maybe H.ForkId }
  -> StateId (FormlessState form)
  -> Milliseconds
  -> HookM m (form Record FormField)
  -> HookM m (form Record FormField)
  -> HookM m a
  -> HookM m Unit
debounceForm internalRef publicId ms pre post last = do
  { debouncer, validation } <- liftEffect $ Ref.read internalRef
  -- if there is a running validation, cancel it
  traverse_ Hooks.kill validation

  case debouncer of
    Nothing -> do
      var <- liftAff $ AVar.empty
      fiber <- mkFiber var

      forkId <- processAfterDelay var

      liftEffect $ Ref.modify_ (_ { debouncer = Just { var, fiber, forkId }}) internalRef
      atomic pre Nothing

    Just db -> do
      let var = db.var
          forkId' = db.forkId
      void $ killFiber' db.fiber
      void $ Hooks.kill forkId'
      fiber <- mkFiber var
      forkId <- processAfterDelay var
      liftEffect $ Ref.modify_ (_ { debouncer = Just { var, fiber, forkId }}) internalRef

  where
  mkFiber :: AVar Unit -> HookM m (Fiber Unit)
  mkFiber v = H.liftAff $ forkAff do
    delay ms
    AVar.put unit v

  killFiber' :: forall x n. MonadAff n => Fiber x -> n Unit
  killFiber' = H.liftAff <<< killFiber (error ("time's up!"))

  processAfterDelay :: AVar Unit -> HookM m ForkId
  processAfterDelay var = Hooks.fork do
    void $ liftAff (AVar.take var)
    liftEffect $ Ref.modify_ (_ { debouncer = Nothing }) internalRef
    atomic post (Just last)

  atomic
    :: forall n
     . MonadAff n
    => HookM n (form Record FormField)
    -> Maybe (HookM n a)
    -> HookM n Unit
  atomic process maybeLast = do
    { validation } <- liftEffect $ Ref.read internalRef
    for_ validation Hooks.kill
    liftEffect $ Ref.modify_ (_ { validation = Nothing }) internalRef
    forkId <- Hooks.fork do
      form <- process
      Hooks.modify_ publicId _ { form = form }
      liftEffect $ Ref.modify_ (_ { validation = Nothing }) internalRef
      for_ maybeLast identity
    liftEffect $ Ref.modify_ (_ { validation = Just forkId }) internalRef
