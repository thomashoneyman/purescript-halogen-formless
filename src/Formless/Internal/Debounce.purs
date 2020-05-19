module Formless.Internal.Debounce where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse, traverse_, for_)
import Effect.Aff (Fiber, Milliseconds, delay, error, forkAff, killFiber)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Formless.Types.Component (HalogenM, Debouncer)
import Formless.Types.Form (FormField)
import Halogen (ForkId)
import Halogen as H

-- | A helper function to debounce actions on the form and form fields. Implemented
-- | to reduce type variables necessary in the `State` type

debounceForm
  :: forall form st act ps msg m a
   . MonadAff m
  => Milliseconds
  -> HalogenM form st act ps msg m (form Record FormField)
  -> HalogenM form st act ps msg m (form Record FormField)
  -> HalogenM form st act ps msg m a
  -> HalogenM form st act ps msg m Unit
debounceForm ms pre post last = do
  state <- H.get

  let
    dbRef = (unwrap state.internal).debounceRef
    vdRef = (unwrap state.internal).validationRef

  -- if there is a running validation, cancel it
  readRef vdRef >>= traverse_ H.kill
  debouncer <- H.liftEffect $ map join $ traverse Ref.read dbRef

  case debouncer of
    Nothing -> do
      var <- H.liftAff $ AVar.empty
      fiber <- mkFiber var

      forkId <- processAfterDelay var dbRef

      H.liftEffect $ for_ dbRef $ Ref.write (Just { var, fiber, forkId })
      atomic pre Nothing

    Just db -> do
      let var = db.var
          forkId' = db.forkId
      void $ killFiber' db.fiber
      void $ H.kill forkId'
      fiber <- mkFiber var
      forkId <- processAfterDelay var dbRef 
      H.liftEffect $ for_ dbRef $ Ref.write (Just { var, fiber, forkId })

  where
  mkFiber :: AVar Unit -> HalogenM form st act ps msg m (Fiber Unit)
  mkFiber v = H.liftAff $ forkAff do
    delay ms
    AVar.put unit v

  killFiber' :: forall x n. MonadAff n => Fiber x -> n Unit
  killFiber' = H.liftAff <<< killFiber (error ("time's up!"))

  readRef :: forall x n. MonadAff n => Maybe (Ref (Maybe x)) -> n (Maybe x)
  readRef = H.liftEffect <<< map join <<< traverse Ref.read

  processAfterDelay :: AVar Unit -> (Maybe (Ref (Maybe Debouncer))) -> HalogenM form st act ps msg m ForkId
  processAfterDelay var dbRef = H.fork do
    void $ H.liftAff (AVar.take var)
    H.liftEffect $ traverse_ (Ref.write Nothing) dbRef
    atomic post (Just last)
  
  atomic
    :: forall n
     . MonadAff n
    => HalogenM form st act ps msg n (form Record FormField)
    -> Maybe (HalogenM form st act ps msg n a)
    -> HalogenM form st act ps msg n Unit
  atomic process maybeLast = do
    state <- H.get
    let ref = (unwrap state.internal).validationRef
    mbRef <- readRef ref
    for_ mbRef H.kill
    H.liftEffect $ for_ ref $ Ref.write Nothing
    forkId <- H.fork do
      form <- process
      H.modify_ _ { form = form }
      H.liftEffect $ for_ ref $ Ref.write Nothing
      for_ maybeLast identity
    H.liftEffect $ for_ ref $ Ref.write (Just forkId)
