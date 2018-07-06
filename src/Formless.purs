-- | Formless is a renderless component to help you build forms in Halogen.
-- | It expects that you have already written a form spec and validation and
-- | you simply need a component to run it on your behalf.

module Formless where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, store)
import Data.Either (Either(..))
import Data.Lens as Lens
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy)
import Effect.Aff.Class (class MonadAff)
import Formless.Spec (InputField, _input, _result, _touched, _validator)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Prim.Row (class Cons)
import Record as Record
import Renderless.State (modifyStore_)

data Query pq cq cs form m a
  = HandleBlur (form -> form) a
  | HandleChange (form -> form) a
  | Submit a
  | Raise (pq Unit) a
  | Receive (Input pq cq cs form m) a

-- | The overall component state type, which contains the local state type
-- | and also the render function
type StateStore pq cq cs form m =
  Store State (HTML pq cq cs form m)

-- | The component type
type Component pq cq cs form m
  = H.Component HH.HTML (Query pq cq cs form m) (Input pq cq cs form m) (Message pq) m

-- | The component's HTML type, the result of the render function.
type HTML pq cq cs form m
  = H.ParentHTML (Query pq cq cs form m) cq cs m

-- | The component's DSL type, the result of the eval function.
type DSL pq cq cs form m
  = H.ParentDSL (StateStore pq cq cs form m) (Query pq cq cs form m) cq cs (Message pq) m

-- | The component local state
type State =
  { isValid :: Boolean
  , formResult ::
      Maybe
        { name :: String
        , email :: String
        }
  , form ::
      { name ::
          { input :: String
          , touched :: Boolean
          , validator :: String -> Either String String
          , result :: Maybe (Either String String)
          }
      , email ::
         { input :: String
         , touched :: Boolean
         , validator :: String -> Either String String
         , result :: Maybe (Either String String)
         }
      }
  }

-- | The component's input type
type Input pq cq cs form m =
  { render :: State -> H.ParentHTML (Query pq cq cs form m) cq cs m }

data Message pq
  = Submitted
  | Emit (pq Unit)

-- | The component itself
component :: ∀ pq cq cs form m. Ord cs => MonadAff m => Component pq cq cs form m
component =
  H.parentComponent
    { initialState
    , render: extract
    , eval
    , receiver: HE.input Receive
    }
  where

  initialState :: Input pq cq cs form m -> StateStore pq cq cs form m
  initialState { render } = store render $
    { isValid: false
    , formResult: Nothing
    , form:
        { name:
            { input: ""
            , touched: false
            , validator: const (Left "Error checking not implemented.")
            , result: Nothing
            }
        , email:
            { input: ""
            , touched: false
            , validator: const (Left "Error checking not implemented here either.")
            , result: Nothing
            }
        }
    }

  eval :: Query pq cq cs form m ~> DSL pq cq cs form m
  eval = case _ of
    HandleBlur fs a -> do
      --  modifyState_ \st -> st { form = fs st.form }
      pure a

    HandleChange fs a -> do
      --  modifyState_ fs
      pure a

    Submit a -> do
      H.raise Submitted
      pure a

    Raise query a -> do
      H.raise (Emit query)
      pure a

    Receive { render } a -> do
      modifyStore_ render (\s -> s)
      pure a

---------
-- Helpers

-- | Given a proxy symbol, will trigger validation on that field using
-- | its validator and current input
handleBlur
  :: ∀ sym form inp err out r
   . IsSymbol sym
  => Cons sym (InputField inp err out) r form
  => SProxy sym
  -> Record form
  -> Record form
handleBlur sym form = newForm form
  where
    input = Record.get _input $ Record.get sym form
    validator = Record.get _validator $ Record.get sym form
    setResult = Lens.set (prop sym <<< prop _result)
    setTouched = Lens.set (prop sym <<< prop _touched)
    newForm = setResult (Just $ validator input) >>> setTouched true

-- | Replace the value at a given field with a new value of the correct type.
handleChange
  :: ∀ sym form inp err out r
   . IsSymbol sym
  => Cons sym (InputField inp err out) r form
  => SProxy sym
  -> inp
  -> Record form
  -> Record form
handleChange sym val = setInput val <<< setTouched true
  where
    setInput = Lens.set (prop sym <<< prop _input)
    setTouched = Lens.set (prop sym <<< prop _touched)
