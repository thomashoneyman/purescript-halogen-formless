module Formless.Spec where

import Prelude

import Control.Alt (class Alt, (<|>))
import Data.Either (Either(..), either)
import Data.Lens (Lens', view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Prism.Either (_Left, _Right)
import Data.Lens.Prism.Maybe (_Just)
import Data.Lens.Record (prop)
import Data.Lens.Traversal (Traversal')
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (class Profunctor)
import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row as Row

-- | @monoidmusician
data FormProxy (form :: (# Type -> Type) -> (Type -> Type -> Type -> Type) -> Type) = FormProxy

-- | A wrapper to represent only the input type.
newtype InputField error input output = InputField input
derive instance newtypeInputField :: Newtype (InputField e i o) _
derive newtype instance eqInputField :: Eq i => Eq (InputField e i o)
derive newtype instance ordInputField :: Ord i => Ord (InputField e i o)

-- | A wrapper to represent only the output type. Used to represent
-- | form results at the end of validation.
newtype OutputField error input output = OutputField output
derive instance newtypeOutputField :: Newtype (OutputField e i o) _

-- | The type that we need to record state across the form, but
-- | we don't need this from the user -- we can fill in 'touched'
-- | and 'result' on their behalf.
newtype FormField m e i o = FormField (Record (FormFieldRow m e i o))
derive instance newtypeFormField :: Newtype (FormField m e i o) _

-- | The row used for the FormField newtype and in lens type signatures
type FormFieldRow m error input output =
  ( input :: input
  , touched :: Boolean
  , result :: Maybe (Either error output)
  , validator :: Maybe (input -> m (Either error output))
  )

-- | Proxies for each of the fields in FormField for easy access
_input = SProxy :: SProxy "input"
_touched = SProxy :: SProxy "touched"
_result = SProxy :: SProxy "result"
_validator = SProxy :: SProxy "validator"


-- | A wrapper to represent the validation function on a form field.
newtype Validator m error input output = Validator (input -> m (Either error output))
derive instance newtypeValidator :: Newtype (Validator m e i o) _
derive instance functorValidator :: Functor m => Functor (Validator m e i)

instance applyValidator :: Monad m => Apply (Validator m e i) where
  apply vf va = Validator \i -> do
    vf' <- unwrap vf i
    va' <- unwrap va i
    pure $ vf' <*> va'

instance applicativeValidator :: Monad m => Applicative (Validator m e i) where
  pure = Validator <<< const <<< pure <<< pure

instance altValidator :: Monad m => Alt (Validator m e i) where
  alt v0 v1 = Validator \i -> do
    v0' <- unwrap v0 i
    v1' <- unwrap v1 i
    pure $ v0' <|> v1'

instance semigroupValidator :: Semigroup (m (Either e o)) => Semigroup (Validator m e i o) where
  append (Validator v0) (Validator v1) = Validator \i -> v0 i <> v1 i

instance monoidValidator
  :: (Applicative m, Monoid (Either e o), Semigroup (m (Either e o)))
  => Monoid (Validator m e i o) where
  mempty = Validator <<< const <<< pure $ mempty

instance semigroupoidValidator :: Monad m => Semigroupoid (Validator m e) where
  compose v1 v0 = Validator \i -> do
    eo <- unwrap v0 i
    either (pure <<< Left) (unwrap v1) eo

instance categoryValidator :: Monad m => Category (Validator m e) where
  identity = Validator $ pure <<< pure

instance profunctorValidator :: Monad m => Profunctor (Validator m e) where
  dimap l r v = (Validator $ l >>> pure >>> pure) >>> v >>> (Validator $ r >>> pure >>> pure)

-- | Used to take a pure i -> o function and turn it into a correct Validator
hoistFn :: ∀ m e i o. Monad m => (i -> o) -> Validator m e i o
hoistFn f = Validator $ f >>> pure >>> pure

-- | Used to take a pure i -> V e o function and turn it into a correct Validator
hoistFnE :: ∀ m e i o. Monad m => (i -> Either e o) -> Validator m e i o
hoistFnE f = Validator $ f >>> pure


----------
-- Get

type FormFieldGet m e i o x =
  forall sym form fields t0
   . IsSymbol sym
  => Newtype (form Record (FormField m)) (Record fields)
  => Row.Cons sym (FormField m e i o) t0 fields
  => SProxy sym
  -> form Record (FormField m)
  -> x

-- | Given a form, get the field at the specified symbol
getField :: ∀ m e i o. FormFieldGet m e i o (Record (FormFieldRow m e i o))
getField sym = view (_Field sym)

-- | Given a form, get the input at the specified symbol
getInput :: ∀ m e i o. FormFieldGet m e i o i
getInput sym = view (_Input sym)

-- | Given a form, get the result at the specified symbol
getResult :: ∀ m e i o. FormFieldGet m e i o (Maybe (Either e o))
getResult sym = view (_Result sym)


----------
-- Lenses

type FormFieldLens m e i o x =
  forall sym form fields t0
   . IsSymbol sym
  => Newtype (form Record (FormField m)) (Record fields)
  => Row.Cons sym (FormField m e i o) t0 fields
  => SProxy sym
  -> Lens' (form Record (FormField m)) x

-- | A lens to operate on the field at a given symbol in your form
_Field :: ∀ m e i o. FormFieldLens m e i o (Record (FormFieldRow m e i o))
_Field sym = _Newtype <<< prop sym <<< _Newtype

-- | A lens to operate on the input at a given symbol in your form
_Input :: ∀ m e i o. FormFieldLens m e i o i
_Input sym = _Field sym <<< prop _input

-- | A lens to operate on the 'touched' field at a given symbol in your form
_Touched :: ∀ m e i o. FormFieldLens m e i o Boolean
_Touched sym = _Field sym <<< prop _touched

-- | A lens to operate on the 'result' field at a given symbol in your form
_Result :: ∀ m e i o. FormFieldLens m e i o (Maybe (Either e o))
_Result sym = _Field sym <<< prop _result

-- | A traversal to operate on the possible error inside the 'result' field at
-- | a given symbol in your form
_Error
  :: ∀ sym form fields t0 m e i o
   . IsSymbol sym
  => Newtype (form Record (FormField m)) (Record fields)
  => Row.Cons sym (FormField m e i o) t0 fields
  => SProxy sym
  -> Traversal' (form Record (FormField m)) e
_Error sym = _Result sym <<< _Just <<< _Left

-- | A traversal to operate on the possible output inside the 'result' field at
-- | a given symbol in your form
_Output
  :: ∀ sym form fields t0 m e i o
   . IsSymbol sym
  => Newtype (form Record (FormField m)) (Record fields)
  => Row.Cons sym (FormField m e i o) t0 fields
  => SProxy sym
  -> Traversal' (form Record (FormField m)) o
_Output sym = _Result sym <<< _Just <<< _Right

----------
-- Helper types

-- | A type synonym that lets you pick out just the error type from
-- | your form row.
type ErrorType error input output = error

-- | A type synonym that lets you pick out just the input type from
-- | your form row.
type InputType error input output = input

-- | A type synonym that lets you pick out just the output type from
-- | your form row.
type OutputType error input output = output
