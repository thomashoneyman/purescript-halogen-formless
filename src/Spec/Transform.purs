module Formless.Spec.Transform where

import Prelude

import Data.Either (Either)
import Data.Lens (ALens', set, view)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Formless.Class.Initial (class Initial, initial)
import Formless.Internal as Internal
import Formless.Spec (FormSpec(..), InputField, InputFieldRow, OutputField, _Field, _Input, _Result, _Touched)
import Prim.Row as Row
import Prim.RowList as RL
import Record.Builder as Builder
import Type.Row (RLProxy(..), RProxy(..))

getInput
  :: ∀ sym form t0 fields e i o
   . IsSymbol sym
  => Newtype (form InputField) (Record fields)
  => Row.Cons sym (InputField e i o) t0 fields
  => SProxy sym
  -> form InputField
  -> i
getInput sym = view (_Input sym)

getResult
  :: ∀ sym form t0 fields e i o
   . IsSymbol sym
  => Newtype (form InputField) (Record fields)
  => Row.Cons sym (InputField e i o) t0 fields
  => SProxy sym
  -> form InputField
  -> Maybe (Either e o)
getResult sym = view (_Result sym)

setInput
  :: ∀ sym form t0 fields e i o
   . IsSymbol sym
  => Newtype (form InputField) (Record fields)
  => Row.Cons sym (InputField e i o) t0 fields
  => SProxy sym
  -> i
  -> form InputField
  -> form InputField
setInput sym v = set (_Result sym) Nothing <<< set (_Touched sym) true <<< set (_Input sym) v

modifyInput
  :: ∀ sym form t0 fields e i o
   . IsSymbol sym
  => Newtype (form InputField) (Record fields)
  => Row.Cons sym (InputField e i o) t0 fields
  => SProxy sym
  -> (i -> i)
  -> form InputField
  -> form InputField
modifyInput sym f = set (_Result sym) Nothing <<< set (_Touched sym) true <<< (_Input sym) f

touchField
  :: ∀ sym form t0 fields e i o
   . IsSymbol sym
  => Newtype (form InputField) (Record fields)
  => Row.Cons sym (InputField e i o) t0 fields
  => SProxy sym
  -> form InputField
  -> form InputField
touchField sym = set (_Touched sym) true

resetField
  :: ∀ sym form t0 fields e i o
   . IsSymbol sym
  => Initial i
  => Newtype (form InputField) (Record fields)
  => Row.Cons sym (InputField e i o) t0 fields
  => SProxy sym
  -> form InputField
  -> form InputField
resetField sym =
  set (_Result sym) Nothing
  <<< set (_Touched sym) false
  <<< set (_Input sym) initial


----------
-- Class

-- | A function to unwrap a record of successful results into an equivalent
-- | record without any newtypes.
-- |
-- | For example, the below User type is identical to the Form type, if it
-- | only held the proper output type Name. You can unwrap a form of output
-- | fields directly into the User type:
-- |
-- | ```purescript
-- | type User = { name :: Name }
-- | newtype Form f = { name :: f String Error Name }
-- |
-- | formToUser :: Form OutputField -> User
-- | formToUser = unwrapOutput
-- | ```
-- |
-- | This is especially useful when creating a submitter function.
unwrapOutput
  :: ∀ row xs row' form
   . RL.RowToList row xs
  => Internal.UnwrapRecord xs row row'
  => Newtype (form OutputField) (Record row)
  => form OutputField
  -> Record row'
unwrapOutput = Internal.unwrapRecord <<< unwrap

-- | A function to transform a record of inputs of labels into a FormSpec.
-- |
-- | ```purescript
-- | newtype Form f = Form
-- |   { name :: f String String String
-- |   , email :: f String Void Email }
-- | derive instance newtypeForm :: Newtype (Form f) _
-- |
-- | -- To retrieve input types only, use the Input type synonym
-- | formSpec :: Form FormSpec
-- | formSpec = mkFormSpec
-- |   { name: ""
-- |   , email: "" }
-- | ```
mkFormSpec
  :: ∀ row xs row' form
   . RL.RowToList row xs
  => Internal.WrapRecord xs row row'
  => Newtype (form FormSpec) (Record row')
  => Record row
  -> form FormSpec
mkFormSpec = wrap <<< Internal.wrapRecord

-- | A function to transform a row of labels into a FormSpec. This allows you
-- | to go directly from a custom form newtype to a spec without having to
-- | fill in any values. Requires that all members have an instance of the
-- | `Initial` type class (all monoidal values do by default, along with some
-- | other primitives).
-- |
-- | ```purescript
-- | newtype Form f = Form (Record (MyRow f))
-- | derive instance newtypeForm :: Newtype (Form f) _
-- |
-- | type MyRow f =
-- |   ( name :: f String String String
-- |   , email :: f String Void String
-- |   , age :: f String String Int
-- |   )
-- |
-- | -- To retrieve input types only, use the Input type synonym
-- | formSpec :: Form FormSpec
-- | formSpec = mkFormSpecFromRow (RProxy :: RProxy (MyRow Input))
-- | ```
mkFormSpecFromRow
  :: ∀ row xs row' form
   . RL.RowToList row xs
  => MakeFormSpecFromRow xs row row'
  => Newtype (form FormSpec) (Record row')
  => RProxy row
  -> form FormSpec
mkFormSpecFromRow r = wrap $ Internal.fromScratch builder
  where builder = mkFormSpecFromRowBuilder (RLProxy :: RLProxy xs) r

-- | The class that provides the Builder implementation to efficiently
-- | transform a row into a proper FormSpec by wrapping it in newtypes and
-- | supplying initial values
class MakeFormSpecFromRow (xs :: RL.RowList) (row :: # Type) (to :: # Type) | xs -> to where
  mkFormSpecFromRowBuilder :: RLProxy xs -> RProxy row -> Internal.FromScratch to

instance mkFormSpecFromRowNil :: MakeFormSpecFromRow RL.Nil row () where
  mkFormSpecFromRowBuilder _ _ = identity

instance mkFormSpecFromRowCons
  :: ( IsSymbol name
     , Initial i
     , Row.Cons name i trash row
     , MakeFormSpecFromRow tail row from
     , Internal.Row1Cons name (FormSpec e i o) from to
     )
  => MakeFormSpecFromRow (RL.Cons name i tail) row to where
  mkFormSpecFromRowBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      val = FormSpec initial
      rest = mkFormSpecFromRowBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name val

mkLensesFromFormSpec
  :: ∀ form row xs row'
   . RL.RowToList row xs
  => MakeLenses form xs row'
  => Newtype (form FormSpec) (Record row)
  => form FormSpec
  -> Record row'
mkLensesFromFormSpec _ = mkLensesFromRow
  (Internal.FormProxy :: Internal.FormProxy form)
  (RProxy :: RProxy row)

mkLensesFromRow
  :: ∀ form row xs row'
   . RL.RowToList row xs
  => MakeLenses form xs row'
  => Internal.FormProxy form
  -> RProxy row
  -> Record row'
mkLensesFromRow _ _ = Internal.fromScratch builder where
  builder = mkLensesFromRowBuilder
    (Internal.FormProxy :: Internal.FormProxy form)
    (RLProxy :: RLProxy xs)

type FormLens form e i o =
  { field :: ALens' (form InputField) (Record (InputFieldRow e i o))
  , input :: ALens' (form InputField) i
  , touched :: ALens' (form InputField) Boolean
  , result :: ALens' (form InputField) (Maybe (Either e o))
  --  , error :: APrism' (form InputField) e
  --  , output :: APrism' (form InputField) o
  }

class MakeLenses form (xs :: RL.RowList) (to :: # Type) | xs -> to where
  mkLensesFromRowBuilder :: Internal.FormProxy form -> RLProxy xs -> Internal.FromScratch to

instance makeLensesNil :: MakeLenses form RL.Nil () where
  mkLensesFromRowBuilder _ _ = identity

instance makeLensesCons
  :: ( IsSymbol name
     , Newtype (form InputField) (Record row')
     , Row.Cons name (InputField e i o) trash row'
     , Internal.Row1Cons name (FormLens form e i o) from to
     , MakeLenses form tail from
     )
  => MakeLenses form (RL.Cons name (FormSpec e i o) tail) to where
  mkLensesFromRowBuilder form _ =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      first = Builder.insert _name val
      rest = mkLensesFromRowBuilder form (RLProxy :: RLProxy tail)

      val :: FormLens form e i o
      val =
        { field: _Field _name
        , input: _Input _name
        , touched: _Touched _name
        , result: _Result _name
        --  , error: _Error _name
        --  , output: _Output _name
        }
