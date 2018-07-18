module Formless.Spec where

import Prelude

import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Formless.Class.Initial (class Initial, initial)
import Prim.Row (class Cons)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Row (RLProxy(..), RProxy)

-- | The type that will be applied to the user's input row to
-- | create the spec form that we'll compare against to measure
-- | 'touched' states, etc. This is what the user is responsible
-- | for providing.
newtype FormSpec input error output = FormSpec input
derive instance newtypeFormSpec :: Newtype (FormSpec i e o) _

-- | A wrapper to represent only the output type. Used to represent
-- | form results at the end of validation.
newtype OutputField input error output = OutputField output
derive instance newtypeOutputField :: Newtype (OutputField i e o) _

-- | The type that we need to record state across the form, but
-- | we don't need this from the user -- we can fill in 'touched'
-- | and 'result' on their behalf.
newtype InputField input error output = InputField
  { input :: input
  , touched :: Boolean -- Whether the field has been changed by the user
  , result :: Maybe (Either error output)
  }
derive instance newtypeInputField :: Newtype (InputField i e o) _

-- | Proxies for each of the fields in InputField and FormSpec for easy access
_input = SProxy :: SProxy "input"
_touched = SProxy :: SProxy "touched"
_result = SProxy :: SProxy "result"

-- | Easy access to any given field from the form, unwrapped
getField
  :: ∀ sym form t0 field fields i e o
   . IsSymbol sym
  => Newtype (form InputField) (Record fields)
  => Cons sym (InputField i e o) t0 fields
  => Newtype (InputField i e o) field
  => SProxy sym
  -> form InputField
  -> field
getField sym form =
  unwrap $ Record.get sym $ unwrap form

-- | Easy access to any given field's input value from the form
getInput
  :: ∀ sym form t0 fields i e o
   . IsSymbol sym
  => Newtype (form InputField) (Record fields)
  => Cons sym (InputField i e o) t0 fields
  => SProxy sym
  -> form InputField
  -> i
getInput sym = _.input <<< getField sym

-- | Easy access to any given field's touched value from the form
getTouched
  :: ∀ sym form t0 fields i e o
   . IsSymbol sym
  => Newtype (form InputField) (Record fields)
  => Cons sym (InputField i e o) t0 fields
  => SProxy sym
  -> form InputField
  -> Boolean
getTouched sym = _.touched <<< getField sym

-- | Easy access to any given field's result value from the form, if the
-- | result exists.
getResult
  :: ∀ sym form t0 fields i e o
   . IsSymbol sym
  => Newtype (form InputField) (Record fields)
  => Cons sym (InputField i e o) t0 fields
  => SProxy sym
  -> form InputField
  -> Maybe (Either e o)
getResult sym = _.result <<< getField sym

-- | Easy access to any given field's error from its result field in the form,
-- | if the error exists.
getError
  :: ∀ sym form t0 fields i e o
   . IsSymbol sym
  => Newtype (form InputField) (Record fields)
  => Cons sym (InputField i e o) t0 fields
  => SProxy sym
  -> form InputField
  -> Maybe e
getError sym form = case getResult sym form of
  Just (Left e) -> Just e
  _ -> Nothing

-- | Easy access to any given field's output from its result field in the form,
-- | if the output exists.
getOutput
  :: ∀ sym form t0 fields i e o
   . IsSymbol sym
  => Newtype (form InputField) (Record fields)
  => Cons sym (InputField i e o) t0 fields
  => SProxy sym
  -> form InputField
  -> Maybe o
getOutput sym = join <<< map hush <<< getResult sym


----------
-- Helper types

-- | A type synonym that lets you pick out just the input type from
-- | your form row.
type Input input error output = input

-- | A type synonym that lets you pick out just the error type from
-- | your form row.
type Error input error output = error

-- | A type synonym that lets you pick out just the output type from
-- | your form row.
type Output input error output = output

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
  :: ∀ row xs row' form wrapper
   . RL.RowToList row xs
  => UnwrapOutput xs row () row'
  => Newtype (form wrapper) (Record row)
  => form wrapper
  -> Record row'
unwrapOutput r = Builder.build builder {}
  where
    builder = unwrapOutputBuilder (RLProxy :: RLProxy xs) (unwrap r)

-- | The class that provides the Builder implementation to efficiently unpack
-- | a record of output fields into a simple record of only the values.
class UnwrapOutput
  (xs :: RL.RowList) (row :: # Type) (from :: # Type) (to :: # Type)
  | xs -> from to where
  unwrapOutputBuilder :: RLProxy xs -> Record row -> Builder { | from } { | to }

instance unwrapOutputNil :: UnwrapOutput RL.Nil row () () where
  unwrapOutputBuilder _ _ = identity

instance unwrapOutputCons
  :: ( IsSymbol name
     , Row.Cons name (wrapper i e o) trash row
     , Newtype (wrapper i e o) o
     , UnwrapOutput tail row from from'
     , Row.Lacks name from'
     , Row.Cons name o from' to
     )
  => UnwrapOutput (RL.Cons name (wrapper i e o) tail) row from to where
  unwrapOutputBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      val = unwrap $ Record.get _name r
      rest = unwrapOutputBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name val


-- | A function to transform a record of initial values into a FormSpec.
-- | Exists to save you from too many redundant key strokes.
-- |
-- | ```purescript
-- | newtype Form f = Form
-- |   { name :: f String String String }
-- | derive instance newtypeForm :: Newtype (Form f) _
-- |
-- | formSpec :: Form FormSpec
-- | formSpec = mkFormSpec { name: "" }
-- | ```
mkFormSpec
  :: ∀ row xs row' form
   . RL.RowToList row xs
  => MakeFormSpec xs row () row'
  => Newtype (form FormSpec) (Record row')
  => Record row
  -> form FormSpec
mkFormSpec r = wrap $ Builder.build builder {}
  where
    builder = mkFormSpecBuilder (RLProxy :: RLProxy xs) r

-- | The class that provides the Builder implementation to efficiently
-- | transform a record into a proper FormSpec by wrapping it in newtypes
class MakeFormSpec
  (xs :: RL.RowList) (row :: # Type) (from :: # Type) (to :: # Type)
  | xs -> from to where
  mkFormSpecBuilder :: RLProxy xs -> Record row -> Builder { | from } { | to }

instance mkFormSpecNil :: MakeFormSpec RL.Nil row () () where
  mkFormSpecBuilder _ _ = identity

instance mkFormSpecCons
  :: ( IsSymbol name
     , Row.Cons name i trash row
     , MakeFormSpec tail row from from'
     , Row.Lacks name from'
     , Row.Cons name (FormSpec i e o) from' to
     )
  => MakeFormSpec (RL.Cons name i tail) row from to where
  mkFormSpecBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      val = FormSpec $ Record.get _name r
      rest = mkFormSpecBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name val

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
  => MakeFormSpecFromRow xs row () row'
  => Newtype (form FormSpec) (Record row')
  => RProxy row
  -> form FormSpec
mkFormSpecFromRow r = wrap $ Builder.build builder {}
  where
    builder = mkFormSpecFromRowBuilder (RLProxy :: RLProxy xs) r

-- | The class that provides the Builder implementation to efficiently
-- | transform a row into a proper FormSpec by wrapping it in newtypes and
-- | supplying initial values
class MakeFormSpecFromRow
  (xs :: RL.RowList) (row :: # Type) (from :: # Type) (to :: # Type)
  | xs -> from to where
  mkFormSpecFromRowBuilder :: RLProxy xs -> RProxy row -> Builder { | from } { | to }

instance mkFormSpecFromRowNil :: MakeFormSpecFromRow RL.Nil row () () where
  mkFormSpecFromRowBuilder _ _ = identity

instance mkFormSpecFromRowCons
  :: ( IsSymbol name
     , Initial i
     , Row.Cons name i trash row
     , MakeFormSpecFromRow tail row from from'
     , Row.Lacks name from'
     , Row.Cons name (FormSpec i e o) from' to
     )
  => MakeFormSpecFromRow (RL.Cons name i tail) row from to where
  mkFormSpecFromRowBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      val = FormSpec initial
      rest = mkFormSpecFromRowBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name val
