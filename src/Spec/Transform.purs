module Formless.Spec.Transform where

import Prelude

import Data.Either (Either)
import Data.Lens (set, view)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Formless.Class.Initial (class Initial, initial)
import Formless.Internal as Internal
import Formless.Spec (FormProxy, FormSpec(..), InputField, OutputField, _Input, _Result, _Touched)
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
-- | formSpec :: Form FormSpec
-- | formSpec = mkFormSpecFromProxy (FormProxy :: FormProxy Form)
-- | ```
mkFormSpecFromProxy
  :: ∀ row xs row' form' form
   . RL.RowToList row xs
  => MakeFormSpecFromRow xs row row'
  => Newtype (form Internal.Input) (Record row)
  => Newtype (form' FormSpec) (Record row')
  => FormProxy form
  -> form' FormSpec
mkFormSpecFromProxy _ = wrap $ Internal.fromScratch builder
  where
    builder = mkFormSpecFromRowBuilder
      (RLProxy :: RLProxy xs)
      (RProxy :: RProxy row)

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
     , Row.Cons name (Internal.Input e i o) trash row
     , MakeFormSpecFromRow tail row from
     , Internal.Row1Cons name (FormSpec e i o) from to
     )
  => MakeFormSpecFromRow (RL.Cons name (Internal.Input e i o) tail) row to where
  mkFormSpecFromRowBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      val = FormSpec initial
      rest = mkFormSpecFromRowBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name val


-- | A type to collect constraints necessary to apply to prove that a record of
-- | SProxies is compatible with your form type.
type SProxies form =
   ∀ row xs row'
    . RL.RowToList row xs
   => MakeSProxies xs row'
   => Newtype (form FormSpec) (Record row)
   => Record row'

-- | A helper function to produce a record of SProxies given a form spec, to save
-- | you the boilerplate of writing them all out.
-- |
-- | ```purescript
-- | newtype Form f = Form
-- |   { name :: f Void String String
-- |   , email :: f Void String String
-- |   , city :: f Void Int String
-- |   , other :: f Int String Int
-- |   }
-- | derive instance newtypeForm :: Newtype (Form f) _
-- |
-- | proxies :: Proxies Form
-- | proxies = mkSProxies (FormProxy :: FormProxy Form)
-- |
-- | -- You can now access all your proxies from the record with dot syntax
-- | _name :: SProxy "name"
-- | _name = proxies.name
-- | ```
mkSProxies
  :: ∀ form row xs row'
   . RL.RowToList row xs
  => MakeSProxies xs row'
  => Newtype (form FormSpec) (Record row)
  => FormProxy form
  -> Record row'
mkSProxies _ = Internal.fromScratch builder
  where
    builder = makeSProxiesBuilder (RLProxy :: RLProxy xs)

-- | The class used to build up a new record of symbol proxies from an
-- | input row list.
class MakeSProxies (xs :: RL.RowList) (to :: # Type) | xs -> to where
  makeSProxiesBuilder :: RLProxy xs -> Internal.FromScratch to

instance makeSProxiesNil :: MakeSProxies RL.Nil () where
  makeSProxiesBuilder _ = identity

instance makeSProxiesCons
  :: ( IsSymbol name
     , Internal.Row1Cons name (SProxy name) from to
     , MakeSProxies tail from
     )
  => MakeSProxies (RL.Cons name x tail) to where
  makeSProxiesBuilder _ = first <<< rest
    where
      rest = makeSProxiesBuilder (RLProxy :: RLProxy tail)
      first = Builder.insert (SProxy :: SProxy name) (SProxy :: SProxy name)
