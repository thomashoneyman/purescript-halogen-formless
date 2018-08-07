module Formless.Spec.Transform where

import Prelude

import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Formless.Class.Initial (class Initial, initial)
import Formless.Internal as Internal
import Formless.Spec (FormProxy, InputField(..), OutputField, Validator)
import Prim.Row as Row
import Prim.RowList as RL
import Record.Builder as Builder
import Type.Row (RLProxy(..), RProxy(..))

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
  => Newtype (form Record OutputField) (Record row)
  => form Record OutputField
  -> Record row'
unwrapOutput = Internal.unwrapRecord <<< unwrap

-- | A function to transform a record of validators into correct one for the form
-- |
-- | ```purescript
-- | ```

mkValidators
  :: ∀ row row' xs form m
   . RL.RowToList row xs
  => Internal.WrapRecord xs row row'
  => Newtype (form Record (Validator m)) (Record row')
  => Record row
  -> form Record (Validator m)
mkValidators = wrap <<< Internal.wrapRecord

-- | A function to transform a record of inputs of labels into a InputFields.
-- |
-- | ```purescript
-- | newtype Form f = Form
-- |   { name :: f String String String
-- |   , email :: f String Void Email }
-- | derive instance newtypeForm :: Newtype (Form f) _
-- |
-- | -- To retrieve input types only, use the Input type synonym
-- | inputFields :: Form InputFields
-- | inputFields = mkInputFields
-- |   { name: ""
-- |   , email: "" }
-- | ```

mkInputFields
  :: ∀ row row' xs form
   . RL.RowToList row xs
  => Internal.WrapRecord xs row row'
  => Newtype (form Record InputField) (Record row')
  => Record row
  -> form Record InputField
mkInputFields = wrap <<< Internal.wrapRecord

-- | A function to transform a row of labels into a InputFields. This allows you
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
-- | inputFields :: Form InputFields
-- | inputFields = mkInputFieldsFromProxy (FormProxy :: FormProxy Form)
-- | ```

mkInputFieldsFromProxy
  :: ∀ row xs form
   . RL.RowToList row xs
  => MakeInputFieldsFromRow xs row row
  => Newtype (form Record InputField) (Record row)
  => FormProxy form
  -> form Record InputField
mkInputFieldsFromProxy _ = wrap $ Internal.fromScratch builder
  where
    builder = mkInputFieldsFromRowBuilder
      (RLProxy :: RLProxy xs)
      (RProxy :: RProxy row)

-- | The class that provides the Builder implementation to efficiently
-- | transform a row into a proper InputFields by wrapping it in newtypes and
-- | supplying initial values
class MakeInputFieldsFromRow (xs :: RL.RowList) (row :: # Type) (to :: # Type) | xs -> to where
  mkInputFieldsFromRowBuilder :: RLProxy xs -> RProxy row -> Internal.FromScratch to

instance mkInputFieldsFromRowNil :: MakeInputFieldsFromRow RL.Nil row () where
  mkInputFieldsFromRowBuilder _ _ = identity

instance mkInputFieldsFromRowCons
  :: ( IsSymbol name
     , Initial i
     , Row.Cons name (InputField e i o) trash row
     , MakeInputFieldsFromRow tail row from
     , Internal.Row1Cons name (InputField e i o) from to
     )
  => MakeInputFieldsFromRow (RL.Cons name (InputField e i o) tail) row to where
  mkInputFieldsFromRowBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      val = InputField initial
      rest = mkInputFieldsFromRowBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name val

-- | A type to collect constraints necessary to apply to prove that a record of
-- | SProxies is compatible with your form type.
type SProxies form =
   ∀ row xs row'
    . RL.RowToList row xs
   => MakeSProxies xs row'
   => Newtype (form Record InputField) (Record row)
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
  => Newtype (form Record InputField) (Record row)
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

