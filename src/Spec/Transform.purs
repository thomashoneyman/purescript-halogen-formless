module Formless.Spec.Transform where

import Prelude

import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Formless.Class.Initial (class Initial, initial)
import Formless.Internal as Internal
import Formless.Spec (FormSpec(..), OutputField)
import Prim.Row as Row
import Prim.RowList as RL
import Record.Builder as Builder
import Type.Row (RLProxy(..), RProxy)

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
     , Internal.Row1Cons name (FormSpec i e o) from to
     )
  => MakeFormSpecFromRow (RL.Cons name i tail) row to where
  mkFormSpecFromRowBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      val = FormSpec initial
      rest = mkFormSpecFromRowBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name val
