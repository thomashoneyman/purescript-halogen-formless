module Formless.Spec where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, wrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Formless.Class.Initial (class Initial, initial)
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

-- | The type that we need to record state across the form, but
-- | we don't need this from the user -- we can fill in 'touched'
-- | and 'result' on their behalf.
newtype InputField input error output = InputField
  { input :: input
  , touched :: Boolean
  , result :: Maybe (Either error output) -- force the user to end up in Either?
  }
derive instance newtypeInputField :: Newtype (InputField i e o) _

-- | Proxies for each of the fields in InputField and FormSpec for easy access
_input = SProxy :: SProxy "input"
_touched = SProxy :: SProxy "touched"
_validator = SProxy :: SProxy "validator"
_result = SProxy :: SProxy "result"

-- | A wrapper to represent only the output type. Used to represent
-- | form results at the end of validation.
newtype OutputField input error output = OutputField output
derive instance newtypeOutputField :: Newtype (OutputField i e o) _

-- | Perhaps temporary; never exposed to the user, but used to
-- | aid transformations
newtype MaybeOutput i e o = MaybeOutput (Maybe o)
derive instance newtypeMaybeOutput :: Newtype (MaybeOutput i e o) _


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

-- | A function to transform a record of initial values into a FormSpec. Exists to save
-- | you from too many redundant key strokes.
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

-- | The class that provides the Builder implementation to efficiently transform a normal
-- | into a proper FormSpec by wrapping it in newtypes
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

-- | A function to transform a row of labels into a FormSpec. This allows you to go directly
-- | from a custom form newtype to a spec without having to fill in any values at all. Requires
-- | that all members have an instance of the `Initial` type class (all monoidal values do by
-- | default, along with some other primitives).
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

-- | The class that provides the Builder implementation to efficiently transform a normal
-- | into a proper FormSpec by wrapping it in newtypes
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
