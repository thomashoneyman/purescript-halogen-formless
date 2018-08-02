module Formless.Internal where

import Prelude

import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Variant (Variant, on)
import Formless.Spec (FormInput(..), FormProxy, FormSpec(..), InputField(..), OutputField(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.RowList (RLProxy(..))
import Type.Row (RProxy(..))

-----
-- Types

-- | Represents building some output record from an empty record
type FromScratch r = Builder {} (Record r)

-- | Apply a builder that produces an output record from an empty record
fromScratch :: ∀ r. FromScratch r -> Record r
fromScratch = Builder.build <@> {}

-- | A constraint synonym for Row.Cons and Row.Lacks
class (Row.Cons s t r r', Row.Lacks s r) <= Row1Cons s t r r' | s t r -> r', s r' -> t r
instance row1Cons :: (Row.Cons s t r r', Row.Lacks s r) => Row1Cons s t r r'

-- | @monoidmusician
class (Row1Cons s t r r', RL.RowToList r rl, RL.RowToList r' rl')
  <= Row3 s t r r' rl rl'
  | rl' -> s t r r' rl
  , rl -> r
  , s t r -> r' rl rl'
  , s t rl -> r r' rl'
instance row3 ::
  (Row1Cons s t r r', RL.RowToList r rl, RL.RowToList r' (RL.Cons s t rl))
  => Row3 s t r r' rl (RL.Cons s t rl)

-----
-- Functions

-- | Unwraps all the fields in a record, so long as all fields have newtypes
unwrapRecord
  :: ∀ row xs row'
   . RL.RowToList row xs
  => UnwrapRecord xs row row'
  => Record row
  -> Record row'
unwrapRecord = fromScratch <<< unwrapRecordBuilder (RLProxy :: RLProxy xs)

-- | Wraps all the fields in a record, so long as all fields have proper newtype
-- | instances
wrapRecord
  :: ∀ row xs row'
   . RL.RowToList row xs
  => WrapRecord xs row row'
  => Record row
  -> Record row'
wrapRecord = fromScratch <<< wrapRecordBuilder (RLProxy :: RLProxy xs)

-- | Sequences a record of applicatives. Useful when applying monadic field validation
-- | so you can recover the proper type for the Formless validation function. Does not
-- | operate on newtypes, so you'll want to unwrap / re-wrap your form type when using.
sequenceRecord :: ∀ row row' rl m
   . RL.RowToList row rl
  => Applicative m
  => SequenceRecord rl row row' m
  => Record row
  -> m (Record row')
sequenceRecord = map fromScratch <<< sequenceRecordImpl (RLProxy :: RLProxy rl)

-- | A helper function that will count all errors in a record
checkTouched
  :: ∀ form row xs
   . RL.RowToList row xs
  => AllTouched xs row
  => Newtype (form Record FormInput) (Record row)
  => form Record FormInput
  -> Boolean
checkTouched = allTouchedImpl (RLProxy :: RLProxy xs) <<< unwrap

-- | A helper function that will count all errors in a record
countErrors
  :: ∀ form row xs row' xs'
   . RL.RowToList row xs
  => RL.RowToList row' xs'
  => CountErrors xs row row'
  => SumRecord xs' row' (Additive Int)
  => Newtype (form Record FormInput) (Record row)
  => form Record FormInput
  -> Int
countErrors r = unwrap $ sumRecord $ fromScratch builder
  where builder = countErrorsBuilder (RLProxy :: RLProxy xs) (unwrap r)

-- | A helper function that sums a monoidal record
sumRecord
  :: ∀ r rl a
   . SumRecord rl r a
  => RL.RowToList r rl
  => Record r
  -> a
sumRecord = sumImpl (RLProxy :: RLProxy rl)

-- | A helper function that will set all input fields to 'touched = true'. This ensures
-- | subsequent validations apply to all fields even if not edited by the user.
setFormInputsTouched
  :: ∀ row xs form
   . RL.RowToList row xs
  => SetFormInputsTouched xs row row
  => Newtype (form Record FormInput) (Record row)
  => form Record FormInput
  -> form Record FormInput
setFormInputsTouched r = wrap $ fromScratch builder
  where builder = setFormInputsTouchedBuilder (RLProxy :: RLProxy xs) (unwrap r)

-- | A helper function that will automatically transform a record of FormInput(s) into
-- | just the input value
inputFieldsToInput
  :: ∀ row xs row' form
   . RL.RowToList row xs
  => FormInputsToInput xs row row'
  => Newtype (form Record FormInput) (Record row)
  => Newtype (form Record InputField) (Record row')
  => form Record FormInput
  -> form Record InputField
inputFieldsToInput r = wrap $ fromScratch builder
  where builder = inputFieldsToInputBuilder (RLProxy :: RLProxy xs) (unwrap r)

-- | A helper function that will automatically transform a record of FormSpec(s) into
-- | a record of FormInput(s).
formSpecToFormInputs
  :: ∀ row xs row' form
   . RL.RowToList row xs
  => FormSpecToFormInput xs row row'
  => Newtype (form Record FormSpec) (Record row)
  => Newtype (form Record FormInput) (Record row')
  => form Record FormSpec
  -> form Record FormInput
formSpecToFormInputs r = wrap $ fromScratch builder
  where builder = formSpecToFormInputBuilder (RLProxy :: RLProxy xs) (unwrap r)

-- | An intermediate function that transforms a record of FormInput into a record
-- | of MaybeOutput as a step in producing output fields.
inputFieldToMaybeOutput
  :: ∀ row xs row' form
   . RL.RowToList row xs
  => FormInputToMaybeOutput xs row row'
  => Newtype (form Record FormInput) (Record row)
  => Newtype (form Record OutputField) (Record row')
  => form Record FormInput
  -> Maybe (form Record OutputField)
inputFieldToMaybeOutput r = map wrap $ fromScratch <$> builder
  where builder = inputFieldToMaybeOutputBuilder (RLProxy :: RLProxy xs) (unwrap r)

-----
-- Classes (Internal)

-- | A class to set all input fields to touched for validation purposes
class SetFormInputsTouched (xs :: RL.RowList) (row :: # Type) (to :: # Type) | xs -> to where
  setFormInputsTouchedBuilder :: RLProxy xs -> Record row -> FromScratch to

instance setFormInputsTouchedNil :: SetFormInputsTouched RL.Nil row () where
  setFormInputsTouchedBuilder _ _ = identity

instance setFormInputsTouchedCons
  :: ( IsSymbol name
     , Row.Cons name (FormInput e i o) trash row
     , Row1Cons name (FormInput e i o) from to
     , SetFormInputsTouched tail row from
     )
  => SetFormInputsTouched (RL.Cons name (FormInput e i o) tail) row to where
  setFormInputsTouchedBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      val = transform $ Record.get _name r
      rest = setFormInputsTouchedBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name val
      transform (FormInput i) = FormInput i { touched = true }

-- | The class that provides the Builder implementation to efficiently transform the record
-- | of FormSpec to record of FormInput.
class FormInputsToInput (xs :: RL.RowList) (row :: # Type) (to :: # Type) | xs -> to where
  inputFieldsToInputBuilder :: RLProxy xs -> Record row -> FromScratch to

instance inputFieldsToInputNil :: FormInputsToInput RL.Nil row () where
  inputFieldsToInputBuilder _ _ = identity

instance inputFieldsToInputCons
  :: ( IsSymbol name
     , Row.Cons name (FormInput e i o) trash row
     , FormInputsToInput tail row from
     , Row1Cons name (InputField e i o) from to
     )
  => FormInputsToInput (RL.Cons name (FormInput e i o) tail) row to where
  inputFieldsToInputBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      val = transform $ Record.get _name r
      rest = inputFieldsToInputBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name val
      transform (FormInput fields) = InputField fields.input

-- | The class that provides the Builder implementation to efficiently transform the record
-- | of FormSpec to record of FormInput.
class FormSpecToFormInput (xs :: RL.RowList) (row :: # Type) (to :: # Type) | xs -> to where
  formSpecToFormInputBuilder :: RLProxy xs -> Record row -> FromScratch to

instance formSpecToFormInputNil :: FormSpecToFormInput RL.Nil row () where
  formSpecToFormInputBuilder _ _ = identity

instance formSpecToFormInputCons
  :: ( IsSymbol name
     , Row.Cons name (FormSpec e i o) trash row
     , FormSpecToFormInput tail row from
     , Row1Cons name (FormInput e i o) from to
     )
  => FormSpecToFormInput (RL.Cons name (FormSpec e i o) tail) row to where
  formSpecToFormInputBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      val = transform $ Record.get _name r
      rest = formSpecToFormInputBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name val
      transform (FormSpec input) = FormInput
        { input
        , touched: false
        , result: Nothing
        }

-- | The class that provides the Builder implementation to efficiently transform the record
-- | of MaybeOutput to a record of OutputField, but only if all fields were successfully
-- | validated.
class FormInputToMaybeOutput (xs :: RL.RowList) (row :: # Type) (to :: # Type) | xs -> to where
  inputFieldToMaybeOutputBuilder :: RLProxy xs -> Record row -> Maybe (FromScratch to)

instance inputFieldToMaybeOutputNil :: FormInputToMaybeOutput RL.Nil row () where
  inputFieldToMaybeOutputBuilder _ _ = Just identity

instance inputFieldToMaybeOutputCons
  :: ( IsSymbol name
     , Row.Cons name (FormInput e i o) trash row
     , FormInputToMaybeOutput tail row from
     , Row1Cons name (OutputField e i o) from to
     )
  => FormInputToMaybeOutput (RL.Cons name (FormInput e i o) tail) row to where
  inputFieldToMaybeOutputBuilder _ r =
    transform <$> val <*> rest
    where
      _name = SProxy :: SProxy name

      val :: Maybe (OutputField e i o)
      val = map OutputField $ join $ map hush $ _.result $ unwrap $ Record.get _name r

      rest :: Maybe (FromScratch from)
      rest = inputFieldToMaybeOutputBuilder (RLProxy :: RLProxy tail) r

      transform :: OutputField e i o -> FromScratch from -> FromScratch to
      transform v builder' = Builder.insert _name v <<< builder'

-- | A class to sum a monoidal record
class Monoid a <= SumRecord (rl :: RL.RowList) (r :: # Type) a | rl -> a where
  sumImpl :: RLProxy rl -> Record r -> a

instance nilSumRecord :: Monoid a => SumRecord RL.Nil r a where
  sumImpl _ _ = mempty

instance consSumRecord
  :: ( IsSymbol name
     , Monoid a
     , Row.Cons name a t0 r
     , SumRecord tail r a
     )
  => SumRecord (RL.Cons name a tail) r a
  where
    sumImpl _ r =
      -- This has to be defined in a variable for some reason; it won't
      -- compile otherwise, but I don't know why not.
      let tail' = sumImpl (RLProxy :: RLProxy tail) r
          val = Record.get (SProxy :: SProxy name) r
       in val <> tail'

-- | Gets out ints
class CountErrors (xs :: RL.RowList) (row :: # Type) (to :: # Type) | xs -> to where
  countErrorsBuilder :: RLProxy xs -> Record row -> FromScratch to

instance countErrorsNil :: CountErrors RL.Nil row () where
  countErrorsBuilder _ _ = identity

instance countErrorsCons
  :: ( IsSymbol name
     , Row.Cons name (FormInput e i o) trash row
     , CountErrors tail row from
     , Row1Cons name (Additive Int) from to
     )
  => CountErrors (RL.Cons name (FormInput e i o) tail) row to where
  countErrorsBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      val = transform $ Record.get _name r
      rest = countErrorsBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name val
      transform (FormInput { result }) =
        case result of
          Just (Left _) -> Additive 1
          _ -> Additive 0

-- | A class to check if all fields in an FormInput record have been touched or not
class AllTouched (rl :: RL.RowList) (r :: # Type) where
  allTouchedImpl :: RLProxy rl -> Record r -> Boolean

instance nilAllTouched :: AllTouched RL.Nil r where
  allTouchedImpl _ _ = true

instance consAllTouched
  :: ( IsSymbol name
     , Row.Cons name (FormInput e i o) t0 r
     , AllTouched tail r
     )
  => AllTouched (RL.Cons name (FormInput e i o) tail) r
  where
    allTouchedImpl _ r =
      if (unwrap (Record.get (SProxy :: SProxy name) r)).touched
        then allTouchedImpl (RLProxy :: RLProxy tail) r
        else false

-- | The class to efficiently unwrap a record of newtypes
class UnwrapRecord (xs :: RL.RowList) (row :: # Type) (to :: # Type) | xs -> to where
  unwrapRecordBuilder :: RLProxy xs -> Record row -> FromScratch to

instance unwrapRecordNil :: UnwrapRecord RL.Nil row () where
  unwrapRecordBuilder _ _ = identity

instance unwrapRecordCons
  :: ( IsSymbol name
     , Row.Cons name wrapper trash row
     , Newtype wrapper x
     , UnwrapRecord tail row from
     , Row1Cons name x from to
     )
  => UnwrapRecord (RL.Cons name wrapper tail) row to where
  unwrapRecordBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      val = unwrap $ Record.get _name r
      rest = unwrapRecordBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name val

-- | The class to efficiently wrap a record of newtypes
class WrapRecord (xs :: RL.RowList) (row :: # Type) (to :: # Type) | xs -> to where
  wrapRecordBuilder :: RLProxy xs -> Record row -> FromScratch to

instance wrapRecordNil :: WrapRecord RL.Nil row () where
  wrapRecordBuilder _ _ = identity

instance wrapRecordCons
  :: ( IsSymbol name
     , Row.Cons name x trash row
     , Newtype wrapper x
     , WrapRecord tail row from
     , Row1Cons name wrapper from to
     )
  => WrapRecord (RL.Cons name x tail) row to where
  wrapRecordBuilder _ r =
    first <<< rest
    where
      _name = SProxy :: SProxy name
      val = wrap $ Record.get _name r
      rest = wrapRecordBuilder (RLProxy :: RLProxy tail) r
      first = Builder.insert _name val

-- | The class to efficiently run the build with sequenceRecord on a record.
class Applicative m <= SequenceRecord rl row to m | rl -> row to m where
  sequenceRecordImpl :: RLProxy rl -> Record row -> m (FromScratch to)

instance sequenceRecordNil :: Applicative m => SequenceRecord RL.Nil row () m where
  sequenceRecordImpl _ _ = pure identity

instance sequenceRecordCons ::
  ( IsSymbol name
  , Applicative m
  , Row.Cons name (m ty) trash row
  , SequenceRecord tail row from m
  , Row1Cons name ty from to
  ) => SequenceRecord (RL.Cons name (m ty) tail) row to m where
  sequenceRecordImpl _ a  =
    fn <$> valA <*> rest
    where
      namep = SProxy :: SProxy name
      valA = Record.get namep a
      tailp = RLProxy :: RLProxy tail
      rest = sequenceRecordImpl tailp a
      fn valA' rest' = Builder.insert namep valA' <<< rest'

-- | A class to reduce the type variables required to use applyRecord
class ApplyRecord (io :: # Type) (i :: # Type) (o :: # Type)
  | io -> i o
  , i -> io o
  , o -> io i
  where
  applyRecord :: Record io -> Record i -> Record o

instance applyRecordImpl
  :: ( RL.RowToList io lio
     , RL.RowToList i li
     , RL.RowToList o lo
     , ApplyRowList lio li lo io i io i o
     )
  => ApplyRecord io i o where
  applyRecord io i = Builder.build (builder io i) {}
    where
      builder =
        applyRowList
        (RLProxy :: RLProxy lio)
        (RLProxy :: RLProxy li)
        (RLProxy :: RLProxy lo)

-- | Modified from the original by @LiamGoodacre. Significantly improved
-- | by @MonoidMusician.
-- |
-- | Applies a record of functions to a record of input values to produce
-- | a record of outputs.
class
  ( RL.RowToList ior io
  , RL.RowToList ir i
  , RL.RowToList or o
  ) <=
  ApplyRowList
    (io :: RL.RowList)
    (i :: RL.RowList)
    (o :: RL.RowList)
    (ior :: # Type)
    (ir :: # Type)
    (iorf :: # Type)
    (irf :: # Type)
    (or :: # Type)
    | io -> i o ior ir or
    , i -> io o ior ir or
    , o -> io i ior ir or
  where
  applyRowList
    :: RLProxy io
    -> RLProxy i
    -> RLProxy o
    -> Record iorf
    -> Record irf
    -> FromScratch or

instance applyRowListNil :: ApplyRowList RL.Nil RL.Nil RL.Nil () () iorf irf () where
  applyRowList _ _ _ _ _ = identity

instance applyRowListCons
  :: ( Row.Cons k (i -> o) unused1 iorf
     , Row.Cons k i unused2 irf
     , Row3 k (i -> o) tior ior tio (RL.Cons k (i -> o) tio)
     , Row3 k i tir ir ti (RL.Cons k i ti)
     , Row3 k o tor or to (RL.Cons k o to)
     , ApplyRowList tio ti to tior tir iorf irf tor
     , IsSymbol k
     )
  => ApplyRowList
       (RL.Cons k (i -> o) tio)
       (RL.Cons k i ti)
       (RL.Cons k o to)
       ior
       ir
       iorf
       irf
       or
  where
    applyRowList io i o ior ir =
      fir <<< tor
      where
        _key = SProxy :: SProxy k
        f = Record.get _key ior
        x = Record.get _key ir

        fir :: Builder { | tor } { | or }
        fir = Builder.insert _key (f x)

        tor :: FromScratch tor
        tor = applyRowList (rltail io) (rltail i) (rltail o) ior ir

        rltail :: ∀ l v t. RLProxy (RL.Cons l v t) -> RLProxy t
        rltail _ = RLProxy

buildInputSetters
  :: ∀ rl form row rin fin rout fout
   . RL.RowToList row rl
  => BuildInputSetters rl rin fin rout fout
  => Newtype (form Record FormInput) (Record row)
  => FormProxy form
  -> (Variant rin -> Record fout -> Record fout)
  -> Variant rout
  -> Record fout
  -> Record fout
buildInputSetters k =
  buildInputSettersImpl (RLProxy :: RLProxy rl) (RProxy :: RProxy fin)

class BuildInputSetters rl rin fin rout fout | rl rin fin -> rout fout where
  buildInputSettersImpl
    :: RLProxy rl
    -> RProxy fin
    -> (Variant rin -> Record fout -> Record fout)
    -> Variant rout
    -> Record fout
    -> Record fout

instance inputSetterNil :: BuildInputSetters RL.Nil r fin r fout where
  buildInputSettersImpl _ _ = identity

instance inputSetterCons ::
  ( IsSymbol sym
  , Row.Cons sym (FormInput e i o) fin fout
  , Row.Cons sym (InputField e i o) rout' rout
  , BuildInputSetters tail rin fin' rout' fout
  ) => BuildInputSetters (RL.Cons sym (InputField e i o) tail) rin fin rout fout
  where
  buildInputSettersImpl _ _ =
    on sym f <<< rest
    where
      sym = SProxy :: SProxy sym

      f a = Record.set sym $ FormInput
        { input: unwrap a
        , touched: false
        , result: Nothing
        }

      rest = buildInputSettersImpl
        (RLProxy :: RLProxy tail)
        (RProxy :: RProxy fin')
