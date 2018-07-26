module Example.Polyform.Spec where

import Prelude

import Data.Newtype (class Newtype)
import Data.String (Pattern(..), contains, length)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (random)
import Formless.Spec (FormSpec, InputType, InputField, OutputType, OutputField)
import Formless.Spec.Transform (mkFormSpecFromRow, unwrapOutput)
import Formless.Validation.Polyform (applyOnInputFields)
import Polyform.Validation (V(..), Validation(..))
import Polyform.Validation as Validation
import Type.Row (type (+), RProxy(..))

-- | Our overall form type, derived from the form row.
newtype Form f = Form (Record (FormRow f))
derive instance newtypeForm :: Newtype (Form f) _

-- | Some useful types we'll parse to
newtype Name = Name String
derive instance newtypeName :: Newtype Name _
derive newtype instance showName :: Show Name

newtype Email = Email String
derive instance newtypeEmail :: Newtype Email _
derive newtype instance showEmail :: Show Email

-- | Our ideal result type
type User = Record (FormRow OutputType)

-- | We'll use this row to generate our form spec, but also to represent the
-- | available fields in the record.
type FormRow f =
  ( name  :: f (Errs (Min + Max ()))       String Name
  , email :: f (Errs (EFormat + EUsed ())) String Email
  , city  :: f (Errs (Min ()))             String String
  , state :: f (Errs ())                   String String
  )

-- | You'll usually want symbol proxies for convenience
_name = SProxy :: SProxy "name"
_email = SProxy :: SProxy "email"
_city = SProxy :: SProxy "city"
_state = SProxy :: SProxy "state"

-- | mkFormSpecFromRow can produce a valid input form spec from your row
-- | without you having to type anything.
formSpec :: Form FormSpec
formSpec = mkFormSpecFromRow $ RProxy :: RProxy (FormRow InputType)

-- | You should provide your own validation. This example uses the composable
-- | validation toolkit `purescript-polyform`
validator :: ∀ m. MonadEffect m => Form InputField -> m (Form InputField)
validator = applyOnInputFields
  { name: Name <$> (minLength 5 *> maxLength 10)
  , email: emailFormat >>> emailIsUsed
  , city: minLength 0
  , state: Validation.hoistFnV pure
  }

-- | You should provide a function from the form with only output values to your ideal
-- | parsed type.
submitter :: ∀ m. Monad m => Form OutputField -> m User
submitter = pure <<< unwrapOutput

----------
-- Validation via Polyform

type Errs r = Array (Variant r)
type EFormat r = (emailFormat :: String | r)
type EUsed r = (emailIsUsed :: Email | r)
type Min r = (minLength :: Tuple Int String | r)
type Max r = (maxLength :: Tuple Int String | r)

-- | This is a pure validator, so we'll leave `m` polymorphic. It parses to a different
-- | output type than its input: an email
emailFormat
  :: ∀ m r
   . Monad m
  => Validation m (Array (Variant (emailFormat :: String | r))) String Email
emailFormat = Validation.hoistFnV \e →
  if contains (Pattern "@") e
    then pure (Email e)
    else Invalid [inj (SProxy ∷ SProxy "emailFormat") e]


-- | This is an effectful validator, here querying the database to see if the email
-- | is in use (faked with `random`). It expects that the email being validated is
-- | already valid.
emailIsUsed
  :: ∀ r m
   . MonadEffect m
  => Validation m (Array (Variant (emailIsUsed :: Email | r))) Email Email
emailIsUsed = Validation \e -> do
  v <- liftEffect random
  pure $ if v > 0.5
    then Invalid [inj (SProxy :: SProxy "emailIsUsed") e]
    else pure e

-- | Another pure validator, this time accepting an additional integer argument to
-- | define the minimum length of the string.
minLength
  :: ∀ m r
   . Monad m
  => Int
  -> Validation m (Array (Variant (minLength :: Tuple Int String | r))) String String
minLength n = Validation.hoistFnV \p ->
  if length p < n
    then Invalid [inj (SProxy :: SProxy "minLength") (Tuple n p)]
    else pure p

-- | The opposite of minLength.
maxLength
  :: ∀ m r
   . Monad m
  => Int
  -> Validation m (Array (Variant (maxLength :: Tuple Int String | r))) String String
maxLength n = Validation.hoistFnV \p ->
  if length p > n
    then Invalid [inj (SProxy :: SProxy "maxLength") (Tuple n p)]
    else pure p
