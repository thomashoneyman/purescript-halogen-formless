module Example.App.Validation where

import Prelude

import Data.Array (head, singleton)
import Data.Either (Either(..), either, fromRight)
import Data.Foldable (length) as Foldable
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (contains, length, null)
import Data.String.Pattern (Pattern(..))
import Data.String.Regex (Regex, regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.Validation.Semigroup (V, invalid)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (random)
import Formless as F
import Partial.Unsafe (unsafePartial)
import Polyform.Validation (Validation(..))
import Polyform.Validation as Validation

-- | A type synonym for purescript-validation semigroup errors
type Errs = Array FieldError

data FieldError
  = EmptyField
  | InvalidEmail
  | EmailInUse
  | TooShort Int
  | TooLong Int
  | InvalidInt String
  | NotEqual String String

derive instance genericFieldError :: Generic FieldError _
instance showFieldError :: Show FieldError where
  show = genericShow

instance toTextFieldError :: ToText FieldError where
  toText EmptyField = "This field is required."
  toText InvalidEmail = "That email is not valid."
  toText EmailInUse = "That email is already being used."
  toText (TooShort n) = "You must enter at least " <> show n <> " characters."
  toText (TooLong n) = "You must enter less than " <> show n <> " characters."
  toText (InvalidInt str) = "Could not parse \"" <> str <> "\" to a valid integer."
  toText (NotEqual str0 str1) = "This field contains \"" <> str1 <> "\" but must be equal to \"" <> str0 <> "\" to validate."

-- | Some useful types we'll parse to
newtype Name = Name String
derive instance newtypeName :: Newtype Name _
derive newtype instance showName :: Show Name

newtype Email = Email String
derive instance newtypeEmail :: Newtype Email _
derive newtype instance showEmail :: Show Email

-- | Unpacks errors to render as a string
showError
  :: ∀ e o
   . ToText e
  => Maybe (Either (Array e) o)
  -> Maybe String
showError = (=<<) (either (map toText <<< head) (const Nothing))

class ToText item where
  toText :: item -> String

instance toTextString :: ToText String where
  toText = identity

--------------------
-- Formless Validation
--------------------

checkNotRequired
  :: ∀ m e i
   . Monad m
  => Semigroup e
  => F.Validator m e i i
checkNotRequired = F.hoistFnE pure

checkEmailFormat
  :: ∀ m
   . Monad m
  => F.Validator m Errs String Email
checkEmailFormat = F.hoistFnE \i ->
  if contains (Pattern "@") i
    then pure (Email i)
    else Left $ singleton InvalidEmail

checkEmailIsUsed
  :: ∀ m
   . MonadEffect m
  => F.Validator m Errs Email Email
checkEmailIsUsed = F.Validator \(Email e) -> do
  -- Perhaps we hit the server to check if the email is in use
  n <- liftEffect random
  pure $ if n > 0.5
    then Left $ singleton EmailInUse
    else pure (Email e)

checkMinLength
  :: ∀ m
   . Monad m
  => Int
  -> F.Validator m Errs String String
checkMinLength n = F.hoistFnE \p ->
  let p' = length p
  in if p' < n
       then Left $ singleton $ TooShort n
       else pure p

-- | The opposite of minLength.
checkMaxLength
  :: ∀ m
   . Monad m
  => Int
  -> F.Validator m Errs String String
checkMaxLength n = F.hoistFnE \p ->
  let p' = length p
   in if length p > n
        then Left $ singleton $ TooLong n
        else pure p


--------------------
-- Polyform Validation
--------------------

notRequired
  :: ∀ m a t0
   . Monad m
  => Monoid t0
  => Validation m t0 a a
notRequired = Validation.hoistFnV pure

emailFormat
  :: ∀ m
   . Monad m
  => Validation m Errs String Email
emailFormat = Validation.hoistFnV \e ->
  if contains (Pattern "@") e
    then pure (Email e)
    else Validation.Invalid $ singleton InvalidEmail

emailIsUsed
  :: ∀ m
   . MonadEffect m
  => Validation m Errs Email Email
emailIsUsed = Validation \(Email e) -> do
  -- Perhaps we hit the server to check if the email is in use
  _ <- liftEffect random
  pure $ if contains (Pattern "t") e
    then Validation.Invalid $ singleton EmailInUse
    else pure (Email e)

minLength
  :: ∀ m
   . Monad m
  => Int
  -> Validation m Errs String String
minLength n = Validation.hoistFnV \p ->
  let p' = length p
  in if p' < n
       then Validation.Invalid $ singleton $ TooShort n
       else pure p

-- | The opposite of minLength.
maxLength
  :: ∀ m
   . Monad m
  => Int
  -> Validation m Errs String String
maxLength n = Validation.hoistFnV \p ->
  let p' = length p
   in if length p > n
        then Validation.Invalid $ singleton $ TooLong n
        else pure p


--------------------
-- Semigroup Validation
--------------------

validateMaybe :: ∀ a. Maybe a -> V Errs a
validateMaybe Nothing = invalid (singleton EmptyField)
validateMaybe (Just a) = pure a

validateEqual :: String -> String -> V Errs String
validateEqual a b
  | a == b = pure b
  | otherwise = invalid (singleton $ NotEqual a b)

validateInt :: String -> V Errs Int
validateInt str = case Int.fromString str of
  Nothing -> invalid (singleton $ InvalidInt str)
  Just v -> pure v

validateNonEmptyArray
  :: Array String
  -> V Errs (Array String)
validateNonEmptyArray input
  | Foldable.length input >= 1 = pure input
  | otherwise = invalid (singleton EmptyField)

-- | Validate that an input string is not empty
validateNonEmpty :: String -> V Errs String
validateNonEmpty input
  | null input = invalid (singleton EmptyField)
  | otherwise = pure input

-- | Validates that an input string conforms to some regular expression that
-- | checks for valid email addresses
validateEmailRegex :: String -> V Errs String
validateEmailRegex input
  | test emailRegex input = pure input
  | otherwise = invalid (singleton InvalidEmail)

-- | Validate that an input string is at least as long as some given `Int`
validateMinimumLength
  :: String
  -> Int
  -> V Errs String
validateMinimumLength input n
  | (length input) < n = invalid (singleton (TooShort n))
  | otherwise = pure input

-- | Validate that an input string is shorter than given `Int`
validateMaximumLength
  :: String
  -> Int
  -> V Errs String
validateMaximumLength input n
  | (length input) > n = invalid (singleton (TooLong n))
  | otherwise = pure input

unsafeRegexFromString :: String -> Regex
unsafeRegexFromString str = unsafePartial (fromRight (regex str noFlags))

emailRegex :: Regex
emailRegex =
  unsafeRegexFromString "^\\w+([.-]?\\w+)*@\\w+([.-]?\\w+)*(\\.\\w{2,3})+$"
