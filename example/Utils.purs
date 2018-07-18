module Example.Utils where

import Prelude

import Data.Either (Either, either, fromRight)
import Data.Foldable (length) as Foldable
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromString) as Int
import Data.List.NonEmpty (NonEmptyList, singleton)
import Data.Maybe (Maybe(..))
import Data.String (length, null)
import Data.String.Regex (Regex, regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.Validation.Semigroup (V, invalid)
import Partial.Unsafe (unsafePartial)


-- | Unpacks errors to render as a string
showError
  :: ∀ e o r
   . Show e
  => { result :: Maybe (Either e o) | r }
  -> Maybe String
showError = join <<< map (either (Just <<< show) (const Nothing)) <<< _.result


--------------------
-- Semigroup Validation
--------------------

-- | A type synonym for purescript-validation semigroup errors
type Errs = NonEmptyList FieldError

data FieldError
  = EmptyField
  | InvalidEmail String
  | TooShort Int Int
  | TooLong Int Int
  | InvalidInt String
  | NotEqual String String


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
  -> V (NonEmptyList FieldError) (Array String)
validateNonEmptyArray input
  | Foldable.length input >= 1 = pure input
  | otherwise = invalid (singleton EmptyField)

-- | Validate that an input string is not empty
validateNonEmpty :: String -> V (NonEmptyList FieldError) String
validateNonEmpty input
  | null input = invalid (singleton EmptyField)
  | otherwise = pure input

-- | Validates that an input string conforms to some regular expression that
-- | checks for valid email addresses
validateEmailRegex :: String -> V (NonEmptyList FieldError) String
validateEmailRegex input
  | test emailRegex input = pure input
  | otherwise = invalid (singleton (InvalidEmail input))

-- | Validate that an input string is at least as long as some given `Int`
validateMinimumLength
  :: String
  -> Int
  -> V (NonEmptyList FieldError) String
validateMinimumLength input minLength
  | (length input) < minLength = invalid (singleton (TooShort (length input) minLength))
  | otherwise = pure input

-- | Validate that an input string is shorter than given `Int`
validateMaximumLength
  :: String
  -> Int
  -> V (NonEmptyList FieldError) String
validateMaximumLength input maxLength
  | (length input) > maxLength = invalid (singleton (TooLong (length input) maxLength))
  | otherwise = pure input

-- | Derive a `Generic` instance for `FieldError` so we can get a `Show`
-- | instance to print to the console.
derive instance genericFieldError :: Generic FieldError _

-- | Derive `show` for `FieldError` using the `Generic` instance.
instance showFieldError :: Show FieldError where
  show = genericShow

-- | Utility function to unsafely construct a regular expression from a pattern
-- | string.
-- |
-- | This will fail at runtime with an error if the pattern string is invalid.
unsafeRegexFromString :: String -> Regex
unsafeRegexFromString str = unsafePartial (fromRight (regex str noFlags))

-- | Regular expression for email address validation.
emailRegex :: Regex
emailRegex =
  unsafeRegexFromString "^\\w+([.-]?\\w+)*@\\w+([.-]?\\w+)*(\\.\\w{2,3})+$"
