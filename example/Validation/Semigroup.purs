-- | Credit: Joe Kachmar's 2018 presentation at LambdaConf:
-- | https://github.com/jkachmar/lc2018
-- |
-- | Code here is largely untouched from the section 'Exercise 4'

module Example.Validation.Semigroup where

import Prelude

import Data.Array (fromFoldable)
import Data.Bifunctor (bimap)
import Data.Either (fromRight)
import Data.Foldable (class Foldable)
import Data.Foldable (length) as Foldable
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromString) as Int
import Data.List.NonEmpty (NonEmptyList, singleton)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String (null, length, toLower, toUpper)
import Data.String.Regex (Regex, regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.Validation.Semigroup (V, invalid, unV)
import Global.Unsafe (unsafeStringify)
import Partial.Unsafe (unsafePartial)

-- | A type synonym for purescript-validation semigroup errors
type Errs = NonEmptyList InvalidPrimitive

-- | A custom validator that verifies a value is not Nothing
validateMaybe :: ∀ a. Maybe a -> V Errs a
validateMaybe Nothing = invalid (singleton EmptyField)
validateMaybe (Just a) = pure a

-- | A custom validator that verifies a value is not Nothing
validateEqual :: String -> String -> V Errs String
validateEqual a b
  | a == b = pure b
  | otherwise = invalid (singleton $ NotEqual a b)

validateInt :: String -> V Errs Int
validateInt str = case Int.fromString str of
  Nothing -> invalid (singleton $ InvalidInt str)
  Just v -> pure v

-- | Validate that an input string is not empty
validateNonEmptyF
  :: ∀ f a
   . Foldable f
  => f a
  -> V (NonEmptyList InvalidPrimitive) (f a)
validateNonEmptyF input
  | Foldable.length input >= 1 = pure input
  | otherwise = invalid (singleton EmptyField)


--------------------------------------------------------------------------------
-- TYPES AND FUNCTIONS FOR FIELD VALIDATIONS

-- | Newtype wrapper for `String` indicating a valid email address
newtype EmailAddress = EmailAddress String

-- | Newtype wrapper for `String` indicating a valid password
newtype Password = Password String

-- | Type of validation errors encountered when validating form fields
data InvalidField
  = InvalidEmailAddress (NonEmptyList InvalidPrimitive)
  | InvalidPassword     (NonEmptyList InvalidPrimitive)

validateEmailAddress
  :: String
  -> V (NonEmptyList InvalidField) EmailAddress
validateEmailAddress input =
      let result =
               validateNonEmpty input
            *> validateEmailRegex input
      in bimap (singleton <<< InvalidEmailAddress) EmailAddress result

validatePassword
  :: String
  -> Int
  -> Int
  -> V (NonEmptyList InvalidField) Password
validatePassword input minLength maxLength =
     let result =
              validateNonEmpty input
           *> validateContainsMixedCase input
           *> (validateLength input minLength maxLength)
     in bimap (singleton <<< InvalidPassword) Password result

--------------------------------------------------------------------------------
-- TYPES AND FUNCTIONS FOR PRIMITIVE VALIDATIONS

-- | Type of validation errors encountered when validating primitive input
data InvalidPrimitive
  = EmptyField
  | InvalidEmail String
  | TooShort Int Int
  | TooLong Int Int
  | NoLowercase String
  | NoUppercase String
  | InvalidInt String
  | NotEqual String String

-- | Validate that an input string is not empty
validateNonEmpty :: String -> V (NonEmptyList InvalidPrimitive) String
validateNonEmpty input
  | null input = invalid (singleton EmptyField)
  | otherwise = pure input

-- | Validates that an input string conforms to some regular expression that
-- | checks for valid email addresses
validateEmailRegex :: String -> V (NonEmptyList InvalidPrimitive) String
validateEmailRegex input
  | test emailRegex input = pure input
  | otherwise = invalid (singleton (InvalidEmail input))

-- | Validate that an input string is at least as long as some given `Int`
validateMinimumLength
  :: String
  -> Int
  -> V (NonEmptyList InvalidPrimitive) String
validateMinimumLength input minLength
  | (length input) < minLength = invalid (singleton (TooShort (length input) minLength))
  | otherwise = pure input

-- | Validate that an input string is shorter than given `Int`
validateMaximumLength
  :: String
  -> Int
  -> V (NonEmptyList InvalidPrimitive) String
validateMaximumLength input maxLength
  | (length input) > maxLength = invalid (singleton (TooLong (length input) maxLength))
  | otherwise = pure input

-- | Validate that an input string is within the minimum and maximum given `Int`
-- | lengths
validateLength
  :: String
  -> Int
  -> Int
  -> V (NonEmptyList InvalidPrimitive) String
validateLength input minLength maxLength =
     validateMinimumLength input minLength
  *> validateMaximumLength input maxLength

-- | Validate that an input string contains at least one lowercase character
validateContainsLowercase
  :: String
  -> V (NonEmptyList InvalidPrimitive) String
validateContainsLowercase input
  | (toUpper input) == input = invalid (singleton (NoLowercase input))
  | otherwise = pure input

-- | Validate that an input string contains at least one uppercase character
validateContainsUppercase
  :: String
  -> V (NonEmptyList InvalidPrimitive) String
validateContainsUppercase input
  | (toLower input) == input = invalid (singleton (NoUppercase input))
  | otherwise = pure input

-- | Validate that an input string contains some mix of upper- and lowercase
-- | characters
validateContainsMixedCase
  :: String
  -> V (NonEmptyList InvalidPrimitive) String
validateContainsMixedCase input =
     validateContainsLowercase input
  *> validateContainsUppercase input

---------------------------------------------------------------------------------
-- !!! BOILERPLATE TO MAKE EVERYTHING A LITTLE EASIER TO WORK WITH            !!!
---------------------------------------------------------------------------------

-- | Helper function to print validations
printValidation
  :: forall err result
   . Show err
  => V (NonEmptyList err) result
  -> String
printValidation =
  unV (show <<< fromFoldable) (\result -> "Valid: " <> (unsafeStringify result))

-- | Derive a `Generic` instance for `InvalidPrimitive` so we can get a `Show`
-- | instance to print to the console.
derive instance genericInvalidPrimitive :: Generic InvalidPrimitive _

-- | Derive `show` for `InvalidPrimitive` using the `Generic` instance.
instance showInvalidPrimitive :: Show InvalidPrimitive where
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

-- | Manually derive a `Show` instance for `EmailAddress` so it prints nicely
derive instance newtypeEmailAddress :: Newtype EmailAddress _
instance showEmailAddress :: Show EmailAddress where show = unwrap

-- | Manually derive a `Show` instance for `Password` so it prints nicely
derive instance newtypePassword :: Newtype Password _
instance showPassword :: Show Password where show = unwrap

-- | Manually derive a `Show` instance for `InvalidField` that pretty prints the
-- | `NonEmptyList`s as `Array`s
instance showInvalidField :: Show InvalidField where
  show = case _ of
    InvalidEmailAddress errs -> "(InvalidEmailAddress " <> (show (fromFoldable errs)) <> ")"
    InvalidPassword     errs -> "(InvalidPassword "     <> (show (fromFoldable errs)) <> ")"
