module Example.App.Validation where

import Prelude

import Data.Either (Either(..), either)
import Data.Foldable (length) as Foldable
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.String (contains, length, null)
import Data.String.Pattern (Pattern(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (random)
import Formless.Validation (Validation(..), hoistFnE_)

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
showError :: ∀ e o. ToText e => Maybe (Either e o) -> Maybe String
showError = (=<<) (either (pure <<< toText) (const Nothing))

class ToText item where
  toText :: item -> String

instance toTextString :: ToText String where
  toText = identity

--------------------
-- Formless Validation
--------------------

emailFormat :: ∀ form m. Monad m => Validation form m FieldError String Email
emailFormat = hoistFnE_ $ \str ->
  if contains (Pattern "@") str
    then pure $ Email str
    else Left InvalidEmail

emailIsUsed :: ∀ form m. MonadEffect m => Validation form m FieldError Email Email
emailIsUsed = Validation \_ e@(Email e') -> do
  -- Perhaps we hit the server to  if the email is in use
  _ <- liftEffect random
  pure $ if (contains (Pattern "t") e')
    then pure e
    else Left EmailInUse

minLength :: ∀ form m. Monad m => Int -> Validation form m FieldError String String
minLength n = hoistFnE_ $ \str ->
  let n' = length str
   in if n' < n then Left (TooShort n) else Right str

-- | The opposite of minLength.
maxLength :: ∀ form m. Monad m => Int -> Validation form m FieldError String String
maxLength n = hoistFnE_ \str ->
  let n' = length str
   in if n' > n then Left (TooLong n) else Right str

exists :: ∀ form m a. Monad m => Validation form m FieldError (Maybe a) a
exists = hoistFnE_ $ maybe (Left EmptyField) Right

strIsInt :: ∀ form m. Monad m => Validation form m FieldError String Int
strIsInt = hoistFnE_ $ \str -> maybe (Left $ InvalidInt str) Right (Int.fromString str)

nonEmptyArray :: ∀ form m a. Monad m => Validation form m FieldError (Array a) (Array a)
nonEmptyArray = hoistFnE_ \arr ->
  if Foldable.length arr > 0
    then Right arr
    else Left EmptyField

-- | Validate that an input string is not empty
nonEmptyStr :: ∀ form m. Monad m => Validation form m FieldError String String
nonEmptyStr = hoistFnE_ $ \str ->
  if null str
    then Left EmptyField
    else Right str
