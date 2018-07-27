module Example.Polyform.Spec where

import Prelude

import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Example.Utils as V
import Formless.Spec (FormSpec, InputType, InputField, OutputType, OutputField)
import Formless.Spec.Transform (mkFormSpecFromRow, unwrapOutput)
import Formless.Validation.Polyform (applyOnInputFields)
import Polyform.Validation as Validation
import Type.Row (RProxy(..))

type User = Record (FormRow OutputType)

newtype Form f = Form (Record (FormRow f))
derive instance newtypeForm :: Newtype (Form f) _

type FormRow f =
  ( name  :: f V.Errs String V.Name
  , email :: f V.Errs String V.Email
  , city  :: f V.Errs String String
  , state :: f V.Errs String String
  )

_name = SProxy :: SProxy "name"
_email = SProxy :: SProxy "email"
_city = SProxy :: SProxy "city"
_state = SProxy :: SProxy "state"

formSpec :: Form FormSpec
formSpec = mkFormSpecFromRow $ RProxy :: RProxy (FormRow InputType)

validator :: ∀ m. MonadEffect m => Form InputField -> m (Form InputField)
validator = applyOnInputFields
  { name: V.Name <$> (V.minLength 5 *> V.maxLength 10)
  , email: V.emailFormat >>> V.emailIsUsed
  , city: V.minLength 0
  , state: Validation.hoistFnV pure
  }

submitter :: ∀ m. Monad m => Form OutputField -> m User
submitter = pure <<< unwrapOutput
