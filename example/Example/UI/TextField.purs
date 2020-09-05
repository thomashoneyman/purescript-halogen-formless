module Example.UI.TextField where

import Prelude

import Data.Either (Either, hush)
import Data.Foldable (for_)
import Data.Lens (_Left, preview)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Halogen (RefLabel(..), liftEffect)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.Hooks (HookM, getHTMLElementRef)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Formless (FormInput(..), WithInput)
import Web.HTML.HTMLElement as HTMLElement

type UseTextField a =
  Hooks.UseState Boolean
    Hooks.<> Hooks.UseMemo (Either String a)
    Hooks.<> Hooks.Pure

type TextFieldInput a =
  { validate :: String -> Either String a
  , disabled :: Boolean
  }

type TextFieldInterface slots m =
  ( error :: Maybe String
  , focus :: HookM m Unit
  | WithInput slots m ()
  )

textField
  :: forall slots m a
   . MonadEffect m
  => TextFieldInput a
  -> FormInput m (UseTextField a) (TextFieldInterface slots m) String a
textField { validate, disabled } = FormInput \form -> Hooks.do
  let
    currentValue = fromMaybe "" form.value
    refLabel = RefLabel "TextField"

  touched /\ setTouched <- map (map Hooks.put) $ Hooks.useState false
  isValid <- useValidate currentValue

  Hooks.pure
    { input:
        HH.input
          [ HP.type_ InputText
          , HP.value currentValue
          , HP.disabled disabled
          , HP.ref refLabel
          , HE.onValueInput \val -> Just do
              setTouched true
              form.onChange val
          ]
    , error:
        if touched then
          preview _Left isValid
        else
          Nothing
    , focus: do
        mbElem <- getHTMLElementRef refLabel
        for_ mbElem (liftEffect <<< HTMLElement.focus)
    , value: hush isValid
    }
  where
  useValidate value =
    Hooks.captures { value } Hooks.useMemo \_ ->
      validate value
