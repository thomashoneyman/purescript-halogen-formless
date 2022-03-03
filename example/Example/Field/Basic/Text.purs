module Example.Field.Basic.Text where

import Prelude

import Data.Either (Either, hush)
import Data.Lens (_Left, preview)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.Hooks (class HookNewtype, HookM, HookType)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Formless (FormField(..))
import Type.Proxy (Proxy)

foreign import data UseBasicTextField :: Type -> HookType

type UseBasicTextField' a =
  Hooks.UseMemo (Either String a)
    Hooks.<> Hooks.Pure

instance newtypeUseBasicTextField
  :: HookNewtype (UseBasicTextField a) (UseBasicTextField' a)

type BasicTextFieldInput a =
  { validate :: String -> Either String a
  }

type BasicTextFieldInterface m = ( input :: H.ComponentHTML (HookM m Unit) () m )

-- | This basic text field accepts an initial value and a validation function
-- | and it returns a possible error value and an input you can render. You can
-- | use this field if you specify your form type. If you want the compiler to
-- | infer your form type, then use `basicField` instead.
-- |
-- | It demonstrates a simple example building block you may wish to use in forms
-- | in your application.
textField
  :: forall m a
   . Proxy m
  -> BasicTextFieldInput a
  -> FormField m (UseBasicTextField a) (BasicTextFieldInterface m) String a
textField proxy { validate } = FormField proxy \field -> Hooks.wrap Hooks.do
  let
    currentValue :: String
    currentValue = fromMaybe "" field.value

  isValid <- Hooks.captures { currentValue } Hooks.useMemo \_ -> validate currentValue

  let
    input :: HH.HTML _ (HookM m Unit)
    input =
      HH.div
        [ ]
        [ HH.input
            [ HP.type_ InputText
            , HP.value currentValue
            , HE.onValueInput field.onChange
            ]
        , case field.value of
            Nothing -> HH.text ""
            Just _ -> maybe (HH.text "") HH.text (preview _Left isValid)
        ]

  Hooks.pure { input, value: hush isValid }
