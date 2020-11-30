module Example.Field.Basic where

import Prelude

import Data.Either (Either, hush)
import Data.Lens (_Left, preview)
import Data.Maybe (Maybe(..), isJust)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.Hooks (class HookEquals, class HookNewtype, HookM, kind HookType)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Formless (FormField(..))
import Type.Proxy (Proxy2)

foreign import data UseBasicField :: Type -> HookType

type UseBasicField' a =
  Hooks.UseMemo (Either String a)
    Hooks.<> Hooks.Pure

instance newtypeUseBasicField
  :: HookEquals (UseBasicField' a) h
  => HookNewtype (UseBasicField a) h

type BasicFieldInput a =
  { validate :: String -> Either String a
  , initialValue :: Maybe String
  }

type BasicFieldInterface m =
  ( error :: Maybe String
  , input :: H.ComponentHTML (HookM m Unit) () m
  )

-- | This basic text field can be used when the form type is being inferred by
-- | the compiler. The proxy helps the compiler prove that all of the `m` used
-- | in your form are the same.
basicField
  :: forall m a
   . Proxy2 m
  -> BasicFieldInput a
  -> FormField m (UseBasicField a) (BasicFieldInterface m) String a
basicField _ = basicField'

-- | This basic text field accepts an initial value and a validation function
-- | and it returns a possible error value and an input you can render. You can
-- | use this field if you specify your form type. If you want the compiler to
-- | infer your form type, then use `basicField` instead.
-- |
-- | It demonstrates a simple example building block you may wish to use in forms
-- | in your application.
basicField'
  :: forall m a
   . BasicFieldInput a
  -> FormField m (UseBasicField a) (BasicFieldInterface m) String a
basicField' { initialValue, validate } = FormField \field -> Hooks.wrap Hooks.do
  let
    currentValue :: String
    currentValue
      | Just value <- field.value = value
      | Just value <- initialValue = value
      | otherwise = ""

    input :: HH.HTML _ (HookM m Unit)
    input =
      HH.input
        [ HP.type_ InputText
        , HP.value currentValue
        , HE.onValueInput (Just <<< field.onChange)
        ]

  isValid <- useValidate currentValue

  Hooks.pure
    { input
    , error: if isJust field.value then preview _Left isValid else Nothing
    , value: hush isValid
    }
  where
  useValidate value =
    Hooks.captures { value } Hooks.useMemo \_ ->
      validate value
