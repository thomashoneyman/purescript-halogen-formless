module Example.Field.Basic where

import Prelude

import Data.Either (Either, hush)
import Data.Lens (_Left, preview)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.Hooks (class HookEquals, class HookNewtype, HookM, kind HookType)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Formless (FormField(..))
import Type.Proxy (Proxy2(..))

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

basicField'
  :: forall m a
   . BasicFieldInput a
  -> FormField m (UseBasicField a) (BasicFieldInterface m) String a
basicField' = basicField (Proxy2 :: Proxy2 m)

basicField
  :: forall m a
   . Proxy2 m
  -> BasicFieldInput a
  -> FormField m (UseBasicField a) (BasicFieldInterface m) String a
basicField _ { initialValue, validate } = FormField \field -> Hooks.wrap Hooks.do
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
    , error: if field.touched then preview _Left isValid else Nothing
    , value: hush isValid
    }
  where
  useValidate value =
    Hooks.captures { value } Hooks.useMemo \_ ->
      validate value
