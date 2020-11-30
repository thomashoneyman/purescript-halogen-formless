module Example.Input.Basic where

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
import Type.Proxy (Proxy2)

foreign import data UseBasicInput :: Type -> HookType

type UseBasicInput' a =
  Hooks.UseMemo (Either String a)
    Hooks.<> Hooks.Pure

instance newtypeUseBasicInput
  :: HookEquals (UseBasicInput' a) h
  => HookNewtype (UseBasicInput a) h

type BasicInput m a =
  { validate :: String -> Either String a
  , initialValue :: Maybe String
  , proxy :: Proxy2 m
  }

type BasicInputInterface m =
  ( error :: Maybe String
  , input :: H.ComponentHTML (HookM m Unit) () m
  )

basicInput
  :: forall m a
   . BasicInput m a
  -> FormField m (UseBasicInput a) (BasicInputInterface m) String a
basicInput { initialValue, validate } = FormField \field -> Hooks.wrap Hooks.do
  let
    currentValue :: String
    currentValue
      | Just value <- field.value = value
      | Just value <- initialValue = value
      | otherwise = ""

    input :: H.ComponentHTML (HookM m Unit) () m
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
