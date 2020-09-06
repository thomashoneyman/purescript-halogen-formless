module Example.Input.Basic where

import Prelude

import Data.Either (Either, hush)
import Data.Foldable (for_)
import Data.Lens (_Left, preview)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Halogen (RefLabel, liftEffect)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.Hooks (class HookEquals, class HookNewtype, HookM, getHTMLElementRef, kind HookType)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Formless (FormInput(..), WithInput)
import Type.Proxy (Proxy2)
import Web.HTML.HTMLElement as HTMLElement

foreign import data UseBasicInput :: Type -> HookType

type UseBasicInput' a =
  Hooks.UseMemo (Either String a)
    Hooks.<> Hooks.UseState Boolean
    Hooks.<> Hooks.Pure

instance newtypeUseBasicInput
  :: HookEquals (UseBasicInput' a) h
  => HookNewtype (UseBasicInput a) h

type BasicInput a =
  { validate :: String -> Either String a
  , ref :: RefLabel
  , disabled :: Boolean
  }

type BasicInputInterface m =
  ( error :: Maybe String
  , focus :: HookM m Unit
  | WithInput () m ()
  )

basicInput
  :: forall m a
   . MonadEffect m
  => Proxy2 m
  -> BasicInput a
  -> FormInput m (UseBasicInput' a) (BasicInputInterface m) String a
basicInput _ { validate, disabled, ref } = FormInput \field -> Hooks.do
  let
    currentValue :: String
    currentValue = fromMaybe "" field.value

  isValid <- useValidate currentValue
  touched /\ setTouched <- map (map Hooks.put) $ Hooks.useState false

  Hooks.pure
    { input:
        HH.input
          [ HP.type_ InputText
          , HP.value currentValue
          , HP.disabled disabled
          , HP.ref ref
          , HE.onValueInput \val -> Just do
              setTouched true
              field.onChange val
          ]
    , error:
        if touched then
          preview _Left isValid
        else
          Nothing
    , focus: do
        mbElem <- getHTMLElementRef ref
        for_ mbElem (liftEffect <<< HTMLElement.focus)
    , value:
        hush isValid
    }
  where
  useValidate value =
    Hooks.captures { value } Hooks.useMemo \_ ->
      validate value
