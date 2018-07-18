module Example.RealWorld.Spec.OptionsForm where

import Prelude

import Data.Int (toNumber) as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Example.RealWorld.Data.Options
  (Dollars(..), Metric(..), OptionsForm(..), OptionsRow)
import Example.Validation.Semigroup as V
import Formless.Spec as FSpec
import Formless.Validation.Semigroup (onInputField)
import Type.Row (RProxy(..))

-- | mkFormSpecFromRow can produce a valid input form spec from your row
-- | without you having to type anything.
optionsFormSpec :: OptionsForm FSpec.FormSpec
optionsFormSpec =
  FSpec.mkFormSpecFromRow $ RProxy :: RProxy (OptionsRow FSpec.Input)

-- | You should provide your own validation. This example uses the PureScript
-- | standard, `purescript-validation`.
optionsFormValidate
  :: OptionsForm FSpec.InputField
  -> OptionsForm FSpec.InputField
optionsFormValidate (OptionsForm form) =
  case (_.input $ unwrap form.enable) of
    false -> OptionsForm form
    true -> OptionsForm
      { enable: pure `onInputField` form.enable
      , metric: V.validateMaybe `onInputField` form.metric
      , viewCost: (\str ->
          case (_.input $ unwrap form.metric) of
            Just ViewCost -> (pure <<< Dollars) <$> (V.validateInt str)
            _ -> pure Nothing
          ) `onInputField` form.viewCost
      , clickCost: (\str ->
          case (_.input $ unwrap form.metric) of
            Just ClickCost -> (pure <<< Dollars) <$> (V.validateInt str)
            _ -> pure Nothing
          ) `onInputField` form.clickCost
      , installCost: (\str ->
          case (_.input $ unwrap form.metric) of
            Just InstallCost -> (pure <<< Dollars) <$> (V.validateInt str)
            _ -> pure Nothing
          ) `onInputField` form.installCost
      , size: (\str -> Int.toNumber <$> V.validateInt str)
          `onInputField` form.size
      , dimensions: (\str -> Int.toNumber <$> V.validateInt str)
          `onInputField` form.dimensions
      , speed: pure `onInputField` form.speed
      }
