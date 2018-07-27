module Example.RealWorld.Spec.OptionsForm where

import Prelude

import Data.Int as Int
import Data.Maybe (Maybe(..))
import Example.RealWorld.Data.Options (Dollars(..), Metric(..), OptionsForm(..), OptionsRow, _metric)
import Example.Utils as V
import Formless as F
import Formless.Validation.Semigroup (onInputField)
import Type.Row (RProxy(..))

optionsFormSpec :: OptionsForm F.FormSpec
optionsFormSpec = F.mkFormSpecFromRow $ RProxy :: RProxy (OptionsRow F.InputType)

optionsFormValidate
  :: OptionsForm F.InputField
  -> OptionsForm F.InputField
optionsFormValidate (OptionsForm form) = OptionsForm
  { enable: pure `onInputField` form.enable
  , metric: V.validateMaybe `onInputField` form.metric
  , viewCost: (validateMetric ViewCost) `onInputField` form.viewCost
  , clickCost: (validateMetric ClickCost) `onInputField` form.clickCost
  , installCost: (validateMetric InstallCost) `onInputField` form.installCost
  , size: validateInt `onInputField` form.size
  , dimensions: validateInt `onInputField` form.dimensions
  , speed: pure `onInputField` form.speed
  }
  where
    metric = F.getInput _metric (OptionsForm form)

    validateMetric m str
      | metric == Just m = pure <<< Dollars <$> V.validateInt str
      | otherwise = pure Nothing

    validateInt str = Int.toNumber <$> V.validateInt str
