module Example.RealWorld.Spec.OptionsForm where

import Prelude

import Data.Int as Int
import Data.Maybe (Maybe(..))
import Example.App.Validation as V
import Example.RealWorld.Data.Options (Dollars(..), Metric(..), OptionsForm(..), Speed(..), proxies)
import Formless as F
import Formless.Validation.Semigroup (onInputField)

optionsFormSpec :: OptionsForm Record F.FormSpec
optionsFormSpec = F.mkFormSpecFromProxy $ F.FormProxy :: F.FormProxy OptionsForm

-- In the case the user has not toggled the options on, we'll provide them with
-- valid default values
defaultOptionsSpec :: OptionsForm Record F.FormSpec
defaultOptionsSpec = F.mkFormSpec
  { enable: false
  , metric: Just ViewCost
  , viewCost: "1"
  , clickCost: ""
  , installCost: ""
  , size: "1"
  , dimensions: "1"
  , speed: Medium
  }

optionsFormValidate
  :: OptionsForm Record F.InputField
  -> OptionsForm Record F.InputField
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
    metric = F.getInput proxies.metric (OptionsForm form)

    validateMetric m str
      | metric == Just m = pure <<< Dollars <$> V.validateInt str
      | otherwise = pure Nothing

    validateInt str = Int.toNumber <$> V.validateInt str
