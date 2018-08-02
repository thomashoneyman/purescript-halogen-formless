module Example.RealWorld.Spec.OptionsForm where

import Prelude

import Data.Int as Int
import Data.Maybe (Maybe(..))
import Example.App.Validation as V
import Example.RealWorld.Data.Options (Dollars(..), Metric(..), OptionsForm(..), Speed(..), proxies)
import Formless as F
import Formless.Validation.Semigroup (onFormField)

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
  :: OptionsForm Record F.FormField
  -> OptionsForm Record F.FormField
optionsFormValidate (OptionsForm form) = OptionsForm
  { enable: pure `onFormField` form.enable
  , metric: V.validateMaybe `onFormField` form.metric
  , viewCost: (validateMetric ViewCost) `onFormField` form.viewCost
  , clickCost: (validateMetric ClickCost) `onFormField` form.clickCost
  , installCost: (validateMetric InstallCost) `onFormField` form.installCost
  , size: validateInt `onFormField` form.size
  , dimensions: validateInt `onFormField` form.dimensions
  , speed: pure `onFormField` form.speed
  }
  where
    metric = F.getInput proxies.metric (OptionsForm form)

    validateMetric m str
      | metric == Just m = pure <<< Dollars <$> V.validateInt str
      | otherwise = pure Nothing

    validateInt str = Int.toNumber <$> V.validateInt str
