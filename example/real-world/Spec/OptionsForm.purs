module Example.RealWorld.Spec.OptionsForm where

import Prelude

import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Example.App.Validation as V
import Example.RealWorld.Data.Options (Dollars(..), Metric(..), OptionsForm(..), prx)
import Formless as F
import Formless.Validation.Semigroup (toValidator)

optionsFormInputs :: OptionsForm Record F.InputField
optionsFormInputs = F.mkInputFields $ F.FormProxy :: F.FormProxy OptionsForm

-- In the case the user has not toggled the options on, we'll provide them with
-- valid default values.
defaultInputs :: OptionsForm Record F.InputField
defaultInputs = OptionsForm $ inputs
  { metric = F.InputField (Just ViewCost)
  , viewCost = F.InputField "1"
  , size = F.InputField "21"
  , dimensions = F.InputField "3005"
  }
  where
    inputs = unwrap optionsFormInputs


optionsFormValidators :: ∀ m. Monad m => F.PublicState OptionsForm m -> OptionsForm Record (F.Validator m)
optionsFormValidators { form } = OptionsForm
  { enable: toValidator pure
  , metric: toValidator V.validateMaybe
  , viewCost: toValidator $ validateMetric ViewCost
  , clickCost: toValidator $ validateMetric ClickCost
  , installCost: toValidator $ validateMetric InstallCost
  , size: toValidator validateInt
  , dimensions: toValidator validateInt
  , speed: toValidator pure
  }
  where
    metric = F.getInput prx.metric form
    validateInt str = Int.toNumber <$> V.validateInt str
    validateMetric m str
      | metric == Just m = pure <<< Dollars <$> V.validateInt str
      | otherwise = pure Nothing

