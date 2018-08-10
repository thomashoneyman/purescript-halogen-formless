module Example.RealWorld.Spec.OptionsForm where

import Prelude

import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Example.App.Validation as V
import Example.RealWorld.Data.Options (Dollars(..), Metric(..), OptionsForm(..), prx)
import Formless as F

optionsFormInputs :: OptionsForm Record F.InputField
optionsFormInputs = F.mkInputFields $ F.FormProxy :: F.FormProxy OptionsForm

-- In the case the user has not toggled the options on, we'll provide them with
-- valid default values.
defaultInputs :: OptionsForm Record F.InputField
defaultInputs = OptionsForm $ inputs
  { metric = F.InputField $ Just ViewCost
  , viewCost = F.InputField "1"
  , size = F.InputField "21"
  , dimensions = F.InputField "3005"
  }
  where
    inputs = unwrap optionsFormInputs


optionsFormValidators :: ∀ m. Monad m => F.PublicState OptionsForm m -> OptionsForm Record (F.Validation m)
optionsFormValidators { form } = OptionsForm
  { enable: F.hoistFn identity
  , metric: V.exists
  , viewCost: validateMetric ViewCost
  , clickCost: validateMetric ClickCost
  , installCost: validateMetric InstallCost
  , size: Int.toNumber <$> V.strIsInt
  , dimensions: Int.toNumber <$> V.strIsInt
  , speed: F.hoistFn identity
  }
  where
    metric = F.getInput prx.metric form

    validateMetric m =
      if metric == Just m
        then pure <<< Dollars <$> V.strIsInt
        else F.hoistFn (const Nothing)

